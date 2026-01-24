# ==============================================================================
# Assessment Data Tidying Functions
# ==============================================================================
#
# This file contains functions for transforming assessment data from wide
# format (with pct_below, pct_approaching, etc. columns) to long (tidy) format
# with a proficiency_level column.
#
# ==============================================================================


#' Tidy assessment data
#'
#' Transforms wide assessment data to long format with proficiency_level column.
#' The wide format has separate columns for each proficiency level (pct_below,
#' pct_approaching, pct_on_track, pct_mastered). The tidy format pivots these
#' into a single proficiency_level column with corresponding n_students and pct values.
#'
#' @param df A wide data.frame of processed assessment data
#' @return A long data.frame of tidied assessment data
#' @export
#' @examples
#' \dontrun{
#' wide_data <- fetch_assessment(2024, tidy = FALSE)
#' tidy_data <- tidy_assessment(wide_data)
#' }
tidy_assessment <- function(df) {

  if (nrow(df) == 0) {
    return(create_empty_tidy_assessment())
  }

  # Invariant columns (identifiers that stay the same)
  invariants <- c(
    "end_year", "type",
    "district_id", "district_name",
    "school_id", "school_name",
    "test", "subject", "grade", "subgroup",
    "n_tested"
  )
  invariants <- invariants[invariants %in% names(df)]

  # Proficiency level columns to pivot
  prof_cols <- c("pct_below", "pct_approaching", "pct_on_track", "pct_mastered")
  prof_cols <- prof_cols[prof_cols %in% names(df)]

  if (length(prof_cols) == 0) {
    # No proficiency columns to pivot - return as-is with flags
    return(id_assessment_aggs(df))
  }

  # Map column names to proficiency level names
  level_names <- c(
    "pct_below" = "below",
    "pct_approaching" = "approaching",
    "pct_on_track" = "on_track",
    "pct_mastered" = "mastered"
  )

  # Pivot proficiency levels to long format
  tidy_profs <- purrr::map_df(
    prof_cols,
    function(.x) {
      level_name <- level_names[.x]

      df_subset <- df |>
        dplyr::select(dplyr::all_of(c(invariants, .x))) |>
        dplyr::rename(pct = dplyr::all_of(.x)) |>
        dplyr::mutate(
          proficiency_level = level_name,
          # Calculate n_students from percentage and n_tested
          # pct values are typically 0-100, so divide by 100
          n_students = dplyr::case_when(
            !is.na(pct) & !is.na(n_tested) & n_tested > 0 ~ round(pct / 100 * n_tested),
            TRUE ~ NA_real_
          ),
          # Normalize pct to 0-1 range
          pct = dplyr::case_when(
            !is.na(pct) ~ pmin(pct / 100, 1.0),
            TRUE ~ NA_real_
          )
        )

      df_subset
    }
  )

  # Reorder columns and add aggregation flags
  result <- tidy_profs |>
    dplyr::select(
      dplyr::all_of(invariants),
      proficiency_level,
      n_students,
      pct
    ) |>
    id_assessment_aggs()

  result
}


#' Identify assessment aggregation levels
#'
#' Adds boolean flags to identify state, district, and school level records.
#'
#' @param df Assessment dataframe, output of tidy_assessment or process_assessment
#' @return data.frame with boolean aggregation flags
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_assessment(2024)
#' # Data already has aggregation flags via id_assessment_aggs
#' table(tidy_data$is_state, tidy_data$is_district, tidy_data$is_school)
#' }
id_assessment_aggs <- function(df) {
  df |>
    dplyr::mutate(
      # State level: Type == "State"
      is_state = type == "State",

      # District level: Type == "District"
      is_district = type == "District",

      # School level: Type == "School"
      is_school = type == "School"
    )
}


#' Calculate proficiency summary statistics
#'
#' Creates a summary table with proficiency rates aggregated by specified grouping.
#'
#' @param df Tidy assessment data frame
#' @param ... Grouping variables (unquoted)
#' @return Summary data frame with proficiency statistics
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_assessment(2024)
#' # Proficiency by subject and grade
#' assessment_summary(tidy_data, subject, grade)
#'
#' # Proficiency by subgroup
#' assessment_summary(tidy_data, subgroup)
#' }
assessment_summary <- function(df, ...) {

  # Check if tidy format (has proficiency_level column)
  if (!"proficiency_level" %in% names(df)) {
    stop("Input must be tidy assessment data (use tidy = TRUE)")
  }

  df |>
    dplyr::group_by(..., proficiency_level) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      n_tested = sum(n_tested, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct = dplyr::case_when(
        n_tested > 0 ~ n_students / n_tested,
        TRUE ~ NA_real_
      )
    )
}


#' Calculate proficiency rates (on_track + mastered)
#'
#' Calculates the combined proficiency rate (students scoring on_track or mastered)
#' for each row or group in the assessment data.
#'
#' @param df Tidy assessment data frame (must have proficiency_level column)
#' @param ... Optional grouping variables (unquoted)
#' @return Data frame with proficiency rates
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_assessment(2024)
#' # Overall proficiency by state/district/school
#' calc_proficiency(tidy_data)
#'
#' # Proficiency by subject
#' calc_proficiency(tidy_data, subject)
#' }
calc_proficiency <- function(df, ...) {

  if (!"proficiency_level" %in% names(df)) {
    stop("Input must be tidy assessment data (use tidy = TRUE)")
  }

  group_vars <- rlang::enquos(...)

  # If grouping vars provided, group by them plus base identifiers
  if (length(group_vars) > 0) {
    result <- df |>
      dplyr::filter(proficiency_level %in% c("on_track", "mastered")) |>
      dplyr::group_by(!!!group_vars) |>
      dplyr::summarize(
        n_proficient = sum(n_students, na.rm = TRUE),
        n_tested = dplyr::first(n_tested),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_proficient = dplyr::case_when(
          n_tested > 0 ~ n_proficient / n_tested,
          TRUE ~ NA_real_
        )
      )
  } else {
    # Group by row identifiers
    id_cols <- c("end_year", "type", "district_id", "school_id",
                 "test", "subject", "grade", "subgroup")
    id_cols <- id_cols[id_cols %in% names(df)]

    result <- df |>
      dplyr::filter(proficiency_level %in% c("on_track", "mastered")) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarize(
        n_proficient = sum(n_students, na.rm = TRUE),
        n_tested = dplyr::first(n_tested),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_proficient = dplyr::case_when(
          n_tested > 0 ~ n_proficient / n_tested,
          TRUE ~ NA_real_
        )
      )
  }

  result
}


#' Create empty tidy assessment data frame
#'
#' @return Empty data frame with tidy assessment columns
#' @keywords internal
create_empty_tidy_assessment <- function() {
  data.frame(
    end_year = integer(0),
    type = character(0),
    district_id = character(0),
    district_name = character(0),
    school_id = character(0),
    school_name = character(0),
    test = character(0),
    subject = character(0),
    grade = character(0),
    subgroup = character(0),
    n_tested = integer(0),
    proficiency_level = character(0),
    n_students = numeric(0),
    pct = numeric(0),
    is_state = logical(0),
    is_district = logical(0),
    is_school = logical(0),
    stringsAsFactors = FALSE
  )
}
