# ==============================================================================
# Assessment Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading assessment data from the
# Tennessee Department of Education (TDOE) accountability portal.
#
# Available years: 2019, 2021, 2022, 2023, 2024, 2025 (no 2020 due to COVID waiver)
#
# ==============================================================================

#' Fetch Tennessee assessment data
#'
#' Downloads and processes assessment data from the Tennessee Department of
#' Education accountability portal.
#'
#' Data is available from 2019-2025, excluding 2020 (COVID-19 testing waiver).
#' Assessment data includes proficiency levels (Below, Approaching, On Track, Mastered)
#' for subjects including ELA, Math, Science, Social Studies, and EOC courses.
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 2019, 2021-2025.
#' @param level Level of data to fetch: "all" (default), "state", "district", or "school"
#' @param tidy If TRUE (default), returns data in long (tidy) format with proficiency_level
#'   column. If FALSE, returns wide format with separate pct_* columns.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from TDOE.
#' @return Data frame with assessment data. Wide format includes columns for
#'   proficiency percentages (pct_below, pct_approaching, pct_on_track, pct_mastered).
#'   Tidy format pivots these into proficiency_level and pct columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 assessment data (2023-24 school year)
#' assess_2024 <- fetch_assessment(2024)
#'
#' # Get only state-level data
#' state_assess <- fetch_assessment(2024, level = "state")
#'
#' # Get wide format (pct columns not pivoted)
#' assess_wide <- fetch_assessment(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' assess_fresh <- fetch_assessment(2024, use_cache = FALSE)
#'
#' # Filter to math results
#' math_results <- assess_2024 |>
#'   dplyr::filter(subject == "Math", is_state)
#' }
fetch_assessment <- function(end_year, level = "all", tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available <- get_available_assessment_years()
  if (!end_year %in% available$years) {
    if (end_year == 2020) {
      stop(available$note)
    }
    stop(paste0(
      "end_year must be one of: ", paste(available$years, collapse = ", "),
      "\nGot: ", end_year
    ))
  }

  # Validate level
  level <- tolower(level)
  if (!level %in% c("all", "state", "district", "school")) {
    stop("level must be one of 'all', 'state', 'district', 'school'")
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && assessment_cache_exists(end_year, cache_type, level)) {
    message(paste("Using cached assessment data for", end_year))
    return(read_assessment_cache(end_year, cache_type, level))
  }

  # Get raw data from TDOE
  raw <- get_raw_assessment(end_year, level)

  # Process to standard schema
  processed <- process_assessment(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_assessment(processed)
  } else {
    # Add aggregation flags to wide format too
    processed <- id_assessment_aggs(processed)
  }

  # Cache the result
  if (use_cache) {
    write_assessment_cache(processed, end_year, cache_type, level)
  }

  processed
}


#' Fetch assessment data for multiple years
#'
#' Downloads and combines assessment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#'   Note: 2020 is not available due to COVID-19 testing waiver.
#' @param level Level of data to fetch: "all" (default), "state", "district", or "school"
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with assessment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data (excluding 2020)
#' assess_multi <- fetch_assessment_multi(c(2021, 2022, 2023))
#'
#' # Track proficiency trends at state level
#' assess_multi |>
#'   dplyr::filter(is_state, subject == "Math", grade == "All", subgroup == "All Students") |>
#'   dplyr::filter(proficiency_level %in% c("on_track", "mastered")) |>
#'   dplyr::group_by(end_year) |>
#'   dplyr::summarize(pct_proficient = sum(pct, na.rm = TRUE))
#' }
fetch_assessment_multi <- function(end_years, level = "all", tidy = TRUE, use_cache = TRUE) {

  # Validate years
  available <- get_available_assessment_years()

  # Check for 2020
  if (2020 %in% end_years) {
    warning("2020 excluded: ", available$note)
    end_years <- end_years[end_years != 2020]
  }

  invalid_years <- end_years[!end_years %in% available$years]
  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "),
      "\nend_year must be one of: ", paste(available$years, collapse = ", ")
    ))
  }

  if (length(end_years) == 0) {
    stop("No valid years to fetch")
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_assessment(yr, level = level, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}


#' Get assessment data for a specific district
#'
#' Convenience function to fetch assessment data for a single district.
#'
#' @param end_year School year end
#' @param district_id 4-digit district ID (e.g., "0470" for Knox County)
#' @param tidy If TRUE (default), returns tidy format
#' @param use_cache If TRUE (default), uses cached data
#' @return Data frame filtered to specified district
#' @export
#' @examples
#' \dontrun{
#' # Get Knox County (district 0470) assessment data
#' knox_assess <- fetch_district_assessment(2024, "0470")
#'
#' # Get Metro Nashville (district 0190) data
#' metro_assess <- fetch_district_assessment(2024, "0190")
#' }
fetch_district_assessment <- function(end_year, district_id, tidy = TRUE, use_cache = TRUE) {

  # Normalize district_id
  district_id <- sprintf("%04d", as.integer(district_id))

  # Fetch district-level data (faster than fetching all)
  df <- fetch_assessment(end_year, level = "district", tidy = tidy, use_cache = use_cache)

  # Filter to requested district
  df |>
    dplyr::filter(district_id == !!district_id)
}


#' Get assessment data for a specific school
#'
#' Convenience function to fetch assessment data for a single school.
#'
#' @param end_year School year end
#' @param district_id 4-digit district ID
#' @param school_id 4-digit school ID
#' @param tidy If TRUE (default), returns tidy format
#' @param use_cache If TRUE (default), uses cached data
#' @return Data frame filtered to specified school
#' @export
#' @examples
#' \dontrun{
#' # Get a specific school's assessment data
#' school_assess <- fetch_school_assessment(2024, "0470", "0010")
#' }
fetch_school_assessment <- function(end_year, district_id, school_id, tidy = TRUE, use_cache = TRUE) {

  # Normalize IDs
  district_id <- sprintf("%04d", as.integer(district_id))
  school_id <- sprintf("%04d", as.integer(school_id))

  # Fetch school-level data
  df <- fetch_assessment(end_year, level = "school", tidy = tidy, use_cache = use_cache)

  # Filter to requested school
  df |>
    dplyr::filter(district_id == !!district_id, school_id == !!school_id)
}
