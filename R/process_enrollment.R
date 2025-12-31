# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw TDOE enrollment data into a
# clean, standardized format.
#
# ==============================================================================


#' Process raw TDOE enrollment data
#'
#' Transforms raw TDOE data into a standardized schema combining school
#' and district data.
#'
#' @param raw_data List containing school and district data frames from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process school data
  school_processed <- process_school_enr(raw_data$school, end_year)

  # Process district data
  district_processed <- process_district_enr(raw_data$district, end_year)

  # Create state aggregate
  state_processed <- create_state_aggregate(district_processed, end_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, district_processed, school_processed)

  result
}


#' Process school-level enrollment data
#'
#' @param df Raw school data frame
#' @param end_year School year end
#' @return Processed school data frame
#' @keywords internal
process_school_enr <- function(df, end_year) {

  if (nrow(df) == 0) {
    return(create_empty_result(end_year, "Campus"))
  }

  cols <- names(df)
  n_rows <- nrow(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe with same number of rows as input
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("Campus", n_rows),
    stringsAsFactors = FALSE
  )

  # District IDs - Tennessee uses 4-digit district IDs
  district_col <- find_col(c("^district.*id$", "^dist.*id$", "^district.*no$", "^system.*id$", "^system$"))
  if (!is.null(district_col)) {
    result$district_id <- sprintf("%04d", as.integer(trimws(df[[district_col]])))
  } else {
    result$district_id <- rep(NA_character_, n_rows)
  }

  # School IDs - Tennessee uses 4-digit school IDs within district
  school_col <- find_col(c("^school.*id$", "^school.*no$", "^building.*id$", "^school$"))
  if (!is.null(school_col)) {
    school_ids <- sprintf("%04d", as.integer(trimws(df[[school_col]])))
    # Create full campus ID (district + school)
    result$campus_id <- paste0(result$district_id, school_ids)
  } else {
    result$campus_id <- rep(NA_character_, n_rows)
  }

  # Names
  district_name_col <- find_col(c("^district.*name$", "^dist.*name$", "^system.*name$"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  } else {
    result$district_name <- rep(NA_character_, n_rows)
  }

  school_name_col <- find_col(c("^school.*name$", "^building.*name$"))
  if (!is.null(school_name_col)) {
    result$campus_name <- trimws(df[[school_name_col]])
  } else {
    result$campus_name <- rep(NA_character_, n_rows)
  }

  # County
  county_col <- find_col(c("^county$", "^county.*name$"))
  if (!is.null(county_col)) {
    result$county <- trimws(df[[county_col]])
  }

  # Region (Tennessee has education regions/CORE regions)
  region_col <- find_col(c("^region$", "^core.*region$", "^region.*id$"))
  if (!is.null(region_col)) {
    result$region <- trimws(df[[region_col]])
  }

  # Total enrollment
  total_col <- find_col(c("^total$", "^total.*enrollment$", "^enrollment$", "^membership$", "^adm$", "^total.*count$"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics
  demo_map <- list(
    white = c("^white$", "^white.*count$", "^white.*n$", "^wht$"),
    black = c("^black$", "^black.*count$", "^african.*american$", "^blk$", "^bla$"),
    hispanic = c("^hispanic$", "^hispanic.*count$", "^latino$", "^hsp$", "^his$"),
    asian = c("^asian$", "^asian.*count$", "^asn$", "^asi$"),
    pacific_islander = c("^pacific.*islander$", "^native.*hawaiian$", "^nhp$", "^pac$"),
    native_american = c("^american.*indian$", "^native.*american$", "^alaska.*native$", "^ami$", "^ind$"),
    multiracial = c("^multi.*racial$", "^two.*more$", "^multiple.*race$", "^mul$", "^mlt$")
  )

  for (name in names(demo_map)) {
    col <- find_col(demo_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  male_col <- find_col(c("^male$", "^male.*count$", "^m$"))
  if (!is.null(male_col)) {
    result$male <- safe_numeric(df[[male_col]])
  }

  female_col <- find_col(c("^female$", "^female.*count$", "^f$"))
  if (!is.null(female_col)) {
    result$female <- safe_numeric(df[[female_col]])
  }

  # Special populations
  special_map <- list(
    econ_disadv = c("^economically.*disadvantaged$", "^econ.*disadv$", "^ed$", "^free.*reduced$"),
    lep = c("^english.*learner$", "^lep$", "^ell$", "^el$", "^limited.*english$"),
    special_ed = c("^special.*education$", "^sped$", "^disability$", "^iep$", "^swd$")
  )

  for (name in names(special_map)) {
    col <- find_col(special_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels - Tennessee standard grade naming
  grade_map <- list(
    grade_pk = c("^pk$", "^pre.*k$", "^prek$", "^pre-k$"),
    grade_k = c("^k$", "^kindergarten$", "^kg$"),
    grade_01 = c("^1$", "^grade.*1$", "^gr.*1$", "^first$"),
    grade_02 = c("^2$", "^grade.*2$", "^gr.*2$", "^second$"),
    grade_03 = c("^3$", "^grade.*3$", "^gr.*3$", "^third$"),
    grade_04 = c("^4$", "^grade.*4$", "^gr.*4$", "^fourth$"),
    grade_05 = c("^5$", "^grade.*5$", "^gr.*5$", "^fifth$"),
    grade_06 = c("^6$", "^grade.*6$", "^gr.*6$", "^sixth$"),
    grade_07 = c("^7$", "^grade.*7$", "^gr.*7$", "^seventh$"),
    grade_08 = c("^8$", "^grade.*8$", "^gr.*8$", "^eighth$"),
    grade_09 = c("^9$", "^grade.*9$", "^gr.*9$", "^ninth$"),
    grade_10 = c("^10$", "^grade.*10$", "^gr.*10$", "^tenth$"),
    grade_11 = c("^11$", "^grade.*11$", "^gr.*11$", "^eleventh$"),
    grade_12 = c("^12$", "^grade.*12$", "^gr.*12$", "^twelfth$")
  )

  for (name in names(grade_map)) {
    col <- find_col(grade_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  result
}


#' Process district-level enrollment data
#'
#' @param df Raw district data frame
#' @param end_year School year end
#' @return Processed district data frame
#' @keywords internal
process_district_enr <- function(df, end_year) {

  if (nrow(df) == 0) {
    return(create_empty_result(end_year, "District"))
  }

  cols <- names(df)
  n_rows <- nrow(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe with same number of rows as input
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("District", n_rows),
    stringsAsFactors = FALSE
  )

  # District IDs
  district_col <- find_col(c("^district.*id$", "^dist.*id$", "^district.*no$", "^system.*id$", "^system$"))
  if (!is.null(district_col)) {
    result$district_id <- sprintf("%04d", as.integer(trimws(df[[district_col]])))
  } else {
    result$district_id <- rep(NA_character_, n_rows)
  }

  # Campus ID is NA for district rows
  result$campus_id <- rep(NA_character_, n_rows)

  # Names
  district_name_col <- find_col(c("^district.*name$", "^dist.*name$", "^system.*name$"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  } else {
    result$district_name <- rep(NA_character_, n_rows)
  }

  result$campus_name <- rep(NA_character_, n_rows)

  # County
  county_col <- find_col(c("^county$", "^county.*name$"))
  if (!is.null(county_col)) {
    result$county <- trimws(df[[county_col]])
  }

  # Region
  region_col <- find_col(c("^region$", "^core.*region$", "^region.*id$"))
  if (!is.null(region_col)) {
    result$region <- trimws(df[[region_col]])
  }

  # Total enrollment
  total_col <- find_col(c("^total$", "^total.*enrollment$", "^enrollment$", "^membership$", "^adm$"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics
  demo_map <- list(
    white = c("^white$", "^white.*count$", "^white.*n$", "^wht$"),
    black = c("^black$", "^black.*count$", "^african.*american$", "^blk$", "^bla$"),
    hispanic = c("^hispanic$", "^hispanic.*count$", "^latino$", "^hsp$", "^his$"),
    asian = c("^asian$", "^asian.*count$", "^asn$", "^asi$"),
    pacific_islander = c("^pacific.*islander$", "^native.*hawaiian$", "^nhp$", "^pac$"),
    native_american = c("^american.*indian$", "^native.*american$", "^alaska.*native$", "^ami$", "^ind$"),
    multiracial = c("^multi.*racial$", "^two.*more$", "^multiple.*race$", "^mul$", "^mlt$")
  )

  for (name in names(demo_map)) {
    col <- find_col(demo_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  male_col <- find_col(c("^male$", "^male.*count$", "^m$"))
  if (!is.null(male_col)) {
    result$male <- safe_numeric(df[[male_col]])
  }

  female_col <- find_col(c("^female$", "^female.*count$", "^f$"))
  if (!is.null(female_col)) {
    result$female <- safe_numeric(df[[female_col]])
  }

  # Special populations
  special_map <- list(
    econ_disadv = c("^economically.*disadvantaged$", "^econ.*disadv$", "^ed$", "^free.*reduced$"),
    lep = c("^english.*learner$", "^lep$", "^ell$", "^el$", "^limited.*english$"),
    special_ed = c("^special.*education$", "^sped$", "^disability$", "^iep$", "^swd$")
  )

  for (name in names(special_map)) {
    col <- find_col(special_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels
  grade_map <- list(
    grade_pk = c("^pk$", "^pre.*k$", "^prek$", "^pre-k$"),
    grade_k = c("^k$", "^kindergarten$", "^kg$"),
    grade_01 = c("^1$", "^grade.*1$", "^gr.*1$"),
    grade_02 = c("^2$", "^grade.*2$", "^gr.*2$"),
    grade_03 = c("^3$", "^grade.*3$", "^gr.*3$"),
    grade_04 = c("^4$", "^grade.*4$", "^gr.*4$"),
    grade_05 = c("^5$", "^grade.*5$", "^gr.*5$"),
    grade_06 = c("^6$", "^grade.*6$", "^gr.*6$"),
    grade_07 = c("^7$", "^grade.*7$", "^gr.*7$"),
    grade_08 = c("^8$", "^grade.*8$", "^gr.*8$"),
    grade_09 = c("^9$", "^grade.*9$", "^gr.*9$"),
    grade_10 = c("^10$", "^grade.*10$", "^gr.*10$"),
    grade_11 = c("^11$", "^grade.*11$", "^gr.*11$"),
    grade_12 = c("^12$", "^grade.*12$", "^gr.*12$")
  )

  for (name in names(grade_map)) {
    col <- find_col(grade_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  result
}


#' Create state-level aggregate from district data
#'
#' @param district_df Processed district data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(district_df, end_year) {

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "male", "female",
    "econ_disadv", "lep", "special_ed",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    region = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    if (col %in% names(district_df)) {
      state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
    }
  }

  state_row
}


#' Create empty result data frame
#'
#' @param end_year School year end
#' @param type Record type ("State", "District", "Campus")
#' @return Empty data frame with expected columns
#' @keywords internal
create_empty_result <- function(end_year, type) {
  data.frame(
    end_year = integer(0),
    type = character(0),
    district_id = character(0),
    campus_id = character(0),
    district_name = character(0),
    campus_name = character(0),
    county = character(0),
    region = character(0),
    row_total = integer(0),
    white = integer(0),
    black = integer(0),
    hispanic = integer(0),
    asian = integer(0),
    pacific_islander = integer(0),
    native_american = integer(0),
    multiracial = integer(0),
    male = integer(0),
    female = integer(0),
    econ_disadv = integer(0),
    lep = integer(0),
    special_ed = integer(0),
    grade_pk = integer(0),
    grade_k = integer(0),
    grade_01 = integer(0),
    grade_02 = integer(0),
    grade_03 = integer(0),
    grade_04 = integer(0),
    grade_05 = integer(0),
    grade_06 = integer(0),
    grade_07 = integer(0),
    grade_08 = integer(0),
    grade_09 = integer(0),
    grade_10 = integer(0),
    grade_11 = integer(0),
    grade_12 = integer(0),
    stringsAsFactors = FALSE
  )
}
