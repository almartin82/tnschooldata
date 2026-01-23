# ==============================================================================
# Assessment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw TDOE assessment data into a
# clean, standardized format.
#
# ==============================================================================


#' Process raw TDOE assessment data
#'
#' Transforms raw TDOE assessment data into a standardized schema combining
#' state, district, and school data.
#'
#' @param raw_data List containing state, district, and/or school data frames from get_raw_assessment
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_assessment <- function(raw_data, end_year) {

  result_list <- list()

  # Process each level if present
  if ("state" %in% names(raw_data) && nrow(raw_data$state) > 0) {
    result_list$state <- process_assessment_level(raw_data$state, end_year, "State")
  }

  if ("district" %in% names(raw_data) && nrow(raw_data$district) > 0) {
    result_list$district <- process_assessment_level(raw_data$district, end_year, "District")
  }

  if ("school" %in% names(raw_data) && nrow(raw_data$school) > 0) {
    result_list$school <- process_assessment_level(raw_data$school, end_year, "School")
  }

  # Combine all levels
  if (length(result_list) == 0) {
    return(create_empty_assessment_result(end_year))
  }

  dplyr::bind_rows(result_list)
}


#' Process a single level of assessment data
#'
#' @param df Raw data frame for one level (state/district/school)
#' @param end_year School year end
#' @param type Record type ("State", "District", "School")
#' @return Processed data frame
#' @keywords internal
process_assessment_level <- function(df, end_year, type) {

  if (nrow(df) == 0) {
    return(create_empty_assessment_result(end_year))
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

  # Build result dataframe
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep(type, n_rows),
    stringsAsFactors = FALSE
  )

  # District ID - TDOE uses "system" or "district_no"
  district_col <- find_col(c("^system$", "^district.*no$", "^district.*id$", "^district$"))
  if (!is.null(district_col)) {
    district_vals <- trimws(as.character(df[[district_col]]))
    # Handle "0" for state level in district/school files
    result$district_id <- ifelse(
      district_vals == "0" | district_vals == "" | is.na(district_vals),
      NA_character_,
      sprintf("%04d", as.integer(district_vals))
    )
  } else {
    result$district_id <- rep(NA_character_, n_rows)
  }

  # District name
  district_name_col <- find_col(c("^system_name$", "^district.*name$"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(as.character(df[[district_name_col]]))
  } else {
    result$district_name <- rep(NA_character_, n_rows)
  }

  # School ID (only for school level)
  school_col <- find_col(c("^school$", "^school_id$", "^school.*no$"))
  if (!is.null(school_col)) {
    school_vals <- trimws(as.character(df[[school_col]]))
    result$school_id <- ifelse(
      school_vals == "0" | school_vals == "" | is.na(school_vals),
      NA_character_,
      sprintf("%04d", as.integer(school_vals))
    )
  } else {
    result$school_id <- rep(NA_character_, n_rows)
  }

  # School name
  school_name_col <- find_col(c("^school_name$"))
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(as.character(df[[school_name_col]]))
  } else {
    result$school_name <- rep(NA_character_, n_rows)
  }

  # Test type (e.g., TCAP, EOC, etc.)
  test_col <- find_col(c("^test$", "^test_type$", "^assessment$"))
  if (!is.null(test_col)) {
    result$test <- toupper(trimws(as.character(df[[test_col]])))
  } else {
    result$test <- rep(NA_character_, n_rows)
  }

  # Subject
  subject_col <- find_col(c("^subject$", "^content_area$"))
  if (!is.null(subject_col)) {
    result$subject <- standardize_subject(df[[subject_col]])
  } else {
    result$subject <- rep(NA_character_, n_rows)
  }

  # Grade
  grade_col <- find_col(c("^grade$", "^grade_level$", "^tested_grade$"))
  if (!is.null(grade_col)) {
    result$grade <- standardize_grade(df[[grade_col]])
  } else {
    result$grade <- rep(NA_character_, n_rows)
  }

  # Subgroup
  subgroup_col <- find_col(c("^subgroup$", "^student_group$", "^demographic$"))
  if (!is.null(subgroup_col)) {
    result$subgroup <- standardize_subgroup(df[[subgroup_col]])
  } else {
    result$subgroup <- rep("All Students", n_rows)
  }

  # Number tested
  n_tested_col <- find_col(c("^valid_tests$", "^n_tested$", "^tested$", "^num_tested$", "^number_tested$"))
  if (!is.null(n_tested_col)) {
    result$n_tested <- safe_numeric(df[[n_tested_col]])
  } else {
    result$n_tested <- rep(NA_integer_, n_rows)
  }

  # Proficiency percentages
  pct_below_col <- find_col(c("^pct_below$", "^percent_below$", "^below_pct$", "^below$"))
  if (!is.null(pct_below_col)) {
    result$pct_below <- safe_numeric(df[[pct_below_col]])
  } else {
    result$pct_below <- rep(NA_real_, n_rows)
  }

  pct_approach_col <- find_col(c("^pct_approaching$", "^percent_approaching$", "^approaching_pct$", "^approaching$"))
  if (!is.null(pct_approach_col)) {
    result$pct_approaching <- safe_numeric(df[[pct_approach_col]])
  } else {
    result$pct_approaching <- rep(NA_real_, n_rows)
  }

  # TDOE uses "met_expectations" for "on_track"
  pct_ontrack_col <- find_col(c("^pct_on_track$", "^percent_on_track$", "^on_track_pct$", "^on_track$", "^pct_ontrack$",
                                 "^pct_met_expectations$", "^percent_met_expectations$", "^met_expectations$"))
  if (!is.null(pct_ontrack_col)) {
    result$pct_on_track <- safe_numeric(df[[pct_ontrack_col]])
  } else {
    result$pct_on_track <- rep(NA_real_, n_rows)
  }

  # TDOE uses "exceeded_expectations" for "mastered"
  pct_mastered_col <- find_col(c("^pct_mastered$", "^percent_mastered$", "^mastered_pct$", "^mastered$",
                                  "^pct_exceeded_expectations$", "^percent_exceeded_expectations$", "^exceeded_expectations$"))
  if (!is.null(pct_mastered_col)) {
    result$pct_mastered <- safe_numeric(df[[pct_mastered_col]])
  } else {
    result$pct_mastered <- rep(NA_real_, n_rows)
  }

  # Combined on track + mastered (proficient)
  # TDOE uses "met_exceeded" for combined proficient rate
  pct_proficient_col <- find_col(c("^pct_on_mastered$", "^pct_proficient$", "^percent_proficient$",
                                    "^pct_on_track_mastered$", "^on_track_mastered$",
                                    "^pct_met_exceeded$", "^percent_met_exceeded$", "^met_exceeded$"))
  if (!is.null(pct_proficient_col)) {
    result$pct_proficient <- safe_numeric(df[[pct_proficient_col]])
  } else {
    # Calculate if we have the components
    if (!all(is.na(result$pct_on_track)) && !all(is.na(result$pct_mastered))) {
      result$pct_proficient <- result$pct_on_track + result$pct_mastered
    } else {
      result$pct_proficient <- rep(NA_real_, n_rows)
    }
  }

  result
}


#' Standardize subject names
#'
#' @param x Vector of subject names
#' @return Standardized subject names
#' @keywords internal
standardize_subject <- function(x) {
  x <- toupper(trimws(as.character(x)))

  # Standard subject mappings
  x <- gsub("^ELA$|^ENGLISH.*LANGUAGE.*ARTS$|^READING$|^RLA$", "ELA", x)
  x <- gsub("^MATH$|^MATHEMATICS$", "Math", x)
  x <- gsub("^SCIENCE$|^SCI$", "Science", x)
  x <- gsub("^SOCIAL.*STUDIES$|^SS$|^SOC.*STU$", "Social Studies", x)

  # EOC subjects - order matters! More specific patterns first
  x <- gsub("^ALGEBRA\\s*II$|^ALG\\s*2$", "Algebra II", x)
  x <- gsub("^ALGEBRA\\s*I$|^ALG\\s*1$", "Algebra I", x)
  x <- gsub("^GEOMETRY$|^GEO$", "Geometry", x)
  x <- gsub("^ENGLISH\\s*III$|^ENG\\s*3$", "English III", x)
  x <- gsub("^ENGLISH\\s*II$|^ENG\\s*2$", "English II", x)
  x <- gsub("^ENGLISH\\s*I$|^ENG\\s*1$", "English I", x)
  x <- gsub("^BIOLOGY\\s*I$|^BIO\\s*1$|^BIOLOGY$", "Biology I", x)
  x <- gsub("^US.*HISTORY.*GEOGRAPHY$|^US.*HIST.*GEO$", "US History", x)
  x <- gsub("^CHEMISTRY$|^CHEM$", "Chemistry", x)
  x <- gsub("^INTEGRATED\\s*MATH\\s*III$|^INT\\s*MATH\\s*3$", "Integrated Math III", x)
  x <- gsub("^INTEGRATED\\s*MATH\\s*II$|^INT\\s*MATH\\s*2$", "Integrated Math II", x)
  x <- gsub("^INTEGRATED\\s*MATH\\s*I$|^INT\\s*MATH\\s*1$", "Integrated Math I", x)

  x
}


#' Standardize grade levels
#'
#' @param x Vector of grade values
#' @return Standardized grade levels
#' @keywords internal
standardize_grade <- function(x) {
  x <- toupper(trimws(as.character(x)))

  # Remove GRADE prefix first
  x <- gsub("^GRADE\\s*", "", x)

  # Handle ordinal formats
  x <- gsub("^3RD$", "03", x)
  x <- gsub("^4TH$", "04", x)
  x <- gsub("^5TH$", "05", x)
  x <- gsub("^6TH$", "06", x)
  x <- gsub("^7TH$", "07", x)
  x <- gsub("^8TH$", "08", x)

  # Pad single digits to two digits (after prefix removal)
  x <- gsub("^([3-9])$", "0\\1", x)

  # EOC/HS indicators
  x <- gsub("^EOC$|^END.*OF.*COURSE$", "EOC", x)
  x <- gsub("^HS$|^HIGH.*SCHOOL$", "HS", x)
  x <- gsub("^ALL.*GRADES$|^ALL$", "All", x)

  x
}


#' Standardize subgroup names
#'
#' @param x Vector of subgroup names
#' @return Standardized subgroup names
#' @keywords internal
standardize_subgroup <- function(x) {
  x <- trimws(as.character(x))

  # Create a mapping of common variations to standard names
  subgroup_map <- c(
    # All students
    "All Students" = "All Students",
    "ALL STUDENTS" = "All Students",
    "All" = "All Students",
    "ALL" = "All Students",

    # Race/ethnicity
    "Black/African American" = "Black",
    "BLACK/AFRICAN AMERICAN" = "Black",
    "Black" = "Black",
    "BLACK" = "Black",
    "African American" = "Black",

    "White" = "White",
    "WHITE" = "White",

    "Hispanic/Latino" = "Hispanic",
    "HISPANIC/LATINO" = "Hispanic",
    "Hispanic" = "Hispanic",
    "HISPANIC" = "Hispanic",
    "Latino" = "Hispanic",

    "Asian" = "Asian",
    "ASIAN" = "Asian",

    "American Indian/Alaska Native" = "Native American",
    "AMERICAN INDIAN/ALASKA NATIVE" = "Native American",
    "American Indian" = "Native American",
    "Native American" = "Native American",

    "Native Hawaiian/Pacific Islander" = "Pacific Islander",
    "NATIVE HAWAIIAN/PACIFIC ISLANDER" = "Pacific Islander",
    "Pacific Islander" = "Pacific Islander",
    "Hawaiian/Pacific Islander" = "Pacific Islander",

    "Two or More Races" = "Multiracial",
    "TWO OR MORE RACES" = "Multiracial",
    "Multiracial" = "Multiracial",
    "Multiple Races" = "Multiracial",

    # Gender
    "Female" = "Female",
    "FEMALE" = "Female",
    "Male" = "Male",
    "MALE" = "Male",

    # Special populations
    "Economically Disadvantaged" = "Economically Disadvantaged",
    "ECONOMICALLY DISADVANTAGED" = "Economically Disadvantaged",
    "ED" = "Economically Disadvantaged",
    "Free/Reduced Lunch" = "Economically Disadvantaged",

    "Non-Economically Disadvantaged" = "Non-Economically Disadvantaged",
    "NON-ECONOMICALLY DISADVANTAGED" = "Non-Economically Disadvantaged",

    "Students with Disabilities" = "Students with Disabilities",
    "STUDENTS WITH DISABILITIES" = "Students with Disabilities",
    "SWD" = "Students with Disabilities",
    "Special Education" = "Students with Disabilities",
    "SPED" = "Students with Disabilities",

    "Non-Students with Disabilities" = "Non-SWD",
    "NON-STUDENTS WITH DISABILITIES" = "Non-SWD",
    "Non-SWD" = "Non-SWD",

    "English Learners" = "English Learners",
    "ENGLISH LEARNERS" = "English Learners",
    "EL" = "English Learners",
    "ELL" = "English Learners",
    "LEP" = "English Learners",
    "Limited English Proficient" = "English Learners",
    "English Learner" = "English Learners",

    "Non-English Learners" = "Non-EL",
    "NON-ENGLISH LEARNERS" = "Non-EL",
    "Non-EL" = "Non-EL",
    "Non-ELL" = "Non-EL",

    # Super subgroups
    "Super Subgroup" = "Super Subgroup",
    "SUPER SUBGROUP" = "Super Subgroup",
    "BHN" = "BHN",
    "Black/Hispanic/Native American" = "BHN"
  )

  # Apply mapping
  result <- subgroup_map[x]

  # For values not in the map, keep original
  result[is.na(result)] <- x[is.na(result)]

  # Remove names
  unname(result)
}


#' Create empty assessment result data frame
#'
#' @param end_year School year end
#' @return Empty data frame with expected columns
#' @keywords internal
create_empty_assessment_result <- function(end_year) {
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
    pct_below = numeric(0),
    pct_approaching = numeric(0),
    pct_on_track = numeric(0),
    pct_mastered = numeric(0),
    pct_proficient = numeric(0),
    stringsAsFactors = FALSE
  )
}
