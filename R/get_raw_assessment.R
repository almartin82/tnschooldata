# ==============================================================================
# Raw Assessment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw assessment data from TDOE.
#
# Tennessee Department of Education provides assessment data through:
# - Data Downloads page: https://www.tn.gov/education/districts/federal-programs-and-oversight/data/data-downloads.html
# - Accountability Portal: https://www.tn.gov/content/dam/tn/education/accountability/
#
# Data structure:
# - Assessment files include state, district, and school level data
# - Proficiency levels: Below, Approaching, On Track, Mastered
# - Subjects: ELA, Math, Science, Social Studies, EOC courses
#
# Available years: 2019, 2021, 2022, 2023, 2024, 2025 (no 2020 due to COVID waiver)
#
# ==============================================================================


#' Get assessment URL for a given year and level
#'
#' Constructs the URL for downloading assessment data from TDOE's accountability portal.
#' URLs have irregular patterns that vary by year, so we use a lookup table.
#'
#' @param end_year School year end
#' @param level One of "state", "district", "school"
#' @return URL string or NULL if not found
#' @keywords internal
get_assessment_url <- function(end_year, level) {

  # Base URL for accountability data
  base_url <- "https://www.tn.gov/content/dam/tn/education/accountability/"

  # Normalize level
  level <- tolower(level)
  if (!level %in% c("state", "district", "school")) {
    stop("level must be one of 'state', 'district', 'school'")
  }

  # Year-specific URL patterns (TDOE has inconsistent naming)
  # Structure: list(year = list(level = filename))
  url_patterns <- list(
    "2025" = list(
      state = "2025/state_assessment_file_suppressed_2025.xlsx",
      district = "2025/district_assessment_file_suppressed_2025.xlsx",
      school = "2025/school_assessment_file_suppressed_2025.xlsx"
    ),
    "2024" = list(
      state = "2024/state_assessment_file_suppressed_2024.xlsx",
      district = "2024/district_assessment_file_suppressed_2024.xlsx",
      school = "2024/school_assessment_file_suppressed_2024.xlsx"
    ),
    "2023" = list(
      state = "2023/state_assessment_file_suppressed_2023.xlsx",
      district = "2023/district_assessment_file_suppressed_2023.xlsx",
      school = "2023/school_assessment_file_suppressed_2023.xlsx"
    ),
    "2022" = list(
      # Note: 2022 has irregular suffix "upd32323"
      state = "2022/state_assessment_file_suppressed_upd32323.xlsx",
      district = "2022/district_assessment_file_suppressed_upd32323.xlsx",
      school = "2022/school_assessment_file_suppressed_upd32323.xlsx"
    ),
    "2021" = list(
      # Note: 2021 uses CSV format with "upd422" suffix
      state = "2021/state_assessment_file_suppressed_upd422.csv",
      district = "2021/district_assessment_file_suppressed_upd422.csv",
      school = "2021/school_assessment_file_suppressed_upd422.csv"
    ),
    "2019" = list(
      # Note: 2019 uses CSV format with no year suffix
      state = "2019/state_assessment_file_suppressed.csv",
      district = "2019/district_assessment_file_suppressed.csv",
      school = "2019/school_assessment_file_suppressed.csv"
    )
  )

  year_str <- as.character(end_year)

  if (!year_str %in% names(url_patterns)) {
    return(NULL)
  }

  if (!level %in% names(url_patterns[[year_str]])) {
    return(NULL)
  }

  paste0(base_url, url_patterns[[year_str]][[level]])
}


#' Download raw assessment data from TDOE
#'
#' Downloads state, district, and/or school assessment data from Tennessee DOE's
#' accountability portal.
#'
#' @param end_year School year end (2019, 2021-2025; no 2020 due to COVID waiver)
#' @param level One of "all", "state", "district", "school"
#' @return List with state, district, and/or school data frames (depending on level)
#' @keywords internal
get_raw_assessment <- function(end_year, level = "all") {

  # Validate year
  available <- get_available_assessment_years()
  if (!end_year %in% available$years) {
    if (end_year == 2020) {
      stop("Assessment data is not available for 2020 due to COVID-19 testing waiver.")
    }
    stop(paste0(
      "end_year must be one of: ", paste(available$years, collapse = ", "),
      "\nGot: ", end_year
    ))
  }

  message(paste("Downloading TDOE assessment data for", end_year, "..."))

  # Determine which levels to download
  level <- tolower(level)
  if (level == "all") {
    levels_to_download <- c("state", "district", "school")
  } else if (level %in% c("state", "district", "school")) {
    levels_to_download <- level
  } else {
    stop("level must be one of 'all', 'state', 'district', 'school'")
  }

  # Download each level
  result <- list()

  for (lv in levels_to_download) {
    message(paste("  Downloading", lv, "level data..."))
    df <- download_assessment_file(end_year, lv)
    result[[lv]] <- df
  }

  result
}


#' Download a single assessment file
#'
#' @param end_year School year end
#' @param level One of "state", "district", "school"
#' @return Data frame or empty data frame if download fails
#' @keywords internal
download_assessment_file <- function(end_year, level) {

  url <- get_assessment_url(end_year, level)

  if (is.null(url)) {
    message(paste("  No URL pattern defined for", end_year, level))
    return(create_empty_assessment_raw())
  }

  # Determine file extension
  is_csv <- grepl("\\.csv$", url, ignore.case = TRUE)
  file_ext <- if (is_csv) ".csv" else ".xlsx"

  # Create temp file
  tname <- tempfile(
    pattern = paste0("tdoe_assessment_", level, "_"),
    tmpdir = tempdir(),
    fileext = file_ext
  )

  result <- tryCatch({
    # Download with httr
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(180),
      httr::config(
        ssl_verifypeer = 0L,
        ssl_verifyhost = 0L,
        followlocation = TRUE
      )
    )

    if (httr::http_error(response)) {
      message(paste("  HTTP error for", level, ":", httr::status_code(response)))

      # Try fallback patterns
      df <- try_assessment_fallback_patterns(end_year, level)
      unlink(tname)
      return(df)
    }

    # Check file size
    file_info <- file.info(tname)
    if (is.na(file_info$size) || file_info$size < 1000) {
      message(paste("  Downloaded file too small for", level))
      unlink(tname)
      return(create_empty_assessment_raw())
    }

    # Read file based on type
    if (is_csv) {
      df <- readr::read_csv(tname, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
    } else {
      df <- readxl::read_excel(tname, col_types = "text")
    }

    unlink(tname)

    if (nrow(df) == 0) {
      message(paste("  Empty data for", level))
      return(create_empty_assessment_raw())
    }

    df

  }, error = function(e) {
    message(paste("  Download error for", level, ":", e$message))
    unlink(tname)
    create_empty_assessment_raw()
  })

  result
}


#' Try fallback URL patterns for assessment download
#'
#' Some years have multiple URL variations. This function tries alternative patterns.
#'
#' @param end_year School year end
#' @param level One of "state", "district", "school"
#' @return Data frame or empty data frame if all patterns fail
#' @keywords internal
try_assessment_fallback_patterns <- function(end_year, level) {

  base_url <- "https://www.tn.gov/content/dam/tn/education/accountability/"

  # Define fallback patterns to try
  patterns <- c(
    # Standard patterns
    paste0(end_year, "/", level, "_assessment_file_suppressed_", end_year, ".xlsx"),
    paste0(end_year, "/", level, "_assessment_file_suppressed_", end_year, ".csv"),
    paste0(end_year, "/", level, "_assessment_file_", end_year, ".xlsx"),
    paste0(end_year, "/", level, "_assessment_file_", end_year, ".csv"),
    # Without year suffix
    paste0(end_year, "/", level, "_assessment_file_suppressed.xlsx"),
    paste0(end_year, "/", level, "_assessment_file_suppressed.csv"),
    # Uppercase variations
    paste0(end_year, "/", toupper(substr(level, 1, 1)), substr(level, 2, nchar(level)),
           "_Assessment_File_Suppressed_", end_year, ".xlsx")
  )

  for (pattern in patterns) {
    url <- paste0(base_url, pattern)
    is_csv <- grepl("\\.csv$", url, ignore.case = TRUE)
    file_ext <- if (is_csv) ".csv" else ".xlsx"

    tname <- tempfile(fileext = file_ext)

    result <- tryCatch({
      response <- httr::GET(
        url,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(60),
        httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
      )

      if (!httr::http_error(response)) {
        file_info <- file.info(tname)
        if (!is.na(file_info$size) && file_info$size > 1000) {
          if (is_csv) {
            df <- readr::read_csv(tname, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
          } else {
            df <- readxl::read_excel(tname, col_types = "text")
          }
          unlink(tname)
          if (nrow(df) > 0) return(df)
        }
      }
      unlink(tname)
      NULL
    }, error = function(e) {
      unlink(tname)
      NULL
    })

    if (!is.null(result)) return(result)
  }

  create_empty_assessment_raw()
}


#' Create empty assessment raw data frame
#'
#' Returns an empty data frame with expected column structure for assessment data.
#'
#' @return Empty data frame with assessment columns
#' @keywords internal
create_empty_assessment_raw <- function() {
  data.frame(
    system = character(0),
    system_name = character(0),
    school = character(0),
    school_name = character(0),
    test = character(0),
    subject = character(0),
    grade = character(0),
    subgroup = character(0),
    valid_tests = character(0),
    pct_below = character(0),
    pct_approaching = character(0),
    pct_on_track = character(0),
    pct_mastered = character(0),
    pct_on_mastered = character(0),
    stringsAsFactors = FALSE
  )
}
