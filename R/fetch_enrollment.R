# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading enrollment data from the
# Tennessee Department of Education (TDOE) website.
#
# ==============================================================================

#' Fetch Tennessee enrollment data
#'
#' Downloads and processes enrollment data from the Tennessee Department of
#' Education data portal.
#'
#' Data is available from 1999-2024:
#' - 1999-2011: Historical data from Annual Statistical Report (ASR) - district-level only
#' - 2012-2024: Modern data portal - includes school-level data
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 1999-2024.
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from TDOE.
#' @return Data frame with enrollment data. Wide format includes columns for
#'   district_id, campus_id, names, and enrollment counts by demographic/grade.
#'   Tidy format pivots these counts into subgroup and grade_level columns.
#'   Note: Historical years (1999-2011) only include district-level data.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 enrollment data (2023-24 school year)
#' enr_2024 <- fetch_enr(2024)
#'
#' # Get historical data from 2005
#' enr_2005 <- fetch_enr(2005)
#'
#' # Get wide format
#' enr_wide <- fetch_enr(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2024, use_cache = FALSE)
#'
#' # Filter to specific district (Knox County)
#' knox_county <- enr_2024 |>
#'   dplyr::filter(district_id == "0470")
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available <- get_available_years()
  if (end_year < available$min_year || end_year > available$max_year) {
    stop(paste0(
      "end_year must be between ", available$min_year, " and ", available$max_year,
      ". Got: ", end_year
    ))
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached data for", end_year))
    return(read_cache(end_year, cache_type))
  }

  # Get raw data from TDOE
  raw <- get_raw_enr(end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) |>
      id_enr_aggs()
  }

  # If download produced empty/minimal data, try bundled fallback data.
  # For modern era (2012+), also check that campus-level data is present --

  # a partial download (district only) can exceed 100 rows but still be missing
  # all school-level records.
  needs_fallback <- nrow(processed) < 100
  if (!needs_fallback && end_year >= 2012) {
    has_campus <- if (tidy) {
      "is_campus" %in% names(processed) && any(processed$is_campus, na.rm = TRUE)
    } else {
      "type" %in% names(processed) && any(processed$type == "Campus", na.rm = TRUE)
    }
    if (!has_campus) {
      needs_fallback <- TRUE
      message(paste("Download for", end_year, "missing campus data, trying bundled fallback..."))
    }
  }

  if (needs_fallback) {
    bundled <- load_bundled_enr(end_year, cache_type)
    if (!is.null(bundled)) {
      message(paste("Using bundled data for", end_year))
      processed <- bundled
    }
  }

  # Cache the result (only if we got real data)
  if (use_cache && nrow(processed) >= 100) {
    write_cache(processed, end_year, cache_type)
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Downloads and combines enrollment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data
#' enr_multi <- fetch_enr_multi(2022:2024)
#'
#' # Track enrollment trends
#' enr_multi |>
#'   dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#'   dplyr::select(end_year, n_students)
#' }
fetch_enr_multi <- function(end_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years
  available <- get_available_years()
  invalid_years <- end_years[end_years < available$min_year | end_years > available$max_year]
  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "),
      "\nend_year must be between ", available$min_year, " and ", available$max_year
    ))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_enr(yr, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}


#' Load bundled enrollment data as fallback
#'
#' When TDOE is unreachable and no local cache exists, falls back to
#' bundled data included in the package. This ensures vignettes and
#' CI can always render.
#'
#' @param end_year School year end
#' @param cache_type "tidy" or "wide"
#' @return Data frame or NULL if no bundled data available for the year
#' @keywords internal
load_bundled_enr <- function(end_year, cache_type) {
  filename <- paste0("enr_", cache_type, "_", end_year, ".rds")
  bundled_path <- system.file("extdata", filename, package = "tnschooldata")

  if (bundled_path == "" || !file.exists(bundled_path)) {
    return(NULL)
  }

  readRDS(bundled_path)
}
