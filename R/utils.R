# ==============================================================================
# Utility Functions
# ==============================================================================

#' Pipe operator
#'
#' See \code{dplyr::\link[dplyr:reexports]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Convert to numeric, handling suppression markers
#'
#' Tennessee DOE uses various markers for suppressed data (*, **, <5, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  # Handle NULL or empty

if (is.null(x) || length(x) == 0) {
    return(numeric(0))
  }

  # Convert to character if needed
  x <- as.character(x)

  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x[x %in% c("*", "**", "***", ".", "-", "-1", "<5", "<10", "N/A", "NA", "", "NULL")] <- NA_character_

  # Handle values like "< 5" with spaces
  x[grepl("^<\\s*\\d+$", x)] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Pad district ID to 4 digits
#'
#' Tennessee district IDs are 4 digits, zero-padded
#'
#' @param x Vector of district IDs
#' @return Character vector of zero-padded district IDs
#' @keywords internal
pad_district_id <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  # Remove any leading zeros and re-pad to 4 digits
  x <- gsub("^0+", "", x)
  sprintf("%04d", as.integer(x))
}


#' Pad school ID to 4 digits
#'
#' Tennessee school IDs within a district are 4 digits, zero-padded
#'
#' @param x Vector of school IDs
#' @return Character vector of zero-padded school IDs
#' @keywords internal
pad_school_id <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  # Remove any leading zeros and re-pad to 4 digits
  x <- gsub("^0+", "", x)
  sprintf("%04d", as.integer(x))
}


#' Create full campus ID from district and school IDs
#'
#' Tennessee campus IDs are 8 digits: 4-digit district ID + 4-digit school ID
#'
#' @param district_id District ID (will be padded to 4 digits)
#' @param school_id School ID (will be padded to 4 digits)
#' @return Character vector of 8-digit campus IDs
#' @keywords internal
make_campus_id <- function(district_id, school_id) {
  paste0(pad_district_id(district_id), pad_school_id(school_id))
}


#' Get available years for Tennessee enrollment data
#'
#' Returns the range of years for which enrollment data is available from TDOE.
#' Historical data (1999-2011) is sourced from the Annual Statistical Report (ASR)
#' Excel files. Modern data (2012+) uses the current TDOE data portal.
#'
#' @return Named list with min_year, max_year, years vector, and era boundaries
#' @export
#' @examples
#' get_available_years()
get_available_years <- function() {
  list(
    min_year = 1999,
    max_year = 2025,
    years = 1999:2025,
    # Era boundaries for different data formats
    asr_era = 1999:2011,  # Annual Statistical Report format
    modern_era = 2012:2025  # Modern TDOE data portal format
  )
}
