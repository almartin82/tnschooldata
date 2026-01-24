#' tnschooldata: Fetch and Process Tennessee School Data
#'
#' Downloads and processes school data from the Tennessee Department of Education
#' (TDOE). Provides functions for fetching enrollment data from the Education
#' Information System (EIS) and Report Card data, transforming it into tidy
#' format for analysis.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fetch_enr}}}{Fetch enrollment data for a school year}
#'   \item{\code{\link{fetch_enr_multi}}}{Fetch enrollment data for multiple years}
#'   \item{\code{\link{tidy_enr}}}{Transform wide data to tidy (long) format}
#'   \item{\code{\link{id_enr_aggs}}}{Add aggregation level flags}
#'   \item{\code{\link{enr_grade_aggs}}}{Create grade-level aggregations}
#'   \item{\code{\link{get_available_years}}}{Get available data years}
#' }
#'
#' @section Cache functions:
#' \describe{
#'   \item{\code{\link{cache_status}}}{View cached data files}
#'   \item{\code{\link{clear_cache}}}{Remove cached data files}
#' }
#'
#' @section ID System:
#' Tennessee uses the following ID system:
#' \itemize{
#'   \item District IDs: 4 digits, zero-padded (e.g., 0470 = Knox County)
#'   \item School IDs: 4 digits within district
#'   \item Campus IDs: 8 digits (district ID + school ID)
#' }
#'
#' @section Data Sources:
#' Data is sourced from the Tennessee Department of Education:
#' \itemize{
#'   \item Data Downloads: \url{https://www.tn.gov/education/districts/federal-programs-and-oversight/data/data-downloads.html}
#'   \item State Report Card: \url{https://tdepublicschools.ondemand.sas.com/}
#'   \item Annual Statistical Report: \url{https://www.tn.gov/education/districts/federal-programs-and-oversight/data/department-reports.html}
#' }
#'
#' @section Format Eras:
#' Tennessee data is available in two main eras:
#' \itemize{
#'   \item Legacy Era (2012-2018): Older Excel format files
#'   \item Modern Era (2019-2024): Standardized Report Card data system
#' }
#'
#' @section Tennessee Education Regions:
#' Tennessee is divided into 8 CORE (Centers of Regional Excellence) regions:
#' \itemize{
#'   \item Region 1: Upper East Tennessee
#'   \item Region 2: East Tennessee
#'   \item Region 3: Southeast Tennessee
#'   \item Region 4: Upper Cumberland
#'   \item Region 5: Mid-Cumberland (Nashville area)
#'   \item Region 6: South Central Tennessee
#'   \item Region 7: Northwest Tennessee
#'   \item Region 8: Southwest Tennessee (Memphis area)
#' }
#'
#' @docType package
#' @name tnschooldata-package
#' @aliases tnschooldata
#' @keywords internal
"_PACKAGE"

# Global variables to avoid R CMD check notes
utils::globalVariables(c(
  "grade_level", "n_students", "n_tested", "pct", "proficiency_level",
  "row_total", "subgroup", "type"
))
