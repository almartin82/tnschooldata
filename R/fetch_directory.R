# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# Tennessee Department of Education (TDOE) via the TN School Directory portal.
#
# Data source: https://tnschooldirectory.tnedu.gov/
# The directory uses a Microsoft Dynamics 365 / Power Pages backend with a
# FetchXML-based API endpoint at /_api/accounts.
#
# Entity types in the API:
# - customertypecode 100000001 = School
# - customertypecode 100000002 = District
# - customertypecode 100000003 = Region
#
# ==============================================================================

#' Fetch Tennessee school directory data
#'
#' Downloads and processes school directory data from the Tennessee Department
#' of Education's School Directory portal (tnschooldirectory.tnedu.gov).
#' Returns a combined dataset of schools and districts with contact information,
#' addresses, grade levels, and administrator names.
#'
#' @param end_year Currently unused. The directory represents the current state
#'   of Tennessee schools and is updated regularly by TDOE. Included for API
#'   consistency with other fetch functions.
#' @param tidy If TRUE (default), returns data in a standardized format with
#'   consistent column names. If FALSE, returns raw column names from the API.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from TDOE.
#' @return A tibble with school directory data. Columns include:
#'   \itemize{
#'     \item \code{entity_type}: "school" or "district"
#'     \item \code{state_district_id}: District number (from sde_entitynameunique)
#'     \item \code{state_school_id}: School number (accountnumber)
#'     \item \code{district_name}: Name of the school district
#'     \item \code{school_name}: Name of the school (NA for district rows)
#'     \item \code{school_type}: Public, Private, or Charter
#'     \item \code{instructional_type}: Regular, High School, etc.
#'     \item \code{address}: Street address
#'     \item \code{city}: City
#'     \item \code{state}: State (always "TN")
#'     \item \code{zip}: ZIP code
#'     \item \code{phone}: Phone number
#'     \item \code{county_name}: County name
#'     \item \code{grades_served}: Comma-separated grade levels
#'     \item \code{principal_name}: School principal name (from primary contact)
#'     \item \code{principal_email}: School principal email
#'     \item \code{superintendent_name}: District superintendent (from primary contact for districts)
#'     \item \code{superintendent_email}: District superintendent email
#'     \item \code{website}: School or district website URL
#'     \item \code{status}: Active or Inactive
#'     \item \code{nces_number}: NCES identification number
#'   }
#' @details
#' The directory data is downloaded from the TN School Directory portal, which
#' is powered by Microsoft Dynamics 365. The API returns JSON data via FetchXML
#' queries. Schools and districts are fetched separately and combined.
#'
#' The API paginates results in pages of up to 5000 records. For schools
#' (~3000 records) pagination is handled automatically.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get school directory data
#' dir_data <- fetch_directory()
#'
#' # Get raw format (original API field names)
#' dir_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Filter to active public schools
#' library(dplyr)
#' public_schools <- dir_data |>
#'   filter(entity_type == "school", school_type == "Public", status == "Active")
#'
#' # Find all schools in Davidson County (Nashville)
#' nashville <- dir_data |>
#'   filter(county_name == "Davidson", entity_type == "school")
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "directory_tidy" else "directory_raw"

  # Check cache first
  if (use_cache && cache_exists_directory(cache_type)) {
    message("Using cached school directory data")
    return(read_cache_directory(cache_type))
  }

  # Get raw data from TDOE School Directory API
  raw <- get_raw_directory()

  # Process to standard schema
  if (tidy) {
    result <- process_directory(raw)
  } else {
    result <- raw
  }

  # Cache the result
  if (use_cache && nrow(result) > 0) {
    write_cache_directory(result, cache_type)
  }

  result
}


#' Get raw school directory data from TDOE
#'
#' Downloads raw school and district directory data from the Tennessee
#' School Directory portal API. This queries the FetchXML endpoint at
#' tnschooldirectory.tnedu.gov for both schools and districts, including
#' administrator contact information via linked contact records.
#'
#' @return Combined data frame of schools and districts as returned by the API
#' @keywords internal
get_raw_directory <- function() {

  message("Downloading school directory data from TN School Directory...")

  # Fetch schools (customertypecode = 100000001)
  message("  Fetching school records...")
  schools <- fetch_directory_entities(entity_type = "100000001")
  message(paste("  Retrieved", nrow(schools), "school records"))

  # Fetch districts (customertypecode = 100000002)
  message("  Fetching district records...")
  districts <- fetch_directory_entities(entity_type = "100000002")
  message(paste("  Retrieved", nrow(districts), "district records"))

  # Add entity type marker before combining
  if (nrow(schools) > 0) schools$entity_type_code <- "school"
  if (nrow(districts) > 0) districts$entity_type_code <- "district"

  # Combine
  result <- dplyr::bind_rows(schools, districts)

  message(paste("  Total:", nrow(result), "records"))

  result
}


#' Fetch directory entities from the TN School Directory API
#'
#' Queries the FetchXML endpoint for a specific entity type (school or district)
#' with pagination support. Includes a linked join to the contact entity to
#' retrieve administrator names and emails.
#'
#' @param entity_type The customertypecode value: "100000001" for schools,
#'   "100000002" for districts
#' @param page_size Number of records per page (max 5000)
#' @return Data frame with all records for the entity type
#' @keywords internal
fetch_directory_entities <- function(entity_type, page_size = 5000) {

  base_url <- "https://tnschooldirectory.tnedu.gov/_api/accounts"

  all_records <- list()
  page_number <- 1
  has_more <- TRUE
  paging_cookie <- NULL

  while (has_more) {
    # Build FetchXML query
    fetch_xml <- build_directory_fetchxml(
      entity_type = entity_type,
      page_size = page_size,
      page_number = page_number,
      paging_cookie = paging_cookie
    )

    # URL-encode the FetchXML
    encoded_xml <- utils::URLencode(fetch_xml, reserved = TRUE)
    url <- paste0(base_url, "?fetchXml=", encoded_xml)

    # Make the request
    response <- tryCatch({
      httr::GET(
        url,
        httr::timeout(120),
        httr::add_headers(
          "Accept" = "application/json"
        )
      )
    }, error = function(e) {
      stop(paste("Failed to connect to TN School Directory:", e$message))
    })

    if (httr::http_error(response)) {
      stop(paste(
        "TN School Directory API error:",
        httr::status_code(response),
        httr::content(response, as = "text", encoding = "UTF-8")
      ))
    }

    # Parse JSON response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content, flatten = TRUE)

    records <- data$value
    if (is.null(records) || length(records) == 0 || nrow(records) == 0) {
      break
    }

    all_records[[page_number]] <- records

    # Check for more pages
    has_more <- isTRUE(data[["@Microsoft.Dynamics.CRM.morerecords"]])

    if (has_more) {
      paging_cookie <- data[["@Microsoft.Dynamics.CRM.fetchxmlpagingcookie"]]
      page_number <- page_number + 1
    }
  }

  if (length(all_records) == 0) {
    return(dplyr::tibble())
  }

  # Combine all pages
  result <- dplyr::bind_rows(all_records)

  # Convert to tibble
  dplyr::as_tibble(result)
}


#' Build FetchXML query for directory data
#'
#' Constructs a FetchXML query string for the TN School Directory API.
#' Includes attributes for school/district details and a linked entity
#' join to get primary contact (principal/superintendent) information.
#'
#' @param entity_type The customertypecode value
#' @param page_size Number of records per page
#' @param page_number Current page number
#' @param paging_cookie Paging cookie from previous response (NULL for first page)
#' @return FetchXML string
#' @keywords internal
build_directory_fetchxml <- function(entity_type, page_size = 5000,
                                     page_number = 1, paging_cookie = NULL) {

  # Build paging attributes
  paging_attr <- ""
  if (!is.null(paging_cookie)) {
    # Extract just the paging-cookie attribute value from the cookie XML
    # The cookie comes as: <cookie pagenumber="2" pagingcookie="..." istracking="False" />
    cookie_match <- regmatches(
      paging_cookie,
      regexpr('pagingcookie="([^"]*)"', paging_cookie)
    )
    if (length(cookie_match) > 0) {
      cookie_val <- sub('pagingcookie="(.*)"', "\\1", cookie_match)
      # URL-decode the cookie value (it's double-encoded)
      cookie_val <- utils::URLdecode(cookie_val)
      paging_attr <- paste0(
        ' page="', page_number, '"',
        ' paging-cookie="', cookie_val, '"'
      )
    }
  }

  # Build the FetchXML
  fetch_xml <- paste0(
    '<fetch mapping="logical" count="', page_size, '"', paging_attr, '>',
    '<entity name="account">',
    # Core fields
    '<attribute name="name" />',
    '<attribute name="accountnumber" />',
    '<attribute name="statecode" />',
    '<attribute name="customertypecode" />',
    # Address fields
    '<attribute name="address1_line1" />',
    '<attribute name="address1_line2" />',
    '<attribute name="address1_city" />',
    '<attribute name="address1_stateorprovince" />',
    '<attribute name="address1_postalcode" />',
    '<attribute name="address1_composite" />',
    # Contact info
    '<attribute name="telephone1" />',
    '<attribute name="emailaddress1" />',
    '<attribute name="websiteurl" />',
    # TN DOE custom fields
    '<attribute name="sde_county" />',
    '<attribute name="sde_gradesummary" />',
    '<attribute name="sde_schooldistrictname" />',
    '<attribute name="sde_schooldistrictnumber" />',
    '<attribute name="sde_instructionaltype" />',
    '<attribute name="sde_categoriessummary" />',
    '<attribute name="sde_entitynameunique" />',
    '<attribute name="sde_ncesnumber" />',
    '<attribute name="sde_districttype" />',
    '<attribute name="mshied_schooltype" />',
    # Linked contact (principal/superintendent)
    '<link-entity name="contact" from="contactid" to="primarycontactid" ',
    'link-type="outer" alias="contact">',
    '<attribute name="fullname" />',
    '<attribute name="emailaddress1" />',
    '<attribute name="jobtitle" />',
    '</link-entity>',
    # Filter by entity type and active status
    '<filter type="and">',
    '<condition attribute="customertypecode" operator="eq" value="', entity_type, '" />',
    '<condition attribute="statecode" operator="eq" value="0" />',
    '</filter>',
    '</entity>',
    '</fetch>'
  )

  fetch_xml
}


#' Process raw school directory data to standard schema
#'
#' Takes raw directory data from the TN School Directory API and standardizes
#' column names, extracts district/school IDs from the entity name, and
#' organizes contact information.
#'
#' @param raw_data Raw data frame from get_raw_directory()
#' @return Processed data frame with standard schema
#' @keywords internal
process_directory <- function(raw_data) {

  if (nrow(raw_data) == 0) {
    return(dplyr::tibble(
      entity_type = character(0),
      state_district_id = character(0),
      state_school_id = character(0),
      district_name = character(0),
      school_name = character(0),
      school_type = character(0),
      instructional_type = character(0),
      address = character(0),
      city = character(0),
      state = character(0),
      zip = character(0),
      phone = character(0),
      county_name = character(0),
      grades_served = character(0),
      principal_name = character(0),
      principal_email = character(0),
      superintendent_name = character(0),
      superintendent_email = character(0),
      website = character(0),
      status = character(0),
      nces_number = character(0),
      categories = character(0),
      district_type = character(0)
    ))
  }

  # Helper to safely extract a column
  safe_col <- function(df, col_name, formatted = NULL) {
    # Try formatted value first (e.g., for option set fields)
    if (!is.null(formatted) && formatted %in% names(df)) {
      return(as.character(df[[formatted]]))
    }
    if (col_name %in% names(df)) {
      return(as.character(df[[col_name]]))
    }
    rep(NA_character_, nrow(df))
  }

  # Extract district/school IDs from sde_entitynameunique

  # Format: "School|{school_num}|District|{district_num}" or "District|{district_num}"
  entity_unique <- safe_col(raw_data, "sde_entitynameunique")

  district_ids <- vapply(entity_unique, function(x) {
    if (is.na(x)) return(NA_character_)
    parts <- strsplit(x, "\\|")[[1]]
    dist_idx <- which(parts == "District")
    if (length(dist_idx) > 0 && dist_idx[1] < length(parts)) {
      return(parts[dist_idx[1] + 1])
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  school_ids <- vapply(entity_unique, function(x) {
    if (is.na(x)) return(NA_character_)
    parts <- strsplit(x, "\\|")[[1]]
    school_idx <- which(parts == "School")
    if (length(school_idx) > 0 && school_idx[1] < length(parts)) {
      return(parts[school_idx[1] + 1])
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  # Determine entity type
  entity_type <- safe_col(raw_data, "entity_type_code")

  # Extract contact name and email
  contact_name <- safe_col(raw_data, "contact.fullname")
  contact_email <- safe_col(raw_data, "contact.emailaddress1")

  # Build result
  result <- dplyr::tibble(
    entity_type = entity_type,
    state_district_id = district_ids,
    state_school_id = dplyr::case_when(
      entity_type == "school" ~ safe_col(raw_data, "accountnumber"),
      TRUE ~ NA_character_
    ),
    district_name = safe_col(raw_data, "sde_schooldistrictname"),
    school_name = dplyr::case_when(
      entity_type == "school" ~ extract_school_name(safe_col(raw_data, "name")),
      TRUE ~ NA_character_
    ),
    school_type = safe_col(
      raw_data,
      "mshied_schooltype",
      "mshied_schooltype@OData.Community.Display.V1.FormattedValue"
    ),
    instructional_type = safe_col(
      raw_data,
      "sde_instructionaltype",
      "sde_instructionaltype@OData.Community.Display.V1.FormattedValue"
    ),
    address = safe_col(raw_data, "address1_line1"),
    city = safe_col(raw_data, "address1_city"),
    state = dplyr::coalesce(
      safe_col(raw_data, "address1_stateorprovince"),
      "TN"
    ),
    zip = safe_col(raw_data, "address1_postalcode"),
    phone = safe_col(raw_data, "telephone1"),
    county_name = safe_col(
      raw_data,
      "sde_county",
      "sde_county@OData.Community.Display.V1.FormattedValue"
    ),
    grades_served = safe_col(raw_data, "sde_gradesummary"),
    # For schools, primary contact is typically the principal
    principal_name = dplyr::case_when(
      entity_type == "school" ~ contact_name,
      TRUE ~ NA_character_
    ),
    principal_email = dplyr::case_when(
      entity_type == "school" ~ contact_email,
      TRUE ~ NA_character_
    ),
    # For districts, primary contact is typically the superintendent
    superintendent_name = dplyr::case_when(
      entity_type == "district" ~ contact_name,
      TRUE ~ NA_character_
    ),
    superintendent_email = dplyr::case_when(
      entity_type == "district" ~ contact_email,
      TRUE ~ NA_character_
    ),
    website = safe_col(raw_data, "websiteurl"),
    status = safe_col(
      raw_data,
      "statecode",
      "statecode@OData.Community.Display.V1.FormattedValue"
    ),
    nces_number = safe_col(raw_data, "sde_ncesnumber"),
    categories = safe_col(raw_data, "sde_categoriessummary"),
    district_type = safe_col(
      raw_data,
      "sde_districttype",
      "sde_districttype@OData.Community.Display.V1.FormattedValue"
    )
  )

  # Normalize whitespace in address fields (API sometimes has double spaces)
  result$address <- gsub("\\s+", " ", trimws(result$address))
  result$school_name <- trimws(result$school_name)
  result$district_name <- trimws(result$district_name)

  result
}


#' Extract school name from API name field
#'
#' The API returns school names in the format "School Name - Number".
#' This function extracts just the school name portion.
#'
#' @param name_field Character vector of name values from the API
#' @return Character vector of cleaned school names
#' @keywords internal
extract_school_name <- function(name_field) {
  # Remove trailing " - NNNN" pattern (school number suffix)
  cleaned <- gsub("\\s*-\\s*\\d+\\s*$", "", name_field)
  trimws(cleaned)
}


# ==============================================================================
# Directory-specific cache functions
# ==============================================================================

#' Build cache file path for directory data
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return File path string
#' @keywords internal
build_cache_path_directory <- function(cache_type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(cache_type, ".rds"))
}


#' Check if cached directory data exists
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @param max_age Maximum age in days (default 7). Set to Inf to ignore age.
#' @return Logical indicating if valid cache exists
#' @keywords internal
cache_exists_directory <- function(cache_type, max_age = 7) {
  cache_path <- build_cache_path_directory(cache_type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Cached data frame
#' @keywords internal
read_cache_directory <- function(cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data frame to cache
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Invisibly returns the cache path
#' @keywords internal
write_cache_directory <- function(data, cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  cache_dir <- dirname(cache_path)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  saveRDS(data, cache_path)
  invisible(cache_path)
}


#' Clear school directory cache
#'
#' Removes cached school directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear cached directory data
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
