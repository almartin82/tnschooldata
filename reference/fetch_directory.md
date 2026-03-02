# Fetch Tennessee school directory data

Downloads and processes school directory data from the Tennessee
Department of Education's School Directory portal
(tnschooldirectory.tnedu.gov). Returns a combined dataset of schools and
districts with contact information, addresses, grade levels, and
administrator names.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  Currently unused. The directory represents the current state of
  Tennessee schools and is updated regularly by TDOE. Included for API
  consistency with other fetch functions.

- tidy:

  If TRUE (default), returns data in a standardized format with
  consistent column names. If FALSE, returns raw column names from the
  API.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from TDOE.

## Value

A tibble with school directory data. Columns include:

- `entity_type`: "school" or "district"

- `state_district_id`: District number (from sde_entitynameunique)

- `state_school_id`: School number (accountnumber)

- `district_name`: Name of the school district

- `school_name`: Name of the school (NA for district rows)

- `school_type`: Public, Private, or Charter

- `instructional_type`: Regular, High School, etc.

- `address`: Street address

- `city`: City

- `state`: State (always "TN")

- `zip`: ZIP code

- `phone`: Phone number

- `county_name`: County name

- `grades_served`: Comma-separated grade levels

- `principal_name`: School principal name (from primary contact)

- `principal_email`: School principal email

- `superintendent_name`: District superintendent (from primary contact
  for districts)

- `superintendent_email`: District superintendent email

- `website`: School or district website URL

- `status`: Active or Inactive

- `nces_number`: NCES identification number

## Details

The directory data is downloaded from the TN School Directory portal,
which is powered by Microsoft Dynamics 365. The API returns JSON data
via FetchXML queries. Schools and districts are fetched separately and
combined.

The API paginates results in pages of up to 5000 records. For schools
(~3000 records) pagination is handled automatically.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get school directory data
dir_data <- fetch_directory()

# Get raw format (original API field names)
dir_raw <- fetch_directory(tidy = FALSE)

# Force fresh download (ignore cache)
dir_fresh <- fetch_directory(use_cache = FALSE)

# Filter to active public schools
library(dplyr)
public_schools <- dir_data |>
  filter(entity_type == "school", school_type == "Public", status == "Active")

# Find all schools in Davidson County (Nashville)
nashville <- dir_data |>
  filter(county_name == "Davidson", entity_type == "school")
} # }
```
