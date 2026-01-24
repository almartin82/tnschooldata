# Get assessment data for a specific district

Convenience function to fetch assessment data for a single district.

## Usage

``` r
fetch_district_assessment(end_year, district_id, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  School year end

- district_id:

  4-digit district ID (e.g., "0470" for Knox County)

- tidy:

  If TRUE (default), returns tidy format

- use_cache:

  If TRUE (default), uses cached data

## Value

Data frame filtered to specified district

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Knox County (district 0470) assessment data
knox_assess <- fetch_district_assessment(2024, "0470")

# Get Metro Nashville (district 0190) data
metro_assess <- fetch_district_assessment(2024, "0190")
} # }
```
