# Fetch Tennessee assessment data

Downloads and processes assessment data from the Tennessee Department of
Education accountability portal.

## Usage

``` r
fetch_assessment(end_year, level = "all", tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  A school year. Year is the end of the academic year - eg 2023-24
  school year is year '2024'. Valid values are 2019, 2021-2025.

- level:

  Level of data to fetch: "all" (default), "state", "district", or
  "school"

- tidy:

  If TRUE (default), returns data in long (tidy) format with
  proficiency_level column. If FALSE, returns wide format with separate
  pct\_\* columns.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from TDOE.

## Value

Data frame with assessment data. Wide format includes columns for
proficiency percentages (pct_below, pct_approaching, pct_on_track,
pct_mastered). Tidy format pivots these into proficiency_level and pct
columns.

## Details

Data is available from 2019-2025, excluding 2020 (COVID-19 testing
waiver). Assessment data includes proficiency levels (Below,
Approaching, On Track, Mastered) for subjects including ELA, Math,
Science, Social Studies, and EOC courses.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 2024 assessment data (2023-24 school year)
assess_2024 <- fetch_assessment(2024)

# Get only state-level data
state_assess <- fetch_assessment(2024, level = "state")

# Get wide format (pct columns not pivoted)
assess_wide <- fetch_assessment(2024, tidy = FALSE)

# Force fresh download (ignore cache)
assess_fresh <- fetch_assessment(2024, use_cache = FALSE)

# Filter to math results
math_results <- assess_2024 |>
  dplyr::filter(subject == "Math", is_state)
} # }
```
