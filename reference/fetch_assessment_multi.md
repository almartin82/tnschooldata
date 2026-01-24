# Fetch assessment data for multiple years

Downloads and combines assessment data for multiple school years.

## Usage

``` r
fetch_assessment_multi(end_years, level = "all", tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_years:

  Vector of school year ends (e.g., c(2022, 2023, 2024)) Note: 2020 is
  not available due to COVID-19 testing waiver.

- level:

  Level of data to fetch: "all" (default), "state", "district", or
  "school"

- tidy:

  If TRUE (default), returns data in long (tidy) format.

- use_cache:

  If TRUE (default), uses locally cached data when available.

## Value

Combined data frame with assessment data for all requested years

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 3 years of data (excluding 2020)
assess_multi <- fetch_assessment_multi(c(2021, 2022, 2023))

# Track proficiency trends at state level
assess_multi |>
  dplyr::filter(is_state, subject == "Math", grade == "All", subgroup == "All Students") |>
  dplyr::filter(proficiency_level %in% c("on_track", "mastered")) |>
  dplyr::group_by(end_year) |>
  dplyr::summarize(pct_proficient = sum(pct, na.rm = TRUE))
} # }
```
