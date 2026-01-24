# Identify assessment aggregation levels

Adds boolean flags to identify state, district, and school level
records.

## Usage

``` r
id_assessment_aggs(df)
```

## Arguments

- df:

  Assessment dataframe, output of tidy_assessment or process_assessment

## Value

data.frame with boolean aggregation flags

## Examples

``` r
if (FALSE) { # \dontrun{
tidy_data <- fetch_assessment(2024)
# Data already has aggregation flags via id_assessment_aggs
table(tidy_data$is_state, tidy_data$is_district, tidy_data$is_school)
} # }
```
