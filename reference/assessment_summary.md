# Calculate proficiency summary statistics

Creates a summary table with proficiency rates aggregated by specified
grouping.

## Usage

``` r
assessment_summary(df, ...)
```

## Arguments

- df:

  Tidy assessment data frame

- ...:

  Grouping variables (unquoted)

## Value

Summary data frame with proficiency statistics

## Examples

``` r
if (FALSE) { # \dontrun{
tidy_data <- fetch_assessment(2024)
# Proficiency by subject and grade
assessment_summary(tidy_data, subject, grade)

# Proficiency by subgroup
assessment_summary(tidy_data, subgroup)
} # }
```
