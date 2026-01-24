# Calculate proficiency rates (on_track + mastered)

Calculates the combined proficiency rate (students scoring on_track or
mastered) for each row or group in the assessment data.

## Usage

``` r
calc_proficiency(df, ...)
```

## Arguments

- df:

  Tidy assessment data frame (must have proficiency_level column)

- ...:

  Optional grouping variables (unquoted)

## Value

Data frame with proficiency rates

## Examples

``` r
if (FALSE) { # \dontrun{
tidy_data <- fetch_assessment(2024)
# Overall proficiency by state/district/school
calc_proficiency(tidy_data)

# Proficiency by subject
calc_proficiency(tidy_data, subject)
} # }
```
