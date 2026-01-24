# Try fallback URL patterns for assessment download

Some years have multiple URL variations. This function tries alternative
patterns.

## Usage

``` r
try_assessment_fallback_patterns(end_year, level)
```

## Arguments

- end_year:

  School year end

- level:

  One of "state", "district", "school"

## Value

Data frame or empty data frame if all patterns fail
