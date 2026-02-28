# Create empty enrollment data frame

Creates a 0-row data frame with proper column structure for enrollment
data. Used as a fallback when data download fails or is unavailable.

## Usage

``` r
create_empty_enrollment_frame(end_year, level)
```

## Arguments

- end_year:

  School year end

- level:

  "school" or "district"

## Value

Data frame with enrollment data
