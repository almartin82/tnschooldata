# Process raw TDOE assessment data

Transforms raw TDOE assessment data into a standardized schema combining
state, district, and school data.

## Usage

``` r
process_assessment(raw_data, end_year)
```

## Arguments

- raw_data:

  List containing state, district, and/or school data frames from
  get_raw_assessment

- end_year:

  School year end

## Value

Processed data frame with standardized columns
