# Process raw TDOE enrollment data

Transforms raw TDOE data into a standardized schema combining school and
district data.

## Usage

``` r
process_enr(raw_data, end_year)
```

## Arguments

- raw_data:

  List containing school and district data frames from get_raw_enr

- end_year:

  School year end

## Value

Processed data frame with standardized columns
