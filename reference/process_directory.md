# Process raw school directory data to standard schema

Takes raw directory data from the TN School Directory API and
standardizes column names, extracts district/school IDs from the entity
name, and organizes contact information.

## Usage

``` r
process_directory(raw_data)
```

## Arguments

- raw_data:

  Raw data frame from get_raw_directory()

## Value

Processed data frame with standard schema
