# Process ASR enrollment table into standard format

Transforms the raw ASR table format into a standardized data frame. ASR
tables typically have:

- Header rows with column labels (K, 1ST, 2ND, ... 12TH, TOTAL)

- Data rows with district name in first column, grade counts in
  remaining columns

## Usage

``` r
process_asr_enrollment_table(df, end_year)
```

## Arguments

- df:

  Raw data frame from Excel read

- end_year:

  School year end

## Value

Processed data frame with standardized columns
