# Download enrollment data (ASR era: 1999-2011)

Downloads historical enrollment data from Tennessee's Annual Statistical
Report (ASR). ASR files are ZIP archives containing Excel tables with
district-level enrollment data.

## Usage

``` r
download_enrollment_asr(end_year)
```

## Arguments

- end_year:

  School year end (e.g., 2003 for 2002-03 school year)

## Value

List with school (empty) and district data frames

## Details

The key tables are:

- Table 7A: Average Daily Membership by grade (K-12)

- Table 8: Net Enrollment by grade (K-12)

Note: ASR data is district-level only; school-level data is not
available for these years.
