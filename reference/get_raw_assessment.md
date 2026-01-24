# Download raw assessment data from TDOE

Downloads state, district, and/or school assessment data from Tennessee
DOE's accountability portal.

## Usage

``` r
get_raw_assessment(end_year, level = "all")
```

## Arguments

- end_year:

  School year end (2019, 2021-2025; no 2020 due to COVID waiver)

- level:

  One of "all", "state", "district", "school"

## Value

List with state, district, and/or school data frames (depending on
level)
