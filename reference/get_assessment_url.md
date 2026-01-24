# Get assessment URL for a given year and level

Constructs the URL for downloading assessment data from TDOE's
accountability portal. URLs have irregular patterns that vary by year,
so we use a lookup table.

## Usage

``` r
get_assessment_url(end_year, level)
```

## Arguments

- end_year:

  School year end

- level:

  One of "state", "district", "school"

## Value

URL string or NULL if not found
