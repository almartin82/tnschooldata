# Check if cached assessment data exists and is valid

Check if cached assessment data exists and is valid

## Usage

``` r
assessment_cache_exists(end_year, type, level = "all", max_age = 30)
```

## Arguments

- end_year:

  School year end

- type:

  Data type ("tidy" or "wide")

- level:

  Optional level filter that was used ("all", "state", "district",
  "school")

- max_age:

  Maximum age in days (default 30)

## Value

TRUE if valid cache exists
