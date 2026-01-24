# Write assessment data to cache

Write assessment data to cache

## Usage

``` r
write_assessment_cache(df, end_year, type, level = "all")
```

## Arguments

- df:

  Data frame to cache

- end_year:

  School year end

- type:

  Data type ("tidy" or "wide")

- level:

  Optional level filter that was used ("all", "state", "district",
  "school")

## Value

Invisibly returns the cache path
