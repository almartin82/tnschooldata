# Check if cached directory data exists

Check if cached directory data exists

## Usage

``` r
cache_exists_directory(cache_type, max_age = 7)
```

## Arguments

- cache_type:

  Type of cache ("directory_tidy" or "directory_raw")

- max_age:

  Maximum age in days (default 7). Set to Inf to ignore age.

## Value

Logical indicating if valid cache exists
