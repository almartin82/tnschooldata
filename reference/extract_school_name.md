# Extract school name from API name field

The API returns school names in the format "School Name - Number". This
function extracts just the school name portion.

## Usage

``` r
extract_school_name(name_field)
```

## Arguments

- name_field:

  Character vector of name values from the API

## Value

Character vector of cleaned school names
