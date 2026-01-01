# Create full campus ID from district and school IDs

Tennessee campus IDs are 8 digits: 4-digit district ID + 4-digit school
ID

## Usage

``` r
make_campus_id(district_id, school_id)
```

## Arguments

- district_id:

  District ID (will be padded to 4 digits)

- school_id:

  School ID (will be padded to 4 digits)

## Value

Character vector of 8-digit campus IDs
