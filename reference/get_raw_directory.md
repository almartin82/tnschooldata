# Get raw school directory data from TDOE

Downloads raw school and district directory data from the Tennessee
School Directory portal API. This queries the FetchXML endpoint at
tnschooldirectory.tnedu.gov for both schools and districts, including
administrator contact information via linked contact records.

## Usage

``` r
get_raw_directory()
```

## Value

Combined data frame of schools and districts as returned by the API
