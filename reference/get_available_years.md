# Get available years for Tennessee enrollment data

Returns the range of years for which enrollment data is available from
TDOE. Historical data (1999-2011) is sourced from the Annual Statistical
Report (ASR) Excel files. Modern data (2012+) uses the current TDOE data
portal.

## Usage

``` r
get_available_years()
```

## Value

Named list with min_year, max_year, years vector, and era boundaries

## Examples

``` r
get_available_years()
#> $min_year
#> [1] 1999
#> 
#> $max_year
#> [1] 2025
#> 
#> $years
#>  [1] 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013
#> [16] 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025
#> 
#> $asr_era
#>  [1] 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
#> 
#> $modern_era
#>  [1] 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025
#> 
```
