# Getting Started with tnschooldata

## Installation

Install from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("almartin82/tnschooldata")
```

## Quick Start

``` r
library(tnschooldata)
library(dplyr)
```

### Fetch enrollment data

The main function is
[`fetch_enr()`](https://almartin82.github.io/tnschooldata/reference/fetch_enr.md).
It downloads enrollment data from the Tennessee Department of Education
for a given school year.

``` r
# Get 2024 data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Check the structure
head(enr_2024)
```

### Data structure

The default output is in “tidy” (long) format with these key columns:

- `end_year`: School year end (e.g., 2024 for 2023-24)
- `type`: “State”, “District”, or “Campus”
- `district_id`: 4-digit district ID
- `campus_id`: 8-digit campus ID (district + school)
- `district_name`, `campus_name`: Names
- `grade_level`: “TOTAL”, “K”, “01”-“12”, or aggregates like “K8”, “HS”
- `subgroup`: “total_enrollment”, “white”, “black”, “hispanic”, etc.
- `n_students`: Student count
- `pct`: Percentage of total enrollment
- `is_state`, `is_district`, `is_campus`: Boolean flags

### Get state totals

``` r
enr_2024 %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, type, n_students)
```

### Get district totals

``` r
enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(10) %>%
  select(district_name, n_students)
```

### Get demographics

``` r
enr_2024 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))
```

### Multiple years

Use
[`fetch_enr_multi()`](https://almartin82.github.io/tnschooldata/reference/fetch_enr_multi.md)
to get data for multiple years:

``` r
enr_multi <- fetch_enr_multi(2019:2024)

# Track state enrollment over time
enr_multi %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students)
```

### Wide format

Use `tidy = FALSE` for wide format with one row per entity:

``` r
enr_wide <- fetch_enr(2024, tidy = FALSE)

head(enr_wide)
```

Wide format includes columns like: - `row_total`: Total enrollment -
`white`, `black`, `hispanic`, `asian`: Demographic counts - `grade_k`,
`grade_01`, …, `grade_12`: Grade-level enrollment - `econ_disadv`,
`lep`, `special_ed`: Special population counts

## Available Years

Tennessee enrollment data is available from 1999-2024:

``` r
get_available_years()
```

- **1999-2011**: Historical data from Annual Statistical Reports
  (district-level only)
- **2012-2024**: Modern data from TDOE portal (includes school-level)

## Caching

Data is automatically cached locally to speed up repeated requests:

``` r
# View cache status
cache_status()

# Force fresh download
enr_fresh <- fetch_enr(2024, use_cache = FALSE)

# Clear cache
clear_cache(2024, "tidy")
```

## Examples

### Find your district

``` r
enr_2024 %>%
  filter(is_district,
         subgroup == "total_enrollment",
         grade_level == "TOTAL",
         grepl("Knox", district_name))
```

### Compare districts

``` r
target_districts <- c("0470", "0792", "0850")  # Knox, Davidson, Shelby

enr_2024 %>%
  filter(is_district,
         district_id %in% target_districts,
         subgroup == "total_enrollment",
         grade_level == "TOTAL") %>%
  select(district_name, n_students)
```

### Track demographic changes

``` r
enr_multi <- fetch_enr_multi(c(2015, 2020, 2024))

enr_multi %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "hispanic") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))
```

## Session Info

``` r
sessionInfo()
```
