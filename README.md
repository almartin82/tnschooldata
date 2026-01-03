# tnschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/tnschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/tnschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/tnschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/tnschooldata/actions/workflows/python-test.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Fetch and analyze Tennessee school enrollment data from the Tennessee Department of Education (TDOE) in R or Python.

## Installation

You can install the development version of tnschooldata from GitHub:

```r
# install.packages("devtools")
devtools::install_github("almartin82/tnschooldata")
```
## Quick Start

### R

```r
library(tnschooldata)

# Get 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Get historical data from 2005 (2004-05 school year)
enr_2005 <- fetch_enr(2005)

# Get wide format (one row per school)
enr_wide <- fetch_enr(2024, tidy = FALSE)

# Get multiple years at once
enr_multi <- fetch_enr_multi(2020:2024)

# Get historical range (district-level only for pre-2012)
enr_historical <- fetch_enr_multi(1999:2005)

# Filter to specific district (Knox County)
knox <- enr_2024 %>%
  dplyr::filter(district_id == "0470")

# Get state totals
state_totals <- enr_2024 %>%
  dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
```

### Python

```python
import pytnschooldata as tn

# Fetch 2024 data (2023-24 school year)
enr = tn.fetch_enr(2024)

# Statewide total
total = enr[enr['is_state'] & (enr['grade_level'] == 'TOTAL')]['n_students'].sum()
print(f"{total:,} students")

# Get multiple years
enr_multi = tn.fetch_enr_multi([2020, 2021, 2022, 2023, 2024])

# Check available years
years = tn.get_available_years()
print(f"Data available: {years['min_year']}-{years['max_year']}")
```

## Data Availability

### Years Available

| Era | Years | Format | Notes |
|-----|-------|--------|-------|
| Modern | 2019-2024 | Excel/Report Card | Standardized format via State Report Card system |
| Legacy | 2012-2018 | Excel | Older file structures with varying column names |
| Historical | 1999-2011 | ASR ZIP/Excel | Annual Statistical Report archives (district-level only) |

**Total: 26 years of data (1999-2024)**

### Data Sources

1. **Data Downloads Portal**: https://www.tn.gov/education/districts/federal-programs-and-oversight/data/data-downloads.html
   - Membership files: Enrollment by grade, gender, race/ethnicity
   - Profile files: District/school names and demographics

2. **State Report Card**: https://tdepublicschools.ondemand.sas.com/
   - Interactive dashboard with downloadable data
   - Includes enrollment, demographics, achievement data

3. **Annual Statistical Report**: https://www.tn.gov/education/districts/federal-programs-and-oversight/data/department-reports.html
   - Comprehensive annual education statistics

### Aggregation Levels

- **State**: Tennessee statewide totals
- **District**: ~150 school districts (LEAs)
- **School/Campus**: ~1,800 public schools

### Demographics Available

| Category | Available | Notes |
|----------|-----------|-------|
| White | Yes | All years |
| Black/African American | Yes | All years |
| Hispanic/Latino | Yes | All years |
| Asian | Yes | All years |
| Pacific Islander | Yes | 2011+ (combined with Asian before) |
| American Indian/Alaska Native | Yes | All years |
| Multiracial/Two or More Races | Yes | 2011+ |
| Male/Female | Yes | All years |
| Economically Disadvantaged | Yes | All years |
| English Learners (LEP/EL) | Yes | All years |
| Special Education | Yes | All years |

### Grade Levels

- Pre-K (where offered)
- Kindergarten
- Grades 1-12

### Known Caveats

1. **Data Capture Date**: Enrollment data reflects October 1 membership counts
2. **Small Cell Suppression**: Values under 10 may be suppressed for privacy
3. **Charter Schools**: Reported as separate districts in some years
4. **Virtual Schools**: Included in district totals
5. **Pre-2011 Demographics**: Asian and Pacific Islander were combined
6. **Pre-2011 Multiracial**: Two or more races category not available
7. **Historical Era (1999-2011)**: Only district-level enrollment by grade is available (no school-level data, no demographics)
8. **Year 2000 Data**: The 1999-00 school year ASR file may experience download issues on some networks

## Tennessee ID System

Tennessee uses the following identifier system:

- **District ID**: 4 digits, zero-padded (e.g., `0470` = Knox County, `0791` = Shelby County)
- **School ID**: 4 digits within district
- **Campus ID**: 8 digits = District ID + School ID (e.g., `04700005`)

### Major Districts

| District ID | District Name | County |
|-------------|--------------|--------|
| 0791 | Shelby County Schools | Shelby |
| 0470 | Knox County Schools | Knox |
| 0190 | Davidson County (Metro Nashville) | Davidson |
| 0330 | Hamilton County | Hamilton |
| 0750 | Rutherford County | Rutherford |

## Tennessee Education Regions

Tennessee is organized into 8 CORE (Centers of Regional Excellence) regions:

| Region | Area | Major Cities |
|--------|------|--------------|
| 1 | Upper East Tennessee | Johnson City, Kingsport, Bristol |
| 2 | East Tennessee | Knoxville |
| 3 | Southeast Tennessee | Chattanooga |
| 4 | Upper Cumberland | Cookeville |
| 5 | Mid-Cumberland | Nashville |
| 6 | South Central Tennessee | Columbia, Murfreesboro |
| 7 | Northwest Tennessee | Jackson, Clarksville |
| 8 | Southwest Tennessee | Memphis |

## Output Schema

### Wide Format (`tidy = FALSE`)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end (2024 = 2023-24 school year) |
| type | character | "State", "District", or "Campus" |
| district_id | character | 4-digit district identifier |
| campus_id | character | 8-digit campus identifier (NA for districts) |
| district_name | character | District name |
| campus_name | character | Campus name (NA for districts) |
| county | character | County name |
| region | character | CORE region number |
| row_total | integer | Total enrollment |
| white, black, hispanic, asian, ... | integer | Demographic counts |
| male, female | integer | Gender counts |
| econ_disadv, lep, special_ed | integer | Special population counts |
| grade_pk through grade_12 | integer | Grade-level enrollment |

### Tidy Format (`tidy = TRUE`, default)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end |
| type | character | Aggregation level |
| district_id | character | District identifier |
| campus_id | character | Campus identifier |
| district_name | character | District name |
| campus_name | character | Campus name |
| grade_level | character | "TOTAL", "PK", "K", "01"-"12" |
| subgroup | character | "total_enrollment", "white", "black", etc. |
| n_students | integer | Student count |
| pct | numeric | Percentage of total (0-1 scale) |
| is_state | logical | TRUE if state-level row |
| is_district | logical | TRUE if district-level row |
| is_campus | logical | TRUE if campus-level row |

## Caching

Downloaded data is cached locally to avoid repeated downloads:

```r
# View cache status
cache_status()

# Clear all cached data
clear_cache()

# Clear specific year
clear_cache(2024)

# Force fresh download
enr_fresh <- fetch_enr(2024, use_cache = FALSE)
```

## Enrollment Visualizations

<img src="https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png" alt="Tennessee statewide enrollment trends" width="600">

<img src="https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png" alt="Top Tennessee districts" width="600">

See the [full vignette](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks.html) for more insights.

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data in Python and R.

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## License

MIT License
