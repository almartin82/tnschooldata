# tnschooldata

<!-- badges: start -->
<!-- badges: end -->

An R package for fetching and processing school enrollment data from the Tennessee Department of Education (TDOE).

## Installation

You can install the development version of tnschooldata from GitHub:

```r
# install.packages("devtools")
devtools::install_github("almartin82/tnschooldata")
```
## Quick Start

```r
library(tnschooldata)

# Get 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Get wide format (one row per school)
enr_wide <- fetch_enr(2024, tidy = FALSE)

# Get multiple years at once
enr_multi <- fetch_enr_multi(2020:2024)

# Filter to specific district (Knox County)
knox <- enr_2024 %>%
  dplyr::filter(district_id == "0470")

# Get state totals
state_totals <- enr_2024 %>%
  dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
```

## Data Availability

### Years Available

| Era | Years | Format | Notes |
|-----|-------|--------|-------|
| Modern | 2019-2025 | Excel/Report Card | Standardized format via State Report Card system |
| Legacy | 2012-2018 | Excel | Older file structures with varying column names |

**Total: 14 years of data (2012-2025)**

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

## Related Packages

This package is part of the state schooldata family:

- [txschooldata](https://github.com/almartin82/txschooldata) - Texas
- [caschooldata](https://github.com/almartin82/caschooldata) - California
- [nyschooldata](https://github.com/almartin82/nyschooldata) - New York
- [ilschooldata](https://github.com/almartin82/ilschooldata) - Illinois
- [paschooldata](https://github.com/almartin82/paschooldata) - Pennsylvania
- [ohschooldata](https://github.com/almartin82/ohschooldata) - Ohio

## License

MIT License
