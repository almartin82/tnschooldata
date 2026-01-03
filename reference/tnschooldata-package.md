# tnschooldata: Fetch and Process Tennessee School Data

Downloads and processes school data from the Tennessee Department of
Education (TDOE). Provides functions for fetching enrollment data from
the Education Information System (EIS) and Report Card data,
transforming it into tidy format for analysis.

## Main functions

- [`fetch_enr`](https://almartin82.github.io/tnschooldata/reference/fetch_enr.md):

  Fetch enrollment data for a school year

- [`fetch_enr_multi`](https://almartin82.github.io/tnschooldata/reference/fetch_enr_multi.md):

  Fetch enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/tnschooldata/reference/tidy_enr.md):

  Transform wide data to tidy (long) format

- [`id_enr_aggs`](https://almartin82.github.io/tnschooldata/reference/id_enr_aggs.md):

  Add aggregation level flags

- [`enr_grade_aggs`](https://almartin82.github.io/tnschooldata/reference/enr_grade_aggs.md):

  Create grade-level aggregations

- [`get_available_years`](https://almartin82.github.io/tnschooldata/reference/get_available_years.md):

  Get available data years

## Cache functions

- [`cache_status`](https://almartin82.github.io/tnschooldata/reference/cache_status.md):

  View cached data files

- [`clear_cache`](https://almartin82.github.io/tnschooldata/reference/clear_cache.md):

  Remove cached data files

## ID System

Tennessee uses the following ID system:

- District IDs: 4 digits, zero-padded (e.g., 0470 = Knox County)

- School IDs: 4 digits within district

- Campus IDs: 8 digits (district ID + school ID)

## Data Sources

Data is sourced from the Tennessee Department of Education:

- Data Downloads:
  <https://www.tn.gov/education/districts/federal-programs-and-oversight/data/data-downloads.html>

- State Report Card: <https://tdepublicschools.ondemand.sas.com/>

- Annual Statistical Report:
  <https://www.tn.gov/education/districts/federal-programs-and-oversight/data/department-reports.html>

## Format Eras

Tennessee data is available in two main eras:

- Legacy Era (2012-2018): Older Excel format files

- Modern Era (2019-2024): Standardized Report Card data system

## Tennessee Education Regions

Tennessee is divided into 8 CORE (Centers of Regional Excellence)
regions:

- Region 1: Upper East Tennessee

- Region 2: East Tennessee

- Region 3: Southeast Tennessee

- Region 4: Upper Cumberland

- Region 5: Mid-Cumberland (Nashville area)

- Region 6: South Central Tennessee

- Region 7: Northwest Tennessee

- Region 8: Southwest Tennessee (Memphis area)

## See also

Useful links:

- <https://github.com/almartin82/tnschooldata>

- Report bugs at <https://github.com/almartin82/tnschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
