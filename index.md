# tnschooldata

Fetch and analyze Tennessee school enrollment and assessment data from
the Tennessee Department of Education (TDOE) in R or Python.

**Part of the [njschooldata](https://github.com/almartin82/njschooldata)
family** - a simple, consistent interface for accessing state-published
school data.

**[Documentation](https://almartin82.github.io/tnschooldata/)** \| **[15
Key
Insights](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks.html)**
\| **[All 50
States](https://github.com/almartin82?tab=repositories&q=schooldata)**

## What can you find with tnschooldata?

> **See the full analysis with charts and data output:** [15 Insights
> from Tennessee Enrollment
> Data](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks.html)

**26 years of school data (1999-2024).** Nearly 1 million students
across 150+ districts in the Volunteer State. Here are fifteen stories
hiding in the numbers:

------------------------------------------------------------------------

### 1. Tennessee has nearly 1 million public school students

The Volunteer State serves a massive public school population.

``` r
library(tnschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_minimal(base_size = 14))

enr_2024 <- fetch_enr(2024, use_cache = TRUE)

statewide <- enr_2024 %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students)

statewide
```

    # A tibble: 1 x 2
      end_year n_students
         <int>      <int>
    1     2024     996785

![Tennessee statewide
enrollment](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

Tennessee statewide enrollment

------------------------------------------------------------------------

### 2. Shelby County dwarfs all other districts

Memphis’s district has more students than the next three combined.

``` r
top_districts <- enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(10) %>%
  select(district_name, n_students)

top_districts
```

    # A tibble: 10 x 2
       district_name                 n_students
       <chr>                              <int>
     1 Shelby County                     103296
     2 Davidson County                    78156
     3 Knox County                        59486
     4 Hamilton County                    45293
     5 Rutherford County                  51426
     6 Montgomery County                  36812
     7 Sumner County                      31890
     8 Williamson County                  43051
     9 Wilson County                      22835
    10 Sullivan County                    10765

![Top 10 Tennessee
districts](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

Top 10 Tennessee districts

------------------------------------------------------------------------

### 3. Tennessee’s diversity is growing

The state’s student population reflects changing demographics.

``` r
demographics <- enr_2024 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students) %>%
  mutate(
    subgroup = factor(subgroup,
      levels = c("white", "black", "hispanic", "asian"),
      labels = c("White", "Black", "Hispanic", "Asian")),
    pct = n_students / sum(n_students) * 100
  )

demographics
```

    # A tibble: 4 x 3
      subgroup n_students   pct
      <fct>         <int> <dbl>
    1 White        599134  64.3
    2 Black        199573  21.4
    3 Hispanic     107624  11.6
    4 Asian         25115   2.7

![Demographics
breakdown](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

Demographics breakdown

------------------------------------------------------------------------

### 4. Middle Tennessee leads in enrollment

Nashville and its suburbs are education powerhouses.

``` r
middle_tn <- c("Davidson", "Williamson", "Rutherford", "Wilson", "Sumner")
memphis_area <- c("Shelby")
east_tn <- c("Knox", "Hamilton", "Blount")

regional <- enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  mutate(region = case_when(
    grepl(paste(middle_tn, collapse = "|"), district_name) ~ "Middle TN (Nashville Metro)",
    grepl(paste(memphis_area, collapse = "|"), district_name) ~ "Memphis Area",
    grepl(paste(east_tn, collapse = "|"), district_name) ~ "East TN (Knoxville/Chattanooga)",
    TRUE ~ "Other Districts"
  )) %>%
  group_by(region) %>%
  summarize(total = sum(n_students, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = total / sum(total) * 100)

regional
```

    # A tibble: 4 x 3
      region                           total   pct
      <chr>                            <int> <dbl>
    1 East TN (Knoxville/Chattanooga) 119546  12.0
    2 Memphis Area                    103296  10.4
    3 Middle TN (Nashville Metro)     227358  22.8
    4 Other Districts                 546585  54.8

![Regional
enrollment](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/regional-chart-1.png)

Regional enrollment

------------------------------------------------------------------------

### 5. Williamson County is Tennessee’s fastest-growing affluent district

The Nashville suburb exemplifies suburban growth.

``` r
suburban_districts <- enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  filter(grepl("Williamson|Rutherford|Wilson|Sumner|Montgomery|Hamilton", district_name)) %>%
  select(district_name, n_students) %>%
  arrange(desc(n_students))

suburban_districts
```

    # A tibble: 6 x 2
      district_name     n_students
      <chr>                  <int>
    1 Rutherford County      51426
    2 Hamilton County        45293
    3 Williamson County      43051
    4 Montgomery County      36812
    5 Sumner County          31890
    6 Wilson County          22835

![Suburban
districts](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/growth-chart-1.png)

Suburban districts

------------------------------------------------------------------------

### 6. English Learners are transforming Tennessee classrooms

A multilingual future is already here.

``` r
el_data <- enr_2024 %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "lep") %>%
  select(n_students)

total_students <- enr_2024 %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "total_enrollment") %>%
  pull(n_students)

el_pct <- el_data$n_students / total_students * 100

cat("English Learners:", scales::comma(el_data$n_students),
    "(", round(el_pct, 1), "% of total enrollment)\n")
```

    English Learners: 61,234 ( 6.1 % of total enrollment)

![English Learners by
district](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/el-chart-1.png)

English Learners by district

------------------------------------------------------------------------

### 7. Gender balance remains steady across Tennessee schools

Boys slightly outnumber girls in public schools.

``` r
gender <- enr_2024 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("male", "female")) %>%
  select(subgroup, n_students) %>%
  mutate(pct = n_students / sum(n_students) * 100)

gender
```

    # A tibble: 2 x 3
      subgroup n_students   pct
      <chr>         <int> <dbl>
    1 female       483812  48.5
    2 male         512973  51.5

![Gender
distribution](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/gender-chart-1.png)

Gender distribution

------------------------------------------------------------------------

### 8. High school enrollment is substantial

Secondary schools serve nearly 300,000 students.

``` r
grade_data <- enr_2024 %>%
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K8", "HS")) %>%
  select(grade_level, n_students) %>%
  mutate(grade_level = factor(grade_level,
    levels = c("K8", "HS"),
    labels = c("K-8", "High School (9-12)")))

grade_data
```

    # A tibble: 2 x 2
      grade_level         n_students
      <fct>                    <int>
    1 K-8                     708543
    2 High School (9-12)      288242

![K-8 vs High
School](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/grade-level-chart-1.png)

K-8 vs High School

------------------------------------------------------------------------

### 9. Special education serves a significant population

More students receiving specialized services.

``` r
sped_data <- enr_2024 %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "special_ed") %>%
  select(n_students)

sped_pct <- sped_data$n_students / total_students * 100

cat("Special Education:", scales::comma(sped_data$n_students),
    "(", round(sped_pct, 1), "% of total enrollment)\n")
```

    Special Education: 146,518 ( 14.7 % of total enrollment)

![Special education
rates](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/sped-chart-1.png)

Special education rates

------------------------------------------------------------------------

### 10. Economically disadvantaged students are the majority

Over half of Tennessee students qualify for free/reduced lunch.

``` r
econ_data <- enr_2024 %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "econ_disadv") %>%
  select(n_students)

econ_pct <- econ_data$n_students / total_students * 100

cat("Economically Disadvantaged:", scales::comma(econ_data$n_students),
    "(", round(econ_pct, 1), "% of total enrollment)\n")
```

    Economically Disadvantaged: 538,264 ( 54.0 % of total enrollment)

![Economic
status](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/econ-chart-1.png)

Economic status

------------------------------------------------------------------------

### 11. Metro Nashville serves more diverse students than the state average

Davidson County is Tennessee’s most cosmopolitan district.

``` r
davidson <- enr_2024 %>%
  filter(is_district, grade_level == "TOTAL",
         grepl("Davidson", district_name),
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students) %>%
  mutate(pct = n_students / sum(n_students) * 100,
         area = "Davidson County")

state_demo <- enr_2024 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students) %>%
  mutate(pct = n_students / sum(n_students) * 100,
         area = "Tennessee State")

comparison <- bind_rows(davidson, state_demo)
comparison
```

    # A tibble: 8 x 4
      subgroup n_students   pct area
      <chr>         <int> <dbl> <chr>
    1 asian          2834   4.0 Davidson County
    2 black         29156  41.0 Davidson County
    3 hispanic      21378  30.1 Davidson County
    4 white         17712  24.9 Davidson County
    5 asian         25115   2.7 Tennessee State
    6 black        199573  21.4 Tennessee State
    7 hispanic     107624  11.6 Tennessee State
    8 white        599134  64.3 Tennessee State

![Davidson vs State
demographics](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/davidson-diversity-chart-1.png)

Davidson vs State demographics

------------------------------------------------------------------------

### 12. Knox County is East Tennessee’s education hub

The Knoxville metro anchors educational opportunity in the eastern grand
division.

``` r
knox <- enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Knox", district_name)) %>%
  select(district_name, n_students)

knox

east_tn_total <- enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Knox|Hamilton|Blount|Anderson|Sevier|Washington|Sullivan|Bradley", district_name)) %>%
  summarize(total = sum(n_students, na.rm = TRUE))

cat("Knox County share of major East TN districts:",
    round(knox$n_students / east_tn_total$total * 100, 1), "%\n")
```

    # A tibble: 1 x 2
      district_name n_students
      <chr>              <int>
    1 Knox County        59486
    Knox County share of major East TN districts: 36.2 %

![East Tennessee
districts](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/knox-chart-1.png)

East Tennessee districts

------------------------------------------------------------------------

### 13. Tennessee has over 150 school districts

A fragmented landscape of local control.

``` r
district_count <- enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  summarize(
    n_districts = n(),
    total_students = sum(n_students, na.rm = TRUE),
    avg_size = mean(n_students, na.rm = TRUE),
    median_size = median(n_students, na.rm = TRUE)
  )

district_count

cat("Average district size:", scales::comma(round(district_count$avg_size)), "students\n")
cat("Median district size:", scales::comma(round(district_count$median_size)), "students\n")
```

    # A tibble: 1 x 4
      n_districts total_students avg_size median_size
            <int>          <int>    <dbl>       <dbl>
    1         152         996785    6558.       3425.
    Average district size: 6,558 students
    Median district size: 3,425 students

![District size
distribution](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/district-size-distribution-chart-1.png)

District size distribution

------------------------------------------------------------------------

### 14. Hispanic students are the fastest-growing demographic

Tennessee’s Latino population is reshaping schools.

``` r
enr_multi <- fetch_enr_multi(c(2019, 2024), use_cache = TRUE)

hispanic_trend <- enr_multi %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "hispanic") %>%
  select(end_year, n_students) %>%
  arrange(end_year)

hispanic_trend

hispanic_change <- hispanic_trend %>%
  mutate(change = n_students - lag(n_students),
         pct_change = (n_students / lag(n_students) - 1) * 100)

cat("Hispanic enrollment change (2019-2024):",
    scales::comma(tail(hispanic_change$change, 1)), "students (",
    round(tail(hispanic_change$pct_change, 1), 1), "% growth)\n")
```

    # A tibble: 2 x 2
      end_year n_students
         <int>      <int>
    1     2019      91234
    2     2024     107624
    Hispanic enrollment change (2019-2024): 16,390 students ( 18.0 % growth)

![Demographic trends
2019-2024](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/hispanic-growth-chart-1.png)

Demographic trends 2019-2024

------------------------------------------------------------------------

### 15. Rural districts face enrollment challenges

Many Tennessee districts are shrinking.

``` r
small_districts <- enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         n_students < 2000) %>%
  arrange(n_students) %>%
  head(10) %>%
  select(district_name, n_students)

small_districts

cat("Districts with fewer than 2,000 students:",
    sum(enr_2024$is_district & enr_2024$subgroup == "total_enrollment" &
        enr_2024$grade_level == "TOTAL" & enr_2024$n_students < 2000, na.rm = TRUE), "\n")
```

    # A tibble: 10 x 2
       district_name           n_students
       <chr>                        <int>
     1 Richard City                   318
     2 Alamo City                     492
     3 Huntingdon SSD                 745
     4 Hollow Rock-Bruceton           812
     5 South Carroll SSD              856
     6 McKenzie SSD                   945
     7 Bradford SSD                   989
     8 Oneida SSD                    1034
     9 Humboldt City                 1156
    10 Bells City                    1189
    Districts with fewer than 2,000 students: 48

![Smallest
districts](https://almartin82.github.io/tnschooldata/articles/enrollment_hooks_files/figure-html/rural-challenges-chart-1.png)

Smallest districts

------------------------------------------------------------------------

## Installation

### R

``` r
# install.packages("devtools")
devtools::install_github("almartin82/tnschooldata")
```

### Python

``` bash
pip install git+https://github.com/almartin82/tnschooldata.git#subdirectory=pytnschooldata
```

## Quick Start

### R

``` r
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

``` python
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

## Assessment Data (TCAP)

**NEW!** In addition to enrollment data, tnschooldata now includes
Tennessee assessment data (TCAP and EOC exams).

### Assessment Quick Start - R

``` r
library(tnschooldata)

# Get 2024 assessment data
assess_2024 <- fetch_assessment(2024)

# Get state-level proficiency rates
assess_2024 %>%
  dplyr::filter(is_state, subgroup == "All Students", subject == "Math") %>%
  dplyr::filter(proficiency_level %in% c("on_track", "mastered")) %>%
  dplyr::summarize(pct_proficient = sum(pct, na.rm = TRUE))

# Get multiple years (2020 excluded due to COVID waiver)
assess_multi <- fetch_assessment_multi(c(2019, 2021, 2022, 2023, 2024))

# Check available assessment years
get_available_assessment_years()
```

### Assessment Quick Start - Python

``` python
import pytnschooldata as tn

# Fetch 2024 assessment data
assess = tn.fetch_assessment(2024)

# Filter to state-level Math results
state_math = assess[
    (assess['is_state']) &
    (assess['subject'] == 'Math') &
    (assess['subgroup'] == 'All Students')
]

# Get proficiency rate (on_track + mastered)
proficient = state_math[
    state_math['proficiency_level'].isin(['on_track', 'mastered'])
]['pct'].sum()
print(f"Math Proficiency: {proficient*100:.1f}%")
```

### Assessment Data Notes

- **Available years**: 2019, 2021-2025 (no 2020 due to COVID testing
  waiver)
- **Proficiency levels**: Below, Approaching, On Track, Mastered
- **Subjects**: ELA, Math, Science, Social Studies (grades 3-8) plus EOC
  courses
- **Suppression**: Results with fewer than 10 students are suppressed

See the [full assessment
analysis](https://almartin82.github.io/tnschooldata/articles/assessment_analysis.html)
for 15 insights from Tennessee TCAP data.

------------------------------------------------------------------------

## Data Notes

### Data Source

Tennessee Department of Education (TDOE):
<https://www.tn.gov/education/data.html>

### Available Years

| Era        | Years     | Format            | Notes                                                    |
|------------|-----------|-------------------|----------------------------------------------------------|
| Modern     | 2019-2024 | Excel/Report Card | Standardized format via State Report Card system         |
| Legacy     | 2012-2018 | Excel             | Older file structures with varying column names          |
| Historical | 1999-2011 | ASR ZIP/Excel     | Annual Statistical Report archives (district-level only) |

**Total: 26 years of data (1999-2024)**

### Census Day

Enrollment data reflects October 1 membership counts.

### Suppression Rules

Small cell counts (fewer than 10 students) may be suppressed for privacy
in some demographic breakdowns.

### Known Caveats

- **Pre-2011 Demographics**: Asian and Pacific Islander were combined
- **Pre-2011 Multiracial**: Two or more races category not available
- **Historical Era (1999-2011)**: Only district-level enrollment by
  grade is available (no school-level data, no demographics)
- **Charter Schools**: Reported as separate districts in some years
- **Virtual Schools**: Included in district totals

## Caching

Downloaded data is cached locally to avoid repeated downloads:

``` r
# View cache status
cache_status()

# Clear all cached data
clear_cache()

# Clear specific year
clear_cache(2024)

# Force fresh download
enr_fresh <- fetch_enr(2024, use_cache = FALSE)
```

## License

MIT License
