# Tennessee Assessment Data Analysis

## Overview

Tennessee’s TCAP (Tennessee Comprehensive Assessment Program) tests
students in grades 3-8 on ELA, Math, Science, and Social Studies. High
school students take End-of-Course (EOC) exams. Results are reported
across four proficiency levels:

- **Below**: Student has not met grade-level expectations
- **Approaching**: Student is approaching grade-level expectations
- **On Track**: Student has met grade-level expectations
- **Mastered**: Student has exceeded grade-level expectations

Students scoring “On Track” or “Mastered” are considered proficient.

``` r
library(tnschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)
```

## Quick Start

``` r
# Fetch 2024 state assessment data
assess_2024 <- fetch_assessment(2024, level = "state", use_cache = TRUE)

# Check structure
glimpse(assess_2024)
```

------------------------------------------------------------------------

## Story 1: Statewide Proficiency Overview

Tennessee students showed steady improvement in proficiency rates across
all subjects in 2024.

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", grade == "All" | grade == "ALL") |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject) |>
  summarize(
    n_tested = first(n_tested),
    pct_proficient = sum(pct, na.rm = TRUE)
  ) |>
  mutate(pct_proficient = round(pct_proficient * 100, 1)) |>
  arrange(desc(pct_proficient))
```

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", grade == "All" | grade == "ALL") |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(subject, pct_proficient), y = pct_proficient)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(pct_proficient * 100, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  labs(
    title = "Tennessee State Proficiency Rates by Subject (2024)",
    subtitle = "Percent of students scoring On Track or Mastered",
    x = NULL, y = "Proficiency Rate"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 2: Math vs ELA Achievement Gap

Math proficiency lags behind ELA across Tennessee, a pattern consistent
with national trends.

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students") |>
  filter(subject %in% c("Math", "ELA")) |>
  filter(grade %in% c("03", "04", "05", "06", "07", "08")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, grade) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = subject, values_from = pct_proficient) |>
  mutate(
    Math = round(Math * 100, 1),
    ELA = round(ELA * 100, 1),
    gap = ELA - Math
  )
```

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students") |>
  filter(subject %in% c("Math", "ELA")) |>
  filter(grade %in% c("03", "04", "05", "06", "07", "08")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, grade) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = grade, y = pct_proficient, color = subject, group = subject)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Math vs ELA Proficiency by Grade (2024)",
    x = "Grade", y = "Proficiency Rate",
    color = "Subject"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 3: Grade 3 Reading Milestone

Third grade reading proficiency is a critical milestone - students who
can’t read by grade 3 are four times more likely to drop out of high
school.

``` r
assess_2024 |>
  filter(is_state, grade == "03", subject == "ELA") |>
  filter(subgroup == "All Students") |>
  select(proficiency_level, n_students, pct) |>
  mutate(pct_display = round(pct * 100, 1))
```

``` r
assess_2024 |>
  filter(is_state, grade == "03", subject == "ELA", subgroup == "All Students") |>
  mutate(proficiency_level = factor(
    proficiency_level,
    levels = c("below", "approaching", "on_track", "mastered")
  )) |>
  ggplot(aes(x = proficiency_level, y = pct, fill = proficiency_level)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("below" = "#d73027", "approaching" = "#fc8d59",
                               "on_track" = "#91cf60", "mastered" = "#1a9850")) +
  labs(
    title = "Grade 3 ELA Proficiency Distribution (2024)",
    subtitle = "Only ~40% of 3rd graders are reading on grade level",
    x = "Proficiency Level", y = "Percent of Students"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

------------------------------------------------------------------------

## Story 4: Achievement Gaps by Race/Ethnicity

Significant proficiency gaps persist between racial/ethnic groups in
Tennessee.

``` r
race_groups <- c("All Students", "White", "Black", "Hispanic", "Asian")

assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL", subject == "Math") |>
  filter(subgroup %in% race_groups) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  mutate(pct_display = round(pct_proficient * 100, 1)) |>
  arrange(desc(pct_proficient))
```

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL", subject == "Math") |>
  filter(subgroup %in% race_groups) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(subgroup, pct_proficient), y = pct_proficient)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(pct_proficient * 100, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7)) +
  labs(
    title = "Math Proficiency by Race/Ethnicity (2024)",
    x = NULL, y = "Proficiency Rate"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 5: Economic Disadvantage Gap

Students from economically disadvantaged backgrounds score 20+
percentage points lower than their peers.

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL") |>
  filter(subgroup %in% c("Economically Disadvantaged", "Non-Economically Disadvantaged")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = subgroup, values_from = pct_proficient) |>
  mutate(
    ED = round(`Economically Disadvantaged` * 100, 1),
    NonED = round(`Non-Economically Disadvantaged` * 100, 1),
    gap = NonED - ED
  ) |>
  select(subject, ED, NonED, gap)
```

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL", subject == "Math") |>
  filter(subgroup %in% c("Economically Disadvantaged", "Non-Economically Disadvantaged")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  ggplot(aes(x = subgroup, y = pct_proficient, fill = subgroup)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct_proficient * 100, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  scale_fill_manual(values = c("Economically Disadvantaged" = "#fc8d59",
                               "Non-Economically Disadvantaged" = "#91bfdb")) +
  labs(
    title = "Math Proficiency: Economic Disadvantage Gap (2024)",
    x = NULL, y = "Proficiency Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

------------------------------------------------------------------------

## Story 6: English Learner Performance

English Learners face significant challenges on Tennessee assessments,
especially in ELA.

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL") |>
  filter(subgroup %in% c("English Learners", "Non-EL", "All Students")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = subgroup, values_from = pct_proficient) |>
  mutate(across(where(is.numeric), ~ round(. * 100, 1)))
```

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL") |>
  filter(subgroup %in% c("English Learners", "All Students")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = subject, y = pct_proficient, fill = subgroup)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "English Learner vs All Students Proficiency (2024)",
    x = NULL, y = "Proficiency Rate",
    fill = "Group"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 7: Students with Disabilities

Students with disabilities have the lowest proficiency rates of any
subgroup.

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL", subject == "ELA") |>
  filter(subgroup %in% c("Students with Disabilities", "Non-SWD", "All Students")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  mutate(pct_display = round(pct_proficient * 100, 1))
```

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL") |>
  filter(subgroup %in% c("Students with Disabilities", "Non-SWD")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = subject, y = pct_proficient, fill = subgroup)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Students with Disabilities" = "#e66101",
                               "Non-SWD" = "#5e3c99")) +
  labs(
    title = "Students with Disabilities vs Non-SWD Proficiency (2024)",
    x = NULL, y = "Proficiency Rate",
    fill = "Group"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 8: Gender Gaps in STEM

Female students outperform male students in ELA, while male students
have a slight edge in math.

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL") |>
  filter(subject %in% c("Math", "ELA", "Science")) |>
  filter(subgroup %in% c("Male", "Female")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = subgroup, values_from = pct_proficient) |>
  mutate(
    Male = round(Male * 100, 1),
    Female = round(Female * 100, 1),
    gap = Female - Male
  )
```

``` r
assess_2024 |>
  filter(is_state, grade == "All" | grade == "ALL") |>
  filter(subject %in% c("Math", "ELA", "Science")) |>
  filter(subgroup %in% c("Male", "Female")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject, subgroup) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = subject, y = pct_proficient, fill = subgroup)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Male" = "#7570b3", "Female" = "#d95f02")) +
  labs(
    title = "Proficiency by Gender (2024)",
    x = NULL, y = "Proficiency Rate",
    fill = "Gender"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 9: Grade-by-Grade Proficiency Trends

Math proficiency declines sharply from grade 3 to grade 8.

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", subject == "Math") |>
  filter(grade %in% c("03", "04", "05", "06", "07", "08")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(grade) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  mutate(pct_display = round(pct_proficient * 100, 1))
```

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", subject == "Math") |>
  filter(grade %in% c("03", "04", "05", "06", "07", "08")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(grade) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  ggplot(aes(x = grade, y = pct_proficient, group = 1)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = paste0(round(pct_proficient * 100), "%")),
            vjust = -1, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0.2, 0.5)) +
  labs(
    title = "Math Proficiency Declines Through Middle School (2024)",
    subtitle = "State-level, All Students",
    x = "Grade", y = "Proficiency Rate"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 10: Science Proficiency

Science assessment results show opportunities for improvement in STEM
education.

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", subject == "Science") |>
  filter(grade %in% c("03", "04", "05", "06", "07", "08")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(grade) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  mutate(pct_display = round(pct_proficient * 100, 1))
```

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", subject == "Science") |>
  filter(grade %in% c("03", "04", "05", "06", "07", "08")) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(grade) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  ggplot(aes(x = grade, y = pct_proficient)) +
  geom_col(fill = "#2c7bb6") +
  geom_text(aes(label = paste0(round(pct_proficient * 100), "%")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  labs(
    title = "Science Proficiency by Grade (2024)",
    x = "Grade", y = "Proficiency Rate"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 11: Proficiency Distribution Deep Dive

Understanding the full distribution of student performance reveals more
than proficiency rates alone.

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", subject == "Math") |>
  filter(grade == "All" | grade == "ALL") |>
  select(proficiency_level, n_students, pct) |>
  mutate(pct_display = round(pct * 100, 1))
```

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students", subject == "Math") |>
  filter(grade == "All" | grade == "ALL") |>
  mutate(proficiency_level = factor(
    proficiency_level,
    levels = c("below", "approaching", "on_track", "mastered")
  )) |>
  ggplot(aes(x = proficiency_level, y = pct, fill = proficiency_level)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct * 100, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.4)) +
  scale_fill_manual(values = c("below" = "#d73027", "approaching" = "#fc8d59",
                               "on_track" = "#91cf60", "mastered" = "#1a9850")) +
  labs(
    title = "Math Proficiency Distribution (2024)",
    subtitle = "State-level, All Students",
    x = "Proficiency Level", y = "Percent of Students"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

------------------------------------------------------------------------

## Story 12: EOC High School Results

End-of-Course exam results show variation across high school subjects.

``` r
eoc_subjects <- c("Algebra I", "Algebra II", "Geometry",
                  "English I", "English II", "Biology I")

assess_2024 |>
  filter(is_state, subgroup == "All Students") |>
  filter(subject %in% eoc_subjects) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  mutate(pct_display = round(pct_proficient * 100, 1)) |>
  arrange(desc(pct_proficient))
```

``` r
assess_2024 |>
  filter(is_state, subgroup == "All Students") |>
  filter(subject %in% eoc_subjects) |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(subject) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(subject, pct_proficient), y = pct_proficient)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "End-of-Course Exam Proficiency Rates (2024)",
    x = NULL, y = "Proficiency Rate"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 13: Year-over-Year Trends

Comparing 2023 to 2024 shows incremental progress in most areas.

``` r
# Fetch multi-year data
assess_multi <- fetch_assessment_multi(c(2023, 2024), level = "state", use_cache = TRUE)

assess_multi |>
  filter(is_state, subgroup == "All Students", subject == "Math") |>
  filter(grade == "All" | grade == "ALL") |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(end_year) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  mutate(pct_display = round(pct_proficient * 100, 1))
```

``` r
assess_multi |>
  filter(is_state, subgroup == "All Students") |>
  filter(subject %in% c("Math", "ELA")) |>
  filter(grade == "All" | grade == "ALL") |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(end_year, subject) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = factor(end_year), y = pct_proficient, fill = subject)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Year-over-Year Proficiency Trends",
    x = "Year", y = "Proficiency Rate",
    fill = "Subject"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 14: COVID Recovery Analysis

Comparing 2019 (pre-COVID) to 2024 shows pandemic learning recovery
progress.

``` r
# 2020 has no data due to COVID waiver
assess_recovery <- fetch_assessment_multi(c(2019, 2024), level = "state", use_cache = TRUE)

assess_recovery |>
  filter(is_state, subgroup == "All Students") |>
  filter(subject %in% c("Math", "ELA")) |>
  filter(grade == "All" | grade == "ALL") |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(end_year, subject) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = end_year, values_from = pct_proficient) |>
  mutate(
    pre_covid = round(`2019` * 100, 1),
    current = round(`2024` * 100, 1),
    recovery_pct = round((current - pre_covid) / pre_covid * 100, 1)
  ) |>
  select(subject, pre_covid, current, recovery_pct)
```

``` r
assess_recovery |>
  filter(is_state, subgroup == "All Students", subject == "Math") |>
  filter(grade == "All" | grade == "ALL") |>
  filter(proficiency_level %in% c("on_track", "mastered")) |>
  group_by(end_year) |>
  summarize(pct_proficient = sum(pct, na.rm = TRUE)) |>
  ggplot(aes(x = factor(end_year), y = pct_proficient)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(pct_proficient * 100, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
  labs(
    title = "Math Proficiency: Pre-COVID vs 2024",
    subtitle = "Recovery from pandemic learning loss",
    x = "Year", y = "Proficiency Rate"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Story 15: Mastery Rate Leaders

Identifying which groups achieve mastery (not just proficiency) at the
highest rates.

``` r
assess_2024 |>
  filter(is_state, subject == "Math") |>
  filter(grade == "All" | grade == "ALL") |>
  filter(proficiency_level == "mastered") |>
  filter(subgroup %in% c("All Students", "Asian", "White", "Black", "Hispanic",
                         "Economically Disadvantaged", "Non-Economically Disadvantaged")) |>
  select(subgroup, n_students, pct) |>
  mutate(pct_mastered = round(pct * 100, 1)) |>
  arrange(desc(pct))
```

``` r
assess_2024 |>
  filter(is_state, subject == "Math") |>
  filter(grade == "All" | grade == "ALL") |>
  filter(proficiency_level == "mastered") |>
  filter(subgroup %in% c("All Students", "Asian", "White", "Black", "Hispanic",
                         "Economically Disadvantaged")) |>
  ggplot(aes(x = reorder(subgroup, pct), y = pct)) +
  geom_col(fill = "#1a9850") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Math Mastery Rates by Subgroup (2024)",
    subtitle = "Percent scoring 'Mastered' (highest level)",
    x = NULL, y = "Mastery Rate"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------

## Data Notes

### Data Source

Assessment data is sourced directly from the Tennessee Department of
Education accountability portal. Files are downloaded from:
`https://www.tn.gov/content/dam/tn/education/accountability/{year}/`

### Available Years

- **2019**: Pre-COVID baseline
- **2020**: No data (COVID-19 testing waiver)
- **2021-2025**: Post-pandemic assessment data

### Suppression Rules

- Results with fewer than 10 students are suppressed (shown as NA)
- Suppression protects student privacy

### Proficiency Levels

1.  **Below**: Has not met grade-level expectations
2.  **Approaching**: Is approaching grade-level expectations
3.  **On Track**: Has met grade-level expectations
4.  **Mastered**: Has exceeded grade-level expectations

Students scoring “On Track” or “Mastered” are considered proficient.

------------------------------------------------------------------------

## Session Info

``` r
sessionInfo()
```
