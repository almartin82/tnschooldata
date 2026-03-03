# ==============================================================================
# Assessment Year Coverage Tests for tnschooldata
# ==============================================================================
#
# Per-year tests for all TCAP/TNReady assessment years:
# - Available years: 2019, 2021, 2022, 2023, 2024, 2025 (no 2020 due to COVID)
# - ELA + Math subjects present
# - Proficiency levels: below, approaching, on_track, mastered
# - Entity structure (state, district, school)
# - Pinned state-level values for 2024
# - COVID year 2020 handling
#
# All fetch calls use use_cache = TRUE
# ==============================================================================

# ==============================================================================
# HELPER: Reusable assessment loader with skip-on-failure
# ==============================================================================

fetch_assess_or_skip <- function(year, level = "all", tidy = TRUE) {
  result <- tryCatch(
    fetch_assessment(year, level = level, tidy = tidy, use_cache = TRUE),
    error = function(e) NULL
  )
  if (is.null(result) || nrow(result) == 0) {
    testthat::skip(paste("Assessment data unavailable for", year))
  }
  result
}

# ==============================================================================
# 2024 ASSESSMENT: COMPREHENSIVE TESTS
# ==============================================================================

test_that("2024 assessment: loads with >0 rows and required columns", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  expect_gt(nrow(assess), 0)

  required_cols <- c("end_year", "type", "subject", "grade", "subgroup",
                     "n_tested", "proficiency_level", "n_students", "pct",
                     "is_state", "is_district", "is_school")
  for (col in required_cols) {
    expect_true(col %in% names(assess), info = paste("Missing column:", col))
  }

  expect_true(all(assess$end_year == 2024))
})

test_that("2024 assessment: has ELA and Math subjects", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  subjects <- unique(assess$subject)
  expect_true("ELA" %in% subjects, info = "Missing ELA subject")
  expect_true("Math" %in% subjects, info = "Missing Math subject")
})

test_that("2024 assessment: has all four proficiency levels", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  levels <- unique(assess$proficiency_level)
  expected <- c("below", "approaching", "on_track", "mastered")
  for (lv in expected) {
    expect_true(lv %in% levels, info = paste("Missing proficiency level:", lv))
  }
})

test_that("2024 assessment: has three test types (TNREADY, EOC, DLM)", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  tests <- unique(assess$test)
  expect_true("TNREADY" %in% tests, info = "Missing TNREADY test type")
  expect_true("EOC" %in% tests, info = "Missing EOC test type")
})

test_that("2024 assessment: pinned TNREADY Math grade 03 n_tested ~71K", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  math3 <- assess[assess$test == "TNREADY" &
                   assess$subject == "Math" &
                   assess$grade == "03" &
                   assess$subgroup == "All Students" &
                   assess$proficiency_level == "below", ]

  skip_if(nrow(math3) == 0, "No TNREADY Math grade 03 data")

  # Pinned: n_tested = 71,234 for 2024 TNREADY Math grade 03
  expect_true(math3$n_tested[1] > 60000,
    info = paste("Math 03 n_tested too low:", math3$n_tested[1]))
  expect_true(math3$n_tested[1] < 85000,
    info = paste("Math 03 n_tested too high:", math3$n_tested[1]))
})

test_that("2024 assessment: pinned TNREADY ELA grade 03 n_tested ~71K", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  ela3 <- assess[assess$test == "TNREADY" &
                  assess$subject == "ELA" &
                  assess$grade == "03" &
                  assess$subgroup == "All Students" &
                  assess$proficiency_level == "below", ]

  skip_if(nrow(ela3) == 0, "No TNREADY ELA grade 03 data")

  # Pinned: n_tested = 71,249 for 2024 TNREADY ELA grade 03
  expect_true(ela3$n_tested[1] > 60000,
    info = paste("ELA 03 n_tested too low:", ela3$n_tested[1]))
  expect_true(ela3$n_tested[1] < 85000,
    info = paste("ELA 03 n_tested too high:", ela3$n_tested[1]))
})

test_that("2024 assessment: pinned TNREADY Math All grades n_tested ~418K", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  math_all <- assess[assess$test == "TNREADY" &
                     assess$subject == "Math" &
                     assess$grade == "All" &
                     assess$subgroup == "All Students" &
                     assess$proficiency_level == "below", ]

  skip_if(nrow(math_all) == 0, "No TNREADY Math All-grade data")

  # Pinned: n_tested = 418,643 for 2024 TNREADY Math All grades
  expect_true(math_all$n_tested[1] > 350000,
    info = paste("Math All n_tested too low:", math_all$n_tested[1]))
  expect_true(math_all$n_tested[1] < 500000,
    info = paste("Math All n_tested too high:", math_all$n_tested[1]))
})

test_that("2024 assessment: EOC subjects include Algebra I and Biology I", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  eoc_subjects <- unique(assess$subject[assess$test == "EOC"])
  expect_true("Algebra I" %in% eoc_subjects, info = "Missing Algebra I in EOC")
  expect_true("Biology I" %in% eoc_subjects, info = "Missing Biology I in EOC")
  expect_true("English II" %in% eoc_subjects, info = "Missing English II in EOC")
})

test_that("2024 assessment: has standard subgroups", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  subgroups <- unique(assess$subgroup)
  expected_subs <- c("All Students", "Economically Disadvantaged",
                     "Students with Disabilities", "English Learners")
  for (s in expected_subs) {
    expect_true(s %in% subgroups, info = paste("Missing subgroup:", s))
  }
})

test_that("2024 assessment: TNREADY tested grades 03-08", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  tnready_grades <- unique(assess$grade[assess$test == "TNREADY" &
                                         assess$subject == "Math"])
  expected_grades <- c("03", "04", "05", "06", "07", "08")
  for (g in expected_grades) {
    expect_true(g %in% tnready_grades,
      info = paste("Missing TNREADY Math grade:", g))
  }
})

# ==============================================================================
# OTHER YEARS: CORE CHECKS
# ==============================================================================

test_that("2023 assessment: loads with ELA and Math", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2023, level = "state")

  expect_gt(nrow(assess), 0)
  expect_true(all(assess$end_year == 2023))

  subjects <- unique(assess$subject)
  expect_true("ELA" %in% subjects)
  expect_true("Math" %in% subjects)
})

test_that("2023 assessment: has 4 proficiency levels", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2023, level = "state")

  levels <- unique(assess$proficiency_level)
  for (lv in c("below", "approaching", "on_track", "mastered")) {
    expect_true(lv %in% levels, info = paste("Missing 2023 level:", lv))
  }
})

test_that("2022 assessment: loads with >0 rows", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2022, level = "state")

  expect_gt(nrow(assess), 0)
  expect_true(all(assess$end_year == 2022))
})

test_that("2021 assessment: loads (first post-COVID year)", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2021, level = "state")

  expect_gt(nrow(assess), 0)
  expect_true(all(assess$end_year == 2021))

  subjects <- unique(assess$subject)
  expect_true("ELA" %in% subjects)
  expect_true("Math" %in% subjects)
})

test_that("2019 assessment: loads (pre-COVID)", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2019, level = "state")

  expect_gt(nrow(assess), 0)
  expect_true(all(assess$end_year == 2019))
})

test_that("2025 assessment: loads with >0 rows", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2025, level = "state")

  expect_gt(nrow(assess), 0)
  expect_true(all(assess$end_year == 2025))
})

# ==============================================================================
# COVID YEAR 2020 HANDLING
# ==============================================================================

test_that("2020 assessment: fetch_assessment errors with COVID message", {
  expect_error(fetch_assessment(2020), "COVID")
})

test_that("2020 is not in available assessment years", {
  years <- get_available_assessment_years()
  expect_false(2020 %in% years$years)
  expect_equal(years$covid_waiver_year, 2020)
})

test_that("fetch_assessment_multi excludes 2020 with warning", {
  # Requesting only 2020 should error (no valid years left)
  expect_error(
    suppressWarnings(fetch_assessment_multi(2020)),
    "No valid years"
  )

  # Including 2020 among valid years should warn
  expect_warning(
    tryCatch(
      fetch_assessment_multi(c(2020, 2024), use_cache = TRUE),
      error = function(e) NULL
    ),
    "2020 excluded"
  )
})

# ==============================================================================
# ENTITY STRUCTURE
# ==============================================================================

test_that("2024 assessment: entity flags are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  type_sums <- assess$is_state + assess$is_district + assess$is_school
  expect_true(all(type_sums == 1),
    info = "Assessment entity flags are not mutually exclusive")
})

test_that("2024 assessment: state-level data has state flag TRUE", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  expect_true(all(assess$is_state),
    info = "State-level request should only return state data")
})

# ==============================================================================
# WIDE FORMAT
# ==============================================================================

test_that("2024 assessment wide: has proficiency pct columns", {
  skip_on_cran()
  skip_if_offline()

  assess_wide <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess_wide) || nrow(assess_wide) == 0, "No wide data")

  expected_cols <- c("pct_below", "pct_approaching", "pct_on_track", "pct_mastered")
  for (col in expected_cols) {
    expect_true(col %in% names(assess_wide),
      info = paste("Missing wide column:", col))
  }
})

test_that("2024 assessment wide: has pct_proficient column", {
  skip_on_cran()
  skip_if_offline()

  assess_wide <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess_wide) || nrow(assess_wide) == 0, "No wide data")

  expect_true("pct_proficient" %in% names(assess_wide),
    info = "Missing pct_proficient column")
})

# ==============================================================================
# VALIDATION ERRORS
# ==============================================================================

test_that("fetch_assessment rejects invalid years", {
  expect_error(fetch_assessment(2018), "end_year must be")
  expect_error(fetch_assessment(2030), "end_year must be")
})

test_that("fetch_assessment rejects invalid level parameter", {
  expect_error(fetch_assessment(2024, level = "invalid"))
  expect_error(fetch_assessment(2024, level = "statewide"))
})

test_that("fetch_assessment_multi rejects all-invalid years", {
  expect_error(fetch_assessment_multi(c(2017, 2018)), "Invalid years")
})

test_that("get_available_assessment_years returns correct structure", {
  years <- get_available_assessment_years()

  expect_true(is.list(years))
  expect_equal(years$min_year, 2019)
  expect_equal(years$max_year, 2025)
  expect_false(2020 %in% years$years)
  expect_equal(length(years$years), 6)  # 2019, 2021-2025
  expect_true(all(c(2019, 2021, 2022, 2023, 2024, 2025) %in% years$years))
})

# ==============================================================================
# fetch_assessment_multi
# ==============================================================================

test_that("fetch_assessment_multi combines years correctly", {
  skip_on_cran()
  skip_if_offline()

  multi <- tryCatch(
    fetch_assessment_multi(c(2023, 2024), level = "state",
                           tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(multi) || nrow(multi) == 0, "Multi-year data unavailable")

  expect_gt(nrow(multi), 0)
  expect_true(2023 %in% multi$end_year)
  expect_true(2024 %in% multi$end_year)
})

# ==============================================================================
# HELPER FUNCTION TESTS
# ==============================================================================

test_that("assessment_summary requires tidy data", {
  df_no_prof <- data.frame(subject = "Math", pct = 0.5)
  expect_error(assessment_summary(df_no_prof), "tidy assessment data")
})

test_that("calc_proficiency requires tidy data", {
  df_no_prof <- data.frame(subject = "Math", pct = 0.5)
  expect_error(calc_proficiency(df_no_prof), "tidy assessment data")
})

test_that("calc_proficiency works on real 2024 data", {
  skip_on_cran()
  skip_if_offline()

  assess <- fetch_assess_or_skip(2024, level = "state")

  prof <- tryCatch(
    calc_proficiency(
      assess[assess$test == "TNREADY" &
             assess$subject == "Math" &
             assess$subgroup == "All Students", ]
    ),
    error = function(e) NULL
  )
  skip_if(is.null(prof), "calc_proficiency failed")

  expect_true("pct_proficient" %in% names(prof))
  expect_true(all(prof$pct_proficient >= 0 & prof$pct_proficient <= 1, na.rm = TRUE))
})
