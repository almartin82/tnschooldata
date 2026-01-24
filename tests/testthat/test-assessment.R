# Tests for assessment functions
# Note: Internal functions accessed via tnschooldata:::

test_that("get_available_assessment_years returns valid structure", {
  years <- get_available_assessment_years()

  expect_true(is.list(years))
  expect_true("min_year" %in% names(years))
  expect_true("max_year" %in% names(years))
  expect_true("years" %in% names(years))
  expect_true("covid_waiver_year" %in% names(years))
  expect_true("note" %in% names(years))

  # Check year range
  expect_equal(years$min_year, 2019)
  expect_equal(years$max_year, 2025)

  # 2020 should NOT be in years (COVID waiver)
  expect_false(2020 %in% years$years)

  # Other years should be present
  expect_true(2019 %in% years$years)
  expect_true(2021 %in% years$years)
  expect_true(2024 %in% years$years)
})

test_that("get_assessment_url returns correct URLs for known years", {
  # 2024 - standard format
  url_2024 <- get_assessment_url(2024, "state")
  expect_true(grepl("2024", url_2024))
  expect_true(grepl("state_assessment", url_2024, ignore.case = TRUE))
  expect_true(grepl("\\.xlsx$", url_2024, ignore.case = TRUE))

  # 2021 - CSV format
  url_2021 <- get_assessment_url(2021, "state")
  expect_true(grepl("2021", url_2021))
  expect_true(grepl("\\.csv$", url_2021, ignore.case = TRUE))

  # 2019 - CSV format without year suffix
  url_2019 <- get_assessment_url(2019, "district")
  expect_true(grepl("2019", url_2019))
  expect_true(grepl("\\.csv$", url_2019, ignore.case = TRUE))
})

test_that("get_assessment_url returns NULL for invalid years", {
  url <- get_assessment_url(2020, "state")
  expect_null(url)

  url <- get_assessment_url(2018, "state")
  expect_null(url)

  url <- get_assessment_url(2030, "state")
  expect_null(url)
})

test_that("get_assessment_url validates level parameter", {
  expect_error(get_assessment_url(2024, "invalid"))
  expect_error(get_assessment_url(2024, "statewide"))
})

test_that("fetch_assessment validates year parameter", {
  # 2020 should error with specific message
  expect_error(fetch_assessment(2020), "COVID")

  # Years outside valid range should error
  expect_error(fetch_assessment(2018), "end_year must be")
  expect_error(fetch_assessment(2030), "end_year must be")
})

test_that("fetch_assessment validates level parameter", {
  expect_error(fetch_assessment(2024, level = "invalid"))
  expect_error(fetch_assessment(2024, level = "statewide"))
})

test_that("fetch_assessment_multi validates year parameters", {
  # Including 2020 should warn and exclude it
  expect_warning(
    tryCatch(
      fetch_assessment_multi(c(2020, 2021), use_cache = FALSE),
      error = function(e) NULL
    ),
    "2020 excluded"
  )

  # Invalid years should error
  expect_error(fetch_assessment_multi(c(2018, 2024)), "Invalid years")
  expect_error(fetch_assessment_multi(c(2024, 2030)), "Invalid years")
})

test_that("fetch_assessment_multi with only 2020 should error", {
  expect_error(
    suppressWarnings(fetch_assessment_multi(2020)),
    "No valid years"
  )
})

test_that("standardize_subject maps common values correctly", {
  # Test via internal function
  expect_equal(standardize_subject("ELA"), "ELA")
  expect_equal(standardize_subject("ENGLISH LANGUAGE ARTS"), "ELA")
  expect_equal(standardize_subject("MATH"), "Math")
  expect_equal(standardize_subject("MATHEMATICS"), "Math")
  expect_equal(standardize_subject("SCIENCE"), "Science")
  expect_equal(standardize_subject("SOCIAL STUDIES"), "Social Studies")

  # EOC subjects
  expect_equal(standardize_subject("ALGEBRA I"), "Algebra I")
  expect_equal(standardize_subject("BIOLOGY"), "Biology I")
  expect_equal(standardize_subject("ENGLISH II"), "English II")
})

test_that("standardize_grade pads single digits", {
  expect_equal(standardize_grade("3"), "03")
  expect_equal(standardize_grade("4"), "04")
  expect_equal(standardize_grade("8"), "08")
  expect_equal(standardize_grade("10"), "10")

  # Handles GRADE prefix
  expect_equal(standardize_grade("GRADE 3"), "03")
  expect_equal(standardize_grade("Grade 5"), "05")
})

test_that("standardize_subgroup maps common values correctly", {
  expect_equal(standardize_subgroup("All Students"), "All Students")
  expect_equal(standardize_subgroup("ALL STUDENTS"), "All Students")
  expect_equal(standardize_subgroup("Black/African American"), "Black")
  expect_equal(standardize_subgroup("Hispanic/Latino"), "Hispanic")
  expect_equal(standardize_subgroup("Economically Disadvantaged"), "Economically Disadvantaged")
  expect_equal(standardize_subgroup("Students with Disabilities"), "Students with Disabilities")
  expect_equal(standardize_subgroup("English Learners"), "English Learners")
})

test_that("create_empty_assessment_result has expected columns", {
  result <- create_empty_assessment_result(2024)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true("end_year" %in% names(result))
  expect_true("type" %in% names(result))
  expect_true("subject" %in% names(result))
  expect_true("grade" %in% names(result))
  expect_true("subgroup" %in% names(result))
  expect_true("n_tested" %in% names(result))
  expect_true("pct_below" %in% names(result))
  expect_true("pct_mastered" %in% names(result))
  expect_true("pct_proficient" %in% names(result))
})

test_that("create_empty_tidy_assessment has expected columns", {
  result <- create_empty_tidy_assessment()

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true("proficiency_level" %in% names(result))
  expect_true("n_students" %in% names(result))
  expect_true("pct" %in% names(result))
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_school" %in% names(result))
})

test_that("tidy_assessment produces correct long format", {
  # Create sample wide data
  wide <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    district_name = NA_character_,
    school_id = NA_character_,
    school_name = NA_character_,
    test = "TCAP",
    subject = "Math",
    grade = "03",
    subgroup = "All Students",
    n_tested = 100,
    pct_below = 10,
    pct_approaching = 25,
    pct_on_track = 40,
    pct_mastered = 25,
    pct_proficient = 65,
    stringsAsFactors = FALSE
  )

  # Tidy it
  tidy_result <- tidy_assessment(wide)

  # Check structure
  expect_true("proficiency_level" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check proficiency levels
  levels <- unique(tidy_result$proficiency_level)
  expect_true("below" %in% levels)
  expect_true("approaching" %in% levels)
  expect_true("on_track" %in% levels)
  expect_true("mastered" %in% levels)

  # Check that n_students were calculated correctly
  # below: 10% of 100 = 10
  below_row <- tidy_result[tidy_result$proficiency_level == "below", ]
  expect_equal(below_row$n_students, 10)

  # mastered: 25% of 100 = 25
  mastered_row <- tidy_result[tidy_result$proficiency_level == "mastered", ]
  expect_equal(mastered_row$n_students, 25)

  # Check pct normalized to 0-1
  expect_true(all(tidy_result$pct <= 1, na.rm = TRUE))
})

test_that("id_assessment_aggs adds correct flags", {
  # Create sample data
  df <- data.frame(
    end_year = 2024,
    type = c("State", "District", "School"),
    district_id = c(NA, "0470", "0470"),
    school_id = c(NA, NA, "0010"),
    subject = "Math",
    grade = "03",
    subgroup = "All Students",
    stringsAsFactors = FALSE
  )

  result <- id_assessment_aggs(df)

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_school" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_school))

  # Check correct assignment
  expect_true(result$is_state[result$type == "State"])
  expect_true(result$is_district[result$type == "District"])
  expect_true(result$is_school[result$type == "School"])

  # Check mutual exclusivity
  type_sums <- result$is_state + result$is_district + result$is_school
  expect_true(all(type_sums == 1))
})

test_that("assessment cache path includes level", {
  path <- get_assessment_cache_path(2024, "tidy", "all")
  expect_true(grepl("assessment_tidy_all_2024.rds", path))

  path_state <- get_assessment_cache_path(2024, "wide", "state")
  expect_true(grepl("assessment_wide_state_2024.rds", path_state))
})

test_that("assessment_cache_exists returns FALSE for non-existent cache", {
  # Use a year that won't exist
  expect_false(assessment_cache_exists(9999, "tidy", "all"))
})
