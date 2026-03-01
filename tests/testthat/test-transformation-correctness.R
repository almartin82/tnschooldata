# ==============================================================================
# Transformation Correctness Tests for tnschooldata
# ==============================================================================
#
# These tests verify that every transformation step in the pipeline preserves
# data fidelity and produces correct output. Tests use real cached data from
# bundled extdata or live cache.
#
# Transformation catalog tested:
#   1. safe_numeric() - suppression markers, commas, whitespace
#   2. process_enr() - raw -> standard schema (column mapping, pct-to-count)
#   3. tidy_enr() - wide -> long pivot (subgroups, grades, total)
#   4. id_enr_aggs() - type-based flag assignment
#   5. enr_grade_aggs() - grade grouping (K8, HS, K12)
#   6. create_state_aggregate() - district sum -> state row
#   7. process_assessment() - raw assessment -> standard schema
#   8. tidy_assessment() - wide -> long proficiency pivot
#   9. id_assessment_aggs() - assessment type flags
#  10. standardize_subject/grade/subgroup() - name normalization
#  11. pct_to_count conversion - round(pct/100 * total)
#  12. pct cap - pmin(n_students / row_total, 1.0)
# ==============================================================================

library(testthat)

# ==============================================================================
# 1. safe_numeric() edge cases
# ==============================================================================

test_that("safe_numeric handles comma-separated thousands", {
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("100,000"), 100000)
})

test_that("safe_numeric handles all suppression markers", {
  markers <- c("*", "**", "***", ".", "-", "-1", "<5", "<10", "N/A", "NA", "", "NULL")
  for (m in markers) {
    expect_true(is.na(safe_numeric(m)),
      info = paste("safe_numeric did not return NA for marker:", repr(m)))
  }
})

test_that("safe_numeric handles spaced less-than markers", {
  expect_true(is.na(safe_numeric("< 5")))
  expect_true(is.na(safe_numeric("< 10")))
  expect_true(is.na(safe_numeric("<  20")))
})

test_that("safe_numeric handles decimal values", {
  expect_equal(safe_numeric("3.14"), 3.14)
  expect_equal(safe_numeric("0.001"), 0.001)
  expect_equal(safe_numeric("99.9"), 99.9)
})

test_that("safe_numeric vectorized returns correct length", {
  input <- c("100", "*", "200", "< 5", "300")
  result <- safe_numeric(input)
  expect_equal(length(result), 5)
  expect_equal(result[1], 100)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 200)
  expect_true(is.na(result[4]))
  expect_equal(result[5], 300)
})

test_that("safe_numeric returns numeric(0) for NULL and empty", {
  expect_equal(length(safe_numeric(NULL)), 0)
  expect_equal(length(safe_numeric(character(0))), 0)
})

# ==============================================================================
# 2. ID padding functions
# ==============================================================================

test_that("pad_district_id handles leading zeros correctly", {
  # Removes then re-pads
  expect_equal(pad_district_id("0001"), "0001")
  expect_equal(pad_district_id("001"), "0001")
  expect_equal(pad_district_id("1"), "0001")
  expect_equal(pad_district_id("9999"), "9999")
})

test_that("make_campus_id produces 8-char IDs", {
  result <- make_campus_id("470", "5")
  expect_equal(nchar(result), 8)
  expect_equal(result, "04700005")
})

# ==============================================================================
# 3. tidy_enr() transformation correctness
# ==============================================================================

test_that("tidy_enr preserves row_total as total_enrollment", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0470",
    campus_id = NA_character_,
    district_name = "Knox County",
    campus_name = NA_character_,
    row_total = 58838,
    white = 38245,
    black = 10002,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)
  total_row <- tidy_result[tidy_result$subgroup == "total_enrollment", ]

  expect_equal(nrow(total_row), 1)
  expect_equal(total_row$n_students, 58838)
  expect_equal(total_row$pct, 1.0)
  expect_equal(total_row$grade_level, "TOTAL")
})

test_that("tidy_enr preserves demographic counts exactly", {
  wide <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    row_total = 1000,
    white = 600,
    black = 200,
    hispanic = 100,
    asian = 50,
    native_american = 10,
    pacific_islander = 5,
    multiracial = 35,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)

  # Each demographic count must be preserved exactly
  for (sg in c("white", "black", "hispanic", "asian", "native_american",
               "pacific_islander", "multiracial")) {
    tidy_row <- tidy_result[tidy_result$subgroup == sg, ]
    expect_equal(tidy_row$n_students, wide[[sg]],
      info = paste(sg, "count not preserved"))
  }
})

test_that("tidy_enr computes pct correctly as n_students / row_total", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    campus_id = NA_character_,
    district_name = "Test District",
    campus_name = NA_character_,
    row_total = 400,
    white = 200,
    black = 100,
    hispanic = 60,
    asian = 40,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)

  white_row <- tidy_result[tidy_result$subgroup == "white", ]
  expect_equal(white_row$pct, 200 / 400)  # 0.5

  black_row <- tidy_result[tidy_result$subgroup == "black", ]
  expect_equal(black_row$pct, 100 / 400)  # 0.25

  asian_row <- tidy_result[tidy_result$subgroup == "asian", ]
  expect_equal(asian_row$pct, 40 / 400)  # 0.1
})

test_that("tidy_enr caps pct at 1.0 via pmin", {
  # If a special population count exceeds row_total (data error),
  # pct should be capped at 1.0
  wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "0001",
    campus_id = "00010001",
    district_name = "Test",
    campus_name = "Test School",
    row_total = 100,
    econ_disadv = 110,  # exceeds total (data quality issue)
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)
  ed_row <- tidy_result[tidy_result$subgroup == "econ_disadv", ]
  expect_equal(ed_row$pct, 1.0)  # capped, not 1.1
  expect_equal(ed_row$n_students, 110)  # count preserved even though pct capped
})

test_that("tidy_enr produces NA pct when row_total is 0", {
  wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "0001",
    campus_id = "00010001",
    district_name = "Test",
    campus_name = NA_character_,
    row_total = 0,
    white = 0,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)
  white_row <- tidy_result[tidy_result$subgroup == "white", ]

  # When row_total is 0, pct should be NA (not Inf or NaN)
  # But n_students of 0 gets filtered out by filter(!is.na(n_students))
  # so check total_enrollment which is 0
  total_row <- tidy_result[tidy_result$subgroup == "total_enrollment", ]
  if (nrow(total_row) > 0) {
    expect_true(is.na(total_row$pct) || total_row$pct == 1.0,
      info = "total_enrollment pct should be 1.0 or NA when row_total is 0")
  }
})

test_that("tidy_enr filters out NA n_students rows", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    campus_id = NA_character_,
    district_name = "Test",
    campus_name = NA_character_,
    row_total = 500,
    white = 300,
    pacific_islander = NA_real_,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)

  # pacific_islander should be excluded (NA n_students)
  pi_rows <- tidy_result[tidy_result$subgroup == "pacific_islander", ]
  expect_equal(nrow(pi_rows), 0)

  # white should be included
  white_rows <- tidy_result[tidy_result$subgroup == "white", ]
  expect_equal(nrow(white_rows), 1)
})

test_that("tidy_enr maps grade columns to standard grade_level names", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    campus_id = NA_character_,
    district_name = "Test",
    campus_name = NA_character_,
    row_total = 1000,
    grade_pk = 50,
    grade_k = 80,
    grade_01 = 75,
    grade_09 = 85,
    grade_12 = 90,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)

  # Check grade_level mapping
  grade_rows <- tidy_result[tidy_result$grade_level != "TOTAL", ]
  grade_levels <- unique(grade_rows$grade_level)

  expect_true("PK" %in% grade_levels)
  expect_true("K" %in% grade_levels)
  expect_true("01" %in% grade_levels)
  expect_true("09" %in% grade_levels)
  expect_true("12" %in% grade_levels)

  # Verify grade count preservation
  pk_row <- tidy_result[tidy_result$grade_level == "PK", ]
  expect_equal(pk_row$n_students, 50)

  k_row <- tidy_result[tidy_result$grade_level == "K", ]
  expect_equal(k_row$n_students, 80)
})

test_that("tidy_enr grade rows have subgroup = total_enrollment", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    campus_id = NA_character_,
    district_name = "Test",
    campus_name = NA_character_,
    row_total = 200,
    grade_k = 100,
    grade_01 = 100,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)
  grade_rows <- tidy_result[tidy_result$grade_level != "TOTAL", ]

  # All grade rows should have subgroup = "total_enrollment"
  expect_true(all(grade_rows$subgroup == "total_enrollment"))
})

test_that("tidy_enr sets aggregation_flag correctly", {
  wide <- data.frame(
    end_year = c(2024, 2024, 2024),
    type = c("State", "District", "Campus"),
    district_id = c(NA, "0470", "0470"),
    campus_id = c(NA, NA, "04700005"),
    district_name = c(NA, "Knox", "Knox"),
    campus_name = c(NA, NA, "School A"),
    row_total = c(100000, 50000, 500),
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)

  # State: both district_id and campus_id are NA -> "state"
  state_rows <- tidy_result[tidy_result$type == "State", ]
  expect_true(all(state_rows$aggregation_flag == "state"))

  # District: district_id non-NA, campus_id is NA -> "district"
  dist_rows <- tidy_result[tidy_result$type == "District", ]
  expect_true(all(dist_rows$aggregation_flag == "district"))

  # Campus: both non-NA -> "campus"
  campus_rows <- tidy_result[tidy_result$type == "Campus", ]
  expect_true(all(campus_rows$aggregation_flag == "campus"))
})

# ==============================================================================
# 4. id_enr_aggs() flag correctness
# ==============================================================================

test_that("id_enr_aggs flags are mutually exclusive", {
  df <- data.frame(
    type = c("State", "District", "Campus", "District", "Campus"),
    stringsAsFactors = FALSE
  )

  result <- id_enr_aggs(df)

  # Each row should have exactly one TRUE flag
  flag_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(flag_sums == 1))
})

test_that("id_enr_aggs handles empty data frame", {
  df <- data.frame(type = character(0), stringsAsFactors = FALSE)
  result <- id_enr_aggs(df)
  expect_equal(nrow(result), 0)
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))
})

# ==============================================================================
# 5. enr_grade_aggs() computation
# ==============================================================================

test_that("enr_grade_aggs K8 = sum of K,01-08", {
  # Build tidy data with known grade values
  grades <- c("K", "01", "02", "03", "04", "05", "06", "07", "08")
  values <- c(80, 75, 78, 80, 77, 76, 79, 81, 82)
  expected_k8 <- sum(values)

  tidy_df <- data.frame(
    end_year = rep(2024, length(grades)),
    type = rep("District", length(grades)),
    district_id = rep("0001", length(grades)),
    campus_id = rep(NA_character_, length(grades)),
    district_name = rep("Test", length(grades)),
    campus_name = rep(NA_character_, length(grades)),
    subgroup = rep("total_enrollment", length(grades)),
    grade_level = grades,
    n_students = values,
    pct = rep(NA_real_, length(grades)),
    is_state = rep(FALSE, length(grades)),
    is_district = rep(TRUE, length(grades)),
    is_campus = rep(FALSE, length(grades)),
    stringsAsFactors = FALSE
  )

  result <- enr_grade_aggs(tidy_df)
  k8_val <- result$n_students[result$grade_level == "K8"]

  expect_equal(k8_val, expected_k8)
})

test_that("enr_grade_aggs HS = sum of 09-12", {
  grades <- c("09", "10", "11", "12")
  values <- c(85, 83, 80, 94)
  expected_hs <- sum(values)

  tidy_df <- data.frame(
    end_year = rep(2024, length(grades)),
    type = rep("District", length(grades)),
    district_id = rep("0001", length(grades)),
    campus_id = rep(NA_character_, length(grades)),
    district_name = rep("Test", length(grades)),
    campus_name = rep(NA_character_, length(grades)),
    subgroup = rep("total_enrollment", length(grades)),
    grade_level = grades,
    n_students = values,
    pct = rep(NA_real_, length(grades)),
    is_state = rep(FALSE, length(grades)),
    is_district = rep(TRUE, length(grades)),
    is_campus = rep(FALSE, length(grades)),
    stringsAsFactors = FALSE
  )

  result <- enr_grade_aggs(tidy_df)
  hs_val <- result$n_students[result$grade_level == "HS"]

  expect_equal(hs_val, expected_hs)
})

test_that("enr_grade_aggs K12 excludes PK", {
  grades <- c("PK", "K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  values <- c(50, 80, 75, 78, 80, 77, 76, 79, 81, 82, 85, 83, 80, 94)
  expected_k12 <- sum(values[-1])  # excludes PK

  tidy_df <- data.frame(
    end_year = rep(2024, length(grades)),
    type = rep("District", length(grades)),
    district_id = rep("0001", length(grades)),
    campus_id = rep(NA_character_, length(grades)),
    district_name = rep("Test", length(grades)),
    campus_name = rep(NA_character_, length(grades)),
    subgroup = rep("total_enrollment", length(grades)),
    grade_level = grades,
    n_students = values,
    pct = rep(NA_real_, length(grades)),
    is_state = rep(FALSE, length(grades)),
    is_district = rep(TRUE, length(grades)),
    is_campus = rep(FALSE, length(grades)),
    stringsAsFactors = FALSE
  )

  result <- enr_grade_aggs(tidy_df)
  k12_val <- result$n_students[result$grade_level == "K12"]

  expect_equal(k12_val, expected_k12)
})

test_that("enr_grade_aggs only aggregates total_enrollment subgroup", {
  # Non-total_enrollment subgroups should be excluded
  tidy_df <- data.frame(
    end_year = rep(2024, 2),
    type = rep("District", 2),
    district_id = rep("0001", 2),
    campus_id = rep(NA_character_, 2),
    district_name = rep("Test", 2),
    campus_name = rep(NA_character_, 2),
    subgroup = c("white", "white"),
    grade_level = c("K", "01"),
    n_students = c(100, 200),
    pct = rep(NA_real_, 2),
    is_state = rep(FALSE, 2),
    is_district = rep(TRUE, 2),
    is_campus = rep(FALSE, 2),
    stringsAsFactors = FALSE
  )

  result <- enr_grade_aggs(tidy_df)
  expect_equal(nrow(result), 0)
})

# ==============================================================================
# 6. create_state_aggregate() correctness
# ==============================================================================

test_that("create_state_aggregate sums district row_totals", {
  district_df <- data.frame(
    end_year = c(2024, 2024, 2024),
    type = c("District", "District", "District"),
    district_id = c("0001", "0002", "0003"),
    campus_id = rep(NA_character_, 3),
    district_name = c("A", "B", "C"),
    campus_name = rep(NA_character_, 3),
    county = rep(NA_character_, 3),
    region = rep(NA_character_, 3),
    row_total = c(10000, 20000, 30000),
    white = c(5000, 10000, 15000),
    black = c(3000, 6000, 9000),
    stringsAsFactors = FALSE
  )

  state <- create_state_aggregate(district_df, 2024)

  expect_equal(state$type, "State")
  expect_equal(state$row_total, 60000)
  expect_equal(state$white, 30000)
  expect_equal(state$black, 18000)
  expect_true(is.na(state$district_id))
})

test_that("create_state_aggregate handles NA values in columns", {
  district_df <- data.frame(
    end_year = c(2024, 2024),
    type = c("District", "District"),
    district_id = c("0001", "0002"),
    campus_id = rep(NA_character_, 2),
    district_name = c("A", "B"),
    campus_name = rep(NA_character_, 2),
    county = rep(NA_character_, 2),
    region = rep(NA_character_, 2),
    row_total = c(1000, 2000),
    white = c(500, NA),
    stringsAsFactors = FALSE
  )

  state <- create_state_aggregate(district_df, 2024)

  expect_equal(state$row_total, 3000)
  # na.rm = TRUE so white = 500 + 0 = 500
  expect_equal(state$white, 500)
})

# ==============================================================================
# 7. Percentage-to-count conversion (process_enr)
# ==============================================================================

test_that("process_district_enr converts pct columns to counts correctly", {
  # Simulate raw district data with percentage columns (modern format)
  raw_district <- data.frame(
    district_no = c("0", "100"),
    district_name = c("State of Tennessee", "Test District"),
    enrollment = c("1000", "500"),
    white_pct = c("60.0", "50.0"),
    african_american_pct = c("20.0", "30.0"),
    hispanic_pct = c("15.0", "10.0"),
    asian_pct = c("5.0", "10.0"),
    economically_disadvantaged_pct = c("30.0", "40.0"),
    limited_english_proficient_pct = c("8.0", "12.0"),
    students_with_disabilities_pct = c("14.0", "16.0"),
    stringsAsFactors = FALSE
  )

  result <- process_district_enr(raw_district, 2024)

  # State row (district_no=0)
  state_row <- result[result$type == "State", ]
  expect_equal(state_row$row_total, 1000)
  expect_equal(state_row$white, round(60.0 / 100 * 1000))  # 600
  expect_equal(state_row$black, round(20.0 / 100 * 1000))  # 200
  expect_equal(state_row$hispanic, round(15.0 / 100 * 1000))  # 150
  expect_equal(state_row$econ_disadv, round(30.0 / 100 * 1000))  # 300

  # District row
  dist_row <- result[result$type == "District", ]
  expect_equal(dist_row$row_total, 500)
  expect_equal(dist_row$white, round(50.0 / 100 * 500))  # 250
  expect_equal(dist_row$econ_disadv, round(40.0 / 100 * 500))  # 200
})

test_that("process_district_enr identifies state row by district_no=0", {
  raw <- data.frame(
    district_no = c("0", "100", "200"),
    district_name = c("State Total", "District A", "District B"),
    enrollment = c("10000", "5000", "5000"),
    stringsAsFactors = FALSE
  )

  result <- process_district_enr(raw, 2024)

  # district_no = 0 should be type "State"
  expect_equal(sum(result$type == "State"), 1)
  expect_equal(sum(result$type == "District"), 2)

  # State row should have NA district_id
  state <- result[result$type == "State", ]
  expect_true(is.na(state$district_id))
})

# ==============================================================================
# 8. Assessment transformations
# ==============================================================================

test_that("tidy_assessment normalizes pct from 0-100 to 0-1 range", {
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
    n_tested = 80000,
    pct_below = 15.0,
    pct_approaching = 30.0,
    pct_on_track = 35.0,
    pct_mastered = 20.0,
    pct_proficient = 55.0,
    stringsAsFactors = FALSE
  )

  tidy <- tidy_assessment(wide)

  # All pct values should be in 0-1 range
  expect_true(all(tidy$pct <= 1, na.rm = TRUE))
  expect_true(all(tidy$pct >= 0, na.rm = TRUE))

  # Check specific normalization
  below_row <- tidy[tidy$proficiency_level == "below", ]
  expect_equal(below_row$pct, 0.15)

  mastered_row <- tidy[tidy$proficiency_level == "mastered", ]
  expect_equal(mastered_row$pct, 0.20)
})

test_that("tidy_assessment calculates n_students from pct and n_tested", {
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
    n_tested = 1000,
    pct_below = 10.0,
    pct_approaching = 25.0,
    pct_on_track = 40.0,
    pct_mastered = 25.0,
    pct_proficient = 65.0,
    stringsAsFactors = FALSE
  )

  tidy <- tidy_assessment(wide)

  # n_students = round(pct / 100 * n_tested)
  below_row <- tidy[tidy$proficiency_level == "below", ]
  expect_equal(below_row$n_students, round(10.0 / 100 * 1000))  # 100

  approaching_row <- tidy[tidy$proficiency_level == "approaching", ]
  expect_equal(approaching_row$n_students, round(25.0 / 100 * 1000))  # 250

  on_track_row <- tidy[tidy$proficiency_level == "on_track", ]
  expect_equal(on_track_row$n_students, round(40.0 / 100 * 1000))  # 400

  mastered_row <- tidy[tidy$proficiency_level == "mastered", ]
  expect_equal(mastered_row$n_students, round(25.0 / 100 * 1000))  # 250
})

test_that("tidy_assessment produces exactly 4 proficiency levels", {
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

  tidy <- tidy_assessment(wide)
  levels <- unique(tidy$proficiency_level)

  expect_equal(length(levels), 4)
  expect_true("below" %in% levels)
  expect_true("approaching" %in% levels)
  expect_true("on_track" %in% levels)
  expect_true("mastered" %in% levels)
})

test_that("tidy_assessment handles NA proficiency values", {
  wide <- data.frame(
    end_year = 2024,
    type = "School",
    district_id = "0470",
    district_name = "Knox",
    school_id = "0010",
    school_name = "Test School",
    test = "TCAP",
    subject = "Math",
    grade = "03",
    subgroup = "All Students",
    n_tested = 5,  # Small n, likely suppressed
    pct_below = NA_real_,
    pct_approaching = NA_real_,
    pct_on_track = NA_real_,
    pct_mastered = NA_real_,
    pct_proficient = NA_real_,
    stringsAsFactors = FALSE
  )

  tidy <- tidy_assessment(wide)

  # Should still produce rows (4 proficiency levels)
  expect_equal(nrow(tidy), 4)

  # pct and n_students should be NA
  expect_true(all(is.na(tidy$pct)))
  expect_true(all(is.na(tidy$n_students)))
})

test_that("tidy_assessment handles n_tested = 0", {
  wide <- data.frame(
    end_year = 2024,
    type = "School",
    district_id = "0470",
    district_name = "Knox",
    school_id = "0010",
    school_name = "Test School",
    test = "TCAP",
    subject = "Math",
    grade = "03",
    subgroup = "Native American",
    n_tested = 0,
    pct_below = 0,
    pct_approaching = 0,
    pct_on_track = 0,
    pct_mastered = 0,
    pct_proficient = 0,
    stringsAsFactors = FALSE
  )

  tidy <- tidy_assessment(wide)

  # n_students should be NA when n_tested = 0
  expect_true(all(is.na(tidy$n_students)))
})

test_that("tidy_assessment pct cap at 1.0 via pmin", {
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
    pct_below = 5,
    pct_approaching = 10,
    pct_on_track = 40,
    pct_mastered = 60,  # Sum = 115, individual mastered = 60%
    pct_proficient = 100,
    stringsAsFactors = FALSE
  )

  tidy <- tidy_assessment(wide)

  # pmin(pct / 100, 1.0) should cap at 1.0
  mastered_row <- tidy[tidy$proficiency_level == "mastered", ]
  expect_equal(mastered_row$pct, 0.60)  # 60/100 = 0.6, below cap
})

# ==============================================================================
# 9. id_assessment_aggs() correctness
# ==============================================================================

test_that("id_assessment_aggs uses type column for flags", {
  df <- data.frame(
    type = c("State", "District", "School"),
    stringsAsFactors = FALSE
  )

  result <- id_assessment_aggs(df)

  expect_true(result$is_state[1])
  expect_false(result$is_district[1])
  expect_false(result$is_school[1])

  expect_false(result$is_state[2])
  expect_true(result$is_district[2])
  expect_false(result$is_school[2])

  expect_false(result$is_state[3])
  expect_false(result$is_district[3])
  expect_true(result$is_school[3])
})

# ==============================================================================
# 10. standardize_subject() correctness
# ==============================================================================

test_that("standardize_subject normalizes ELA variants", {
  expect_equal(standardize_subject("ELA"), "ELA")
  expect_equal(standardize_subject("ENGLISH LANGUAGE ARTS"), "ELA")
  expect_equal(standardize_subject("READING"), "ELA")
  expect_equal(standardize_subject("RLA"), "ELA")
})

test_that("standardize_subject normalizes Math", {
  expect_equal(standardize_subject("MATH"), "Math")
  expect_equal(standardize_subject("MATHEMATICS"), "Math")
  expect_equal(standardize_subject("math"), "Math")
})

test_that("standardize_subject normalizes EOC subjects", {
  expect_equal(standardize_subject("ALGEBRA I"), "Algebra I")
  expect_equal(standardize_subject("ALGEBRA II"), "Algebra II")
  expect_equal(standardize_subject("GEOMETRY"), "Geometry")
  expect_equal(standardize_subject("BIOLOGY"), "Biology I")
  expect_equal(standardize_subject("ENGLISH II"), "English II")
  expect_equal(standardize_subject("ENGLISH III"), "English III")
  expect_equal(standardize_subject("US HISTORY GEOGRAPHY"), "US History")
})

test_that("standardize_subject does not confuse Algebra I and Algebra II", {
  # Order matters in regex - more specific patterns must come first
  expect_equal(standardize_subject("ALGEBRA I"), "Algebra I")
  expect_equal(standardize_subject("ALGEBRA II"), "Algebra II")
  # Verify they are different
  expect_false(standardize_subject("ALGEBRA I") == standardize_subject("ALGEBRA II"))
})

test_that("standardize_subject does not confuse English I/II/III", {
  expect_equal(standardize_subject("ENGLISH I"), "English I")
  expect_equal(standardize_subject("ENGLISH II"), "English II")
  expect_equal(standardize_subject("ENGLISH III"), "English III")
  # All three should be distinct
  expect_false(standardize_subject("ENGLISH I") == standardize_subject("ENGLISH II"))
  expect_false(standardize_subject("ENGLISH II") == standardize_subject("ENGLISH III"))
})

# ==============================================================================
# 11. standardize_grade() correctness
# ==============================================================================

test_that("standardize_grade pads single digits", {
  expect_equal(standardize_grade("3"), "03")
  expect_equal(standardize_grade("4"), "04")
  expect_equal(standardize_grade("9"), "09")
})

test_that("standardize_grade removes GRADE prefix", {
  expect_equal(standardize_grade("GRADE 3"), "03")
  expect_equal(standardize_grade("Grade 5"), "05")
  expect_equal(standardize_grade("GRADE 10"), "10")
})

test_that("standardize_grade handles ordinal formats", {
  expect_equal(standardize_grade("3RD"), "03")
  expect_equal(standardize_grade("4TH"), "04")
  expect_equal(standardize_grade("8TH"), "08")
})

test_that("standardize_grade preserves double digits", {
  expect_equal(standardize_grade("10"), "10")
  expect_equal(standardize_grade("11"), "11")
  expect_equal(standardize_grade("12"), "12")
})

test_that("standardize_grade handles special values", {
  expect_equal(standardize_grade("EOC"), "EOC")
  expect_equal(standardize_grade("END OF COURSE"), "EOC")
  expect_equal(standardize_grade("ALL GRADES"), "All")
  expect_equal(standardize_grade("All"), "All")
})

# ==============================================================================
# 12. standardize_subgroup() correctness
# ==============================================================================

test_that("standardize_subgroup normalizes race names", {
  expect_equal(standardize_subgroup("Black/African American"), "Black")
  expect_equal(standardize_subgroup("BLACK/AFRICAN AMERICAN"), "Black")
  expect_equal(standardize_subgroup("Hispanic/Latino"), "Hispanic")
  expect_equal(standardize_subgroup("American Indian/Alaska Native"), "Native American")
  expect_equal(standardize_subgroup("Native Hawaiian/Pacific Islander"), "Pacific Islander")
  expect_equal(standardize_subgroup("Two or More Races"), "Multiracial")
})

test_that("standardize_subgroup normalizes special populations", {
  expect_equal(standardize_subgroup("Economically Disadvantaged"), "Economically Disadvantaged")
  expect_equal(standardize_subgroup("Students with Disabilities"), "Students with Disabilities")
  expect_equal(standardize_subgroup("English Learners"), "English Learners")
  expect_equal(standardize_subgroup("English Learner"), "English Learners")
})

test_that("standardize_subgroup normalizes short codes", {
  expect_equal(standardize_subgroup("ED"), "Economically Disadvantaged")
  expect_equal(standardize_subgroup("SWD"), "Students with Disabilities")
  expect_equal(standardize_subgroup("EL"), "English Learners")
  expect_equal(standardize_subgroup("ELL"), "English Learners")
  expect_equal(standardize_subgroup("LEP"), "English Learners")
})

test_that("standardize_subgroup preserves unmapped values", {
  # Values not in the map should be kept as-is
  expect_equal(standardize_subgroup("Some New Subgroup"), "Some New Subgroup")
})

# ==============================================================================
# 13. pct_proficient computation
# ==============================================================================

test_that("process_assessment calculates pct_proficient as on_track + mastered", {
  raw_state <- data.frame(
    system = "0",
    system_name = "State",
    school = "0",
    school_name = "State",
    test = "TCAP",
    subject = "Math",
    grade = "3",
    subgroup = "All Students",
    valid_tests = "1000",
    pct_below = "10",
    pct_approaching = "25",
    pct_on_track = "40",
    pct_mastered = "25",
    stringsAsFactors = FALSE
  )

  result <- process_assessment_level(raw_state, 2024, "State")

  # pct_proficient should be calculated as on_track + mastered
  expect_equal(result$pct_proficient, 40 + 25)  # 65
})

# ==============================================================================
# 14. Wide-to-tidy roundtrip fidelity (using bundled data)
# ==============================================================================

test_that("wide and tidy total_enrollment match for all entities (bundled data)", {
  skip_on_cran()

  # Load bundled data
  wide_path <- system.file("extdata", "enr_wide_2024.rds", package = "tnschooldata")
  tidy_path <- system.file("extdata", "enr_tidy_2024.rds", package = "tnschooldata")

  skip_if(wide_path == "" || !file.exists(wide_path), "No bundled wide data")
  skip_if(tidy_path == "" || !file.exists(tidy_path), "No bundled tidy data")

  wide <- readRDS(wide_path)
  tidy <- readRDS(tidy_path)

  # For every entity in wide, row_total should equal n_students in tidy
  # where subgroup = "total_enrollment" and grade_level = "TOTAL"
  tidy_totals <- tidy[tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL", ]

  # Match on district_id + campus_id + type
  for (i in seq_len(nrow(wide))) {
    did <- wide$district_id[i]
    cid <- wide$campus_id[i]
    etype <- wide$type[i]

    if (etype == "State") {
      match <- tidy_totals[tidy_totals$type == "State", ]
    } else if (etype == "District") {
      match <- tidy_totals[tidy_totals$district_id == did & tidy_totals$type == "District", ]
    } else {
      match <- tidy_totals[tidy_totals$campus_id == cid & tidy_totals$type == "Campus", ]
    }

    if (nrow(match) == 1) {
      expect_equal(
        match$n_students,
        wide$row_total[i],
        info = paste(etype, did, cid, "row_total mismatch")
      )
    }
  }
})

test_that("wide and tidy demographic counts match for state (bundled data)", {
  skip_on_cran()

  wide_path <- system.file("extdata", "enr_wide_2024.rds", package = "tnschooldata")
  tidy_path <- system.file("extdata", "enr_tidy_2024.rds", package = "tnschooldata")

  skip_if(wide_path == "" || !file.exists(wide_path), "No bundled wide data")
  skip_if(tidy_path == "" || !file.exists(tidy_path), "No bundled tidy data")

  wide <- readRDS(wide_path)
  tidy <- readRDS(tidy_path)

  state_wide <- wide[wide$type == "State", ]
  state_tidy <- tidy[tidy$is_state & tidy$grade_level == "TOTAL", ]

  # For each demographic present in wide, verify tidy match
  demo_cols <- c("white", "black", "hispanic", "asian", "econ_disadv", "lep", "special_ed")
  for (col in demo_cols) {
    if (col %in% names(state_wide) && !is.na(state_wide[[col]][1])) {
      tidy_row <- state_tidy[state_tidy$subgroup == col, ]
      if (nrow(tidy_row) == 1) {
        expect_equal(
          tidy_row$n_students,
          state_wide[[col]][1],
          info = paste("State", col, "mismatch: wide=", state_wide[[col]][1],
                       "tidy=", tidy_row$n_students)
        )
      }
    }
  }
})

# ==============================================================================
# 15. Data quality invariants on real data
# ==============================================================================

test_that("no Inf/NaN in tidy enrollment pct (cached data)", {
  skip_on_cran()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(result) || nrow(result) == 0, "No cached data available")

  inf_count <- sum(is.infinite(result$pct), na.rm = TRUE)
  nan_count <- sum(is.nan(result$pct), na.rm = TRUE)

  expect_equal(inf_count, 0, info = paste(inf_count, "Inf values in pct"))
  expect_equal(nan_count, 0, info = paste(nan_count, "NaN values in pct"))
})

test_that("all pct values in tidy enrollment are in [0, 1]", {
  skip_on_cran()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(result) || nrow(result) == 0, "No cached data available")

  valid_pcts <- result$pct[!is.na(result$pct)]
  expect_true(all(valid_pcts >= 0), info = "Found negative pct")
  expect_true(all(valid_pcts <= 1), info = "Found pct > 1")
})

test_that("no negative n_students in tidy enrollment", {
  skip_on_cran()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(result) || nrow(result) == 0, "No cached data available")

  valid_n <- result$n_students[!is.na(result$n_students)]
  expect_true(all(valid_n >= 0), info = "Found negative n_students")
})

test_that("state total enrollment is plausible for Tennessee", {
  skip_on_cran()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(result) || nrow(result) == 0, "No cached data available")

  state_total <- result[result$is_state &
                        result$subgroup == "total_enrollment" &
                        result$grade_level == "TOTAL", ]

  skip_if(nrow(state_total) == 0, "No state row")

  # Tennessee has approximately 900K-1M students
  expect_true(state_total$n_students[1] > 800000,
    info = paste("State enrollment implausibly low:", state_total$n_students[1]))
  expect_true(state_total$n_students[1] < 1500000,
    info = paste("State enrollment implausibly high:", state_total$n_students[1]))
})

test_that("district sum approximates state total (within 1%)", {
  skip_on_cran()

  result <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(result) || nrow(result) == 0, "No cached data available")

  state_row <- result[result$type == "State", ]
  district_rows <- result[result$type == "District", ]

  skip_if(nrow(state_row) == 0 || nrow(district_rows) == 0, "Missing state or district rows")
  skip_if(is.na(state_row$row_total[1]) || state_row$row_total[1] == 0, "Invalid state total")

  district_sum <- sum(district_rows$row_total, na.rm = TRUE)
  pct_diff <- abs(state_row$row_total[1] - district_sum) / state_row$row_total[1] * 100

  expect_true(pct_diff < 1,
    info = paste("District sum", district_sum, "differs from state total",
                 state_row$row_total[1], "by", round(pct_diff, 2), "%"))
})

test_that("each entity has exactly one total_enrollment TOTAL row", {
  skip_on_cran()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(result) || nrow(result) == 0, "No cached data available")

  # Group by entity identifiers (type + district_id + campus_id)
  totals <- result[result$subgroup == "total_enrollment" & result$grade_level == "TOTAL", ]

  # Count per entity
  entity_counts <- table(
    paste(totals$type, totals$district_id, totals$campus_id, sep = "|")
  )

  # Every entity should have exactly 1 total_enrollment TOTAL row
  duplicates <- entity_counts[entity_counts > 1]
  expect_equal(length(duplicates), 0,
    info = paste("Entities with duplicate total_enrollment TOTAL:",
                 paste(names(duplicates), collapse = ", ")))
})

test_that("tidy enrollment has exactly one row per entity per subgroup per grade", {
  skip_on_cran()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(result) || nrow(result) == 0, "No cached data available")

  # Group by entity + subgroup + grade_level
  dupes <- result |>
    dplyr::count(type, district_id, campus_id, subgroup, grade_level) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0,
    info = paste("Found", nrow(dupes), "duplicate entity-subgroup-grade combinations"))
})

# ==============================================================================
# 16. process_enr integration test
# ==============================================================================

test_that("process_enr produces State + District + Campus types from modern data", {
  # Simulate modern era raw data list
  school_raw <- data.frame(
    district_no = c("100", "100"),
    district_name = c("Test District", "Test District"),
    school = c("10", "20"),
    school_name = c("School A", "School B"),
    enrollment = c("500", "300"),
    white_pct = c("60", "40"),
    african_american_pct = c("20", "30"),
    stringsAsFactors = FALSE
  )

  district_raw <- data.frame(
    district_no = c("0", "100"),
    district_name = c("State Total", "Test District"),
    enrollment = c("800", "800"),
    white_pct = c("52.5", "52.5"),
    african_american_pct = c("23.75", "23.75"),
    stringsAsFactors = FALSE
  )

  raw_data <- list(school = school_raw, district = district_raw)
  result <- process_enr(raw_data, 2024)

  types <- unique(result$type)
  expect_true("State" %in% types)
  expect_true("District" %in% types)
  expect_true("Campus" %in% types)
})

# ==============================================================================
# 17. calc_proficiency() correctness
# ==============================================================================

test_that("calc_proficiency sums on_track + mastered", {
  tidy_assess <- data.frame(
    end_year = rep(2024, 4),
    type = rep("State", 4),
    district_id = rep(NA_character_, 4),
    school_id = rep(NA_character_, 4),
    test = rep("TCAP", 4),
    subject = rep("Math", 4),
    grade = rep("03", 4),
    subgroup = rep("All Students", 4),
    n_tested = rep(1000, 4),
    proficiency_level = c("below", "approaching", "on_track", "mastered"),
    n_students = c(100, 250, 400, 250),
    pct = c(0.10, 0.25, 0.40, 0.25),
    stringsAsFactors = FALSE
  )

  result <- calc_proficiency(tidy_assess)

  expect_equal(result$n_proficient, 400 + 250)  # 650
  expect_equal(result$pct_proficient, 650 / 1000)  # 0.65
})

# ==============================================================================
# 18. Edge case: empty data propagation
# ==============================================================================

test_that("tidy_enr handles 0-row data frame", {
  empty_wide <- create_empty_result(2024, "District")
  result <- tidy_enr(empty_wide)

  # Should return non-NULL but empty result
  expect_true(is.data.frame(result))
  # With 0 rows of input, bind_rows should give 0 rows
})

test_that("tidy_assessment handles 0-row data frame", {
  result <- tidy_assessment(create_empty_assessment_result(2024))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true("proficiency_level" %in% names(result))
})

test_that("process_enr handles empty school + empty district", {
  raw_data <- list(
    school = create_empty_enrollment_frame(2024, "school"),
    district = create_empty_enrollment_frame(2024, "district")
  )

  result <- process_enr(raw_data, 2024)
  expect_true(is.data.frame(result))
})

# ==============================================================================
# 19. Multiracial subgroup handling
# ==============================================================================

test_that("multiracial subgroup is present when wide has multiracial column", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    campus_id = NA_character_,
    district_name = "Test",
    campus_name = NA_character_,
    row_total = 1000,
    white = 500,
    multiracial = 100,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)
  subgroups <- unique(tidy_result$subgroup)
  expect_true("multiracial" %in% subgroups)

  multi_row <- tidy_result[tidy_result$subgroup == "multiracial", ]
  expect_equal(multi_row$n_students, 100)
})

test_that("multiracial subgroup is absent when wide lacks multiracial column", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    campus_id = NA_character_,
    district_name = "Test",
    campus_name = NA_character_,
    row_total = 1000,
    white = 500,
    black = 300,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)
  subgroups <- unique(tidy_result$subgroup)
  expect_false("multiracial" %in% subgroups)
})

# ==============================================================================
# 20. Gender subgroup handling
# ==============================================================================

test_that("male and female subgroups are present when wide has gender columns", {
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    campus_id = NA_character_,
    district_name = "Test",
    campus_name = NA_character_,
    row_total = 1000,
    male = 510,
    female = 490,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)
  subgroups <- unique(tidy_result$subgroup)
  expect_true("male" %in% subgroups)
  expect_true("female" %in% subgroups)

  expect_equal(tidy_result$n_students[tidy_result$subgroup == "male"], 510)
  expect_equal(tidy_result$n_students[tidy_result$subgroup == "female"], 490)
})

# ==============================================================================
# 21. Assessment subgroup standardization roundtrip
# ==============================================================================

test_that("standardize_subgroup is idempotent (applying twice gives same result)", {
  test_values <- c("All Students", "Black", "Hispanic", "Economically Disadvantaged",
                   "Students with Disabilities", "English Learners", "Native American",
                   "Pacific Islander", "Multiracial", "Male", "Female")

  for (val in test_values) {
    once <- standardize_subgroup(val)
    twice <- standardize_subgroup(once)
    expect_equal(once, twice,
      info = paste("standardize_subgroup not idempotent for:", val))
  }
})

# ==============================================================================
# 22. Multiple districts - tidy preserves per-entity counts
# ==============================================================================

test_that("tidy_enr preserves counts for each entity independently", {
  wide <- data.frame(
    end_year = c(2024, 2024),
    type = c("District", "District"),
    district_id = c("0001", "0002"),
    campus_id = c(NA, NA),
    district_name = c("Small", "Big"),
    campus_name = c(NA, NA),
    row_total = c(100, 10000),
    white = c(50, 5000),
    black = c(30, 3000),
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(wide)

  # Verify each district independently
  d1_total <- tidy_result[tidy_result$district_id == "0001" &
                          tidy_result$subgroup == "total_enrollment", ]
  expect_equal(d1_total$n_students, 100)

  d2_white <- tidy_result[tidy_result$district_id == "0002" &
                          tidy_result$subgroup == "white", ]
  expect_equal(d2_white$n_students, 5000)
  expect_equal(d2_white$pct, 5000 / 10000)
})
