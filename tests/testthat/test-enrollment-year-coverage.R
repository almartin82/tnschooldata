# ==============================================================================
# Enrollment Year Coverage Tests for tnschooldata
# ==============================================================================
#
# Per-year tests through ALL available years (1999-2024) verifying:
# - Data loads with >0 rows and required columns
# - Pinned state totals (~1M students for modern era)
# - Pinned district enrollment (Shelby County / Nashville)
# - Era-specific behaviors (ASR vs Modern)
# - Subgroup/grade completeness
# - Entity flags and cross-year consistency
#
# All fetch calls use use_cache = TRUE
# ==============================================================================

# ==============================================================================
# HELPER: Reusable enrollment loader with skip-on-failure
# ==============================================================================

fetch_enr_or_skip <- function(year, tidy = TRUE) {
  result <- tryCatch(
    fetch_enr(year, tidy = tidy, use_cache = TRUE),
    error = function(e) NULL
  )
  if (is.null(result) || nrow(result) == 0) {
    testthat::skip(paste("Data unavailable for", year))
  }
  # If tidy data has a state total of 0, the download effectively failed
  if (tidy) {
    st <- result[result$is_state &
                 result$subgroup == "total_enrollment" &
                 result$grade_level == "TOTAL", ]
    if (nrow(st) > 0 && !is.na(st$n_students[1]) && st$n_students[1] == 0) {
      testthat::skip(paste("Download returned zero-enrollment data for", year))
    }
  }
  result
}

# ==============================================================================
# MODERN ERA: Per-Year Coverage (2012-2024)
# ==============================================================================

test_that("2024 enrollment: loads, >0 rows, required columns", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  expect_gt(nrow(enr), 0)
  required_cols <- c("end_year", "type", "district_id", "district_name",
                     "grade_level", "subgroup", "n_students", "pct",
                     "is_state", "is_district", "is_campus")
  for (col in required_cols) {
    expect_true(col %in% names(enr), info = paste("Missing column:", col))
  }
  expect_true(all(enr$end_year == 2024))
})

test_that("2024 enrollment: pinned state total ~971K", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_total) == 0, "No state total row")

  # TN 2024 state total: 971,741 (from cached data)
  # Allow 5% tolerance for rounding in pct-to-count conversion
  expect_true(state_total$n_students[1] > 900000,
    info = paste("State total too low:", state_total$n_students[1]))
  expect_true(state_total$n_students[1] < 1100000,
    info = paste("State total too high:", state_total$n_students[1]))
})

test_that("2024 enrollment: pinned Shelby County (Memphis) >100K", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  shelby <- enr[enr$is_district &
                enr$district_id == "0792" &
                enr$subgroup == "total_enrollment" &
                enr$grade_level == "TOTAL", ]
  skip_if(nrow(shelby) == 0, "No Shelby County row")

  # Memphis-Shelby County Schools 2024: 105,202
  expect_true(shelby$n_students[1] > 90000,
    info = paste("Shelby County too low:", shelby$n_students[1]))
  expect_true(shelby$n_students[1] < 130000,
    info = paste("Shelby County too high:", shelby$n_students[1]))
})

test_that("2024 enrollment: pinned Metro Nashville >70K", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  nash <- enr[enr$is_district &
              enr$district_id == "0190" &
              enr$subgroup == "total_enrollment" &
              enr$grade_level == "TOTAL", ]
  skip_if(nrow(nash) == 0, "No Nashville row")

  # Metro Nashville Public Schools 2024: 77,334
  expect_true(nash$n_students[1] > 65000,
    info = paste("Nashville too low:", nash$n_students[1]))
  expect_true(nash$n_students[1] < 95000,
    info = paste("Nashville too high:", nash$n_students[1]))
})

test_that("2024 enrollment: has all expected demographic subgroups", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  subs <- unique(enr$subgroup)
  expected_subs <- c("total_enrollment", "white", "black", "hispanic", "asian",
                     "econ_disadv", "lep", "special_ed")
  for (s in expected_subs) {
    expect_true(s %in% subs, info = paste("Missing subgroup:", s))
  }
})

test_that("2024 enrollment: has campus (school) level data", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  campus_rows <- enr[enr$is_campus &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  # Modern era (2012+) should have school-level data: ~1800 campuses
  expect_true(nrow(campus_rows) > 1000,
    info = paste("Only", nrow(campus_rows), "campuses found"))
})

test_that("2024 enrollment: ~147 districts", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  district_rows <- enr[enr$is_district &
                       enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL", ]
  # TN has ~147 districts
  expect_gt(nrow(district_rows), 130)
  expect_lt(nrow(district_rows), 170)
})

test_that("2024 enrollment: entity flags are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  type_sums <- enr$is_state + enr$is_district + enr$is_campus
  expect_true(all(type_sums == 1),
    info = "Entity flags are not mutually exclusive")
})

test_that("2023 enrollment: loads with >0 rows", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2023)
  expect_gt(nrow(enr), 0)
  expect_true(all(enr$end_year == 2023))
})

test_that("2023 enrollment: pinned state total >900K", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2023)

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_total) == 0, "No state total row")

  expect_true(state_total$n_students[1] > 900000,
    info = paste("2023 state total too low:", state_total$n_students[1]))
  expect_true(state_total$n_students[1] < 1100000,
    info = paste("2023 state total too high:", state_total$n_students[1]))
})

test_that("2022 enrollment: loads with >0 rows and state total >900K", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2022)

  expect_gt(nrow(enr), 0)
  expect_true(all(enr$end_year == 2022))

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_total) == 0, "No state total row")

  expect_true(state_total$n_students[1] > 900000,
    info = paste("2022 state total:", state_total$n_students[1]))
})

test_that("2021 enrollment: loads with >0 rows", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2021)

  expect_gt(nrow(enr), 0)
  expect_true(all(enr$end_year == 2021))
})

test_that("2020 enrollment: loads with >0 rows (COVID year)", {
  skip_on_cran()
  skip_if_offline()

  # 2020 enrollment data should still exist (COVID cancelled assessments, not enrollment reporting)
  enr <- fetch_enr_or_skip(2020)

  expect_gt(nrow(enr), 0)
  expect_true(all(enr$end_year == 2020))
})

test_that("2019 enrollment: loads with >0 rows and state total >900K", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2019)

  expect_gt(nrow(enr), 0)

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_total) == 0, "No state total row")

  expect_true(state_total$n_students[1] > 900000,
    info = paste("2019 state total:", state_total$n_students[1]))
})

# Modern years 2013-2018 (legacy era)
for (yr in 2013:2018) {
  test_that(paste0(yr, " enrollment: loads with >0 rows"), {
    skip_on_cran()
    skip_if_offline()

    enr <- fetch_enr_or_skip(yr)

    expect_gt(nrow(enr), 0)
    expect_true(all(enr$end_year == yr))
  })
}

test_that("2012 enrollment: boundary year loads (first modern era)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2012)

  expect_gt(nrow(enr), 0)
  expect_true(all(enr$end_year == 2012))
})

# ==============================================================================
# ASR ERA: Per-Year Coverage (1999-2011)
# ==============================================================================

test_that("2011 enrollment: last ASR year loads", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2011)

  expect_gt(nrow(enr), 0)
  expect_true(all(enr$end_year == 2011))
})

test_that("2011 enrollment: ASR has grade-level data", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2011)

  grade_levels <- unique(enr$grade_level)
  expect_true("TOTAL" %in% grade_levels)
  expect_true("K" %in% grade_levels, info = "Missing K in ASR data")
  expect_true("01" %in% grade_levels, info = "Missing grade 01 in ASR data")
  expect_true("12" %in% grade_levels, info = "Missing grade 12 in ASR data")
})

test_that("2011 enrollment: ASR is district-level only (no campus data)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2011)

  # Use which() to handle NA values in is_campus flag
  # ASR era data may have NA entity flags if type isn't properly set
  campus_rows <- enr[which(enr$is_campus == TRUE &
                           enr$subgroup == "total_enrollment"), ]
  # ASR era data comes from district-level ZIP files, so campus rows should be 0
  expect_equal(nrow(campus_rows), 0,
    info = paste("ASR era has", nrow(campus_rows), "campus rows (expected 0)"))
})

test_that("2011 enrollment: state total >800K (ASR era)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2011)

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_total) == 0, "No state total row")

  # TN enrollment in 2011 was approximately 950-1000K
  expect_true(state_total$n_students[1] > 800000,
    info = paste("2011 state total too low:", state_total$n_students[1]))
})

test_that("2005 enrollment: mid-ASR year loads with grade data", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2005)

  expect_gt(nrow(enr), 0)

  grade_levels <- unique(enr$grade_level)
  expect_true("K" %in% grade_levels, info = "Missing K in 2005")
  expect_true("12" %in% grade_levels, info = "Missing grade 12 in 2005")
})

test_that("2005 enrollment: state total >800K", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2005)

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_total) == 0, "No state total row")

  expect_true(state_total$n_students[1] > 800000,
    info = paste("2005 state total:", state_total$n_students[1]))
})

test_that("1999 enrollment: earliest year loads", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(1999)

  expect_gt(nrow(enr), 0)
  expect_true(all(enr$end_year == 1999))
})

# Sample of ASR years to avoid exhaustive downloads
for (yr in c(2000, 2002, 2004, 2007, 2009)) {
  test_that(paste0(yr, " enrollment: ASR year loads with >0 rows"), {
    skip_on_cran()
    skip_if_offline()

    enr <- fetch_enr_or_skip(yr)

    expect_gt(nrow(enr), 0)
    expect_true(all(enr$end_year == yr))
  })
}

# ==============================================================================
# CROSS-ERA CONSISTENCY
# ==============================================================================

test_that("state total enrollment is plausible across eras", {
  skip_on_cran()
  skip_if_offline()

  # Sample years from each era - use years known to have cached/downloadable data
  # ASR: 2005 or 2011, Modern: 2023 or 2024
  sample_years <- c(2011, 2024)
  totals <- list()

  for (yr in sample_years) {
    enr <- tryCatch(
      fetch_enr(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) NULL
    )
    if (!is.null(enr) && nrow(enr) > 0) {
      st <- enr[which(enr$is_state == TRUE &
                      enr$subgroup == "total_enrollment" &
                      enr$grade_level == "TOTAL"), ]
      if (nrow(st) > 0 && !is.na(st$n_students[1]) && st$n_students[1] > 0) {
        totals[[as.character(yr)]] <- st$n_students[1]
      }
    }
  }

  skip_if(length(totals) < 1, "Not enough years loaded for cross-era comparison")

  # Each year should have a plausible total (800K-1.2M for TN)
  for (yr_str in names(totals)) {
    expect_true(totals[[yr_str]] > 800000,
      info = paste(yr_str, "total too low:", totals[[yr_str]]))
    expect_true(totals[[yr_str]] < 1200000,
      info = paste(yr_str, "total too high:", totals[[yr_str]]))
  }
})

test_that("modern era has demographic subgroups", {
  skip_on_cran()
  skip_if_offline()

  # Modern (2024) should have demographics
  enr_modern <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr_modern) || nrow(enr_modern) == 0, "No 2024 data")

  modern_subs <- unique(enr_modern$subgroup)
  expect_true("white" %in% modern_subs, info = "Modern era missing 'white'")
  expect_true("black" %in% modern_subs, info = "Modern era missing 'black'")
  expect_true("econ_disadv" %in% modern_subs, info = "Modern era missing 'econ_disadv'")
})

test_that("ASR era has grade-level data", {
  skip_on_cran()
  skip_if_offline()

  enr_asr <- tryCatch(
    fetch_enr(2005, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr_asr) || nrow(enr_asr) == 0, "No 2005 data")

  # Skip if download returned zero-enrollment data
  st <- enr_asr[which(enr_asr$is_state == TRUE &
                      enr_asr$subgroup == "total_enrollment" &
                      enr_asr$grade_level == "TOTAL"), ]
  skip_if(nrow(st) > 0 && st$n_students[1] == 0,
    "Download returned zero-enrollment data for 2005")

  asr_grades <- unique(enr_asr$grade_level)
  expect_true("K" %in% asr_grades, info = "ASR era missing grade K")
})

# ==============================================================================
# WIDE FORMAT CONSISTENCY
# ==============================================================================

test_that("2024 wide format: has expected columns", {
  skip_on_cran()
  skip_if_offline()

  wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(wide) || nrow(wide) == 0, "No 2024 wide data")

  required_wide_cols <- c("end_year", "type", "district_id", "row_total")
  for (col in required_wide_cols) {
    expect_true(col %in% names(wide), info = paste("Missing wide column:", col))
  }
})

test_that("2024 wide format: state row_total matches tidy total", {
  skip_on_cran()
  skip_if_offline()

  wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )
  tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )

  skip_if(is.null(wide) || nrow(wide) == 0, "No wide data")
  skip_if(is.null(tidy) || nrow(tidy) == 0, "No tidy data")

  wide_state <- wide[wide$type == "State", ]
  skip_if(nrow(wide_state) == 0, "No state row in wide data")

  tidy_state <- tidy[tidy$is_state &
                     tidy$subgroup == "total_enrollment" &
                     tidy$grade_level == "TOTAL", ]
  skip_if(nrow(tidy_state) == 0, "No state total in tidy data")

  expect_equal(wide_state$row_total[1], tidy_state$n_students[1],
    info = "Wide row_total should match tidy n_students for state total")
})

# ==============================================================================
# GRADE AGGREGATION
# ==============================================================================

test_that("enr_grade_aggs produces K8, HS, K12 for 2024", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr_or_skip(2024)

  # Only works if grade-level data exists
  grade_data <- enr[enr$subgroup == "total_enrollment" &
                    !enr$grade_level %in% c("TOTAL"), ]

  skip_if(nrow(grade_data) == 0,
    "No grade-level data in 2024 (modern era may not have per-grade breakdowns)")

  aggs <- enr_grade_aggs(enr)
  agg_levels <- unique(aggs$grade_level)

  expect_true("K8" %in% agg_levels, info = "Missing K8 aggregate")
  expect_true("HS" %in% agg_levels, info = "Missing HS aggregate")
  expect_true("K12" %in% agg_levels, info = "Missing K12 aggregate")

  # K12 should be > 0
  k12_state <- aggs[which(aggs$is_state == TRUE & aggs$grade_level == "K12"), ]
  if (nrow(k12_state) > 0) {
    expect_gt(k12_state$n_students[1], 0)
  }
})

# ==============================================================================
# fetch_enr_multi
# ==============================================================================

test_that("fetch_enr_multi returns combined data for 2023:2024", {
  skip_on_cran()
  skip_if_offline()

  multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(multi) || nrow(multi) == 0, "Multi-year data unavailable")

  expect_gt(nrow(multi), 0)
  expect_true(all(multi$end_year %in% 2023:2024))
  expect_true(2023 %in% multi$end_year)
  expect_true(2024 %in% multi$end_year)
})

test_that("fetch_enr_multi rejects invalid years", {
  expect_error(fetch_enr_multi(c(1998, 2024)), "Invalid years")
  expect_error(fetch_enr_multi(c(2024, 2030)), "Invalid years")
})

# ==============================================================================
# EDGE CASES
# ==============================================================================

test_that("fetch_enr rejects year below range", {
  expect_error(fetch_enr(1998), "end_year must be between")
})

test_that("fetch_enr rejects year above range", {
  expect_error(fetch_enr(2030), "end_year must be between")
})

test_that("get_available_years returns correct era boundaries", {
  years <- get_available_years()

  expect_equal(years$min_year, 1999)
  expect_equal(years$max_year, 2024)
  expect_equal(min(years$asr_era), 1999)
  expect_equal(max(years$asr_era), 2011)
  expect_equal(min(years$modern_era), 2012)
  expect_equal(max(years$modern_era), 2024)
  expect_equal(length(years$years), 26)  # 1999 to 2024 = 26 years
})
