# Tests for enrollment functions
# Note: Most tests are marked as skip_on_cran since they require network access

test_that("safe_numeric handles various inputs", {
  # Normal numbers
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)

  # Suppressed values
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("**")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("< 5")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("N/A")))

  # Whitespace handling
  expect_equal(safe_numeric("  100  "), 100)

  # Empty input
  expect_equal(length(safe_numeric(NULL)), 0)
  expect_equal(length(safe_numeric(character(0))), 0)
})

test_that("pad_district_id creates 4-digit IDs", {
  expect_equal(pad_district_id("1"), "0001")
  expect_equal(pad_district_id("12"), "0012")
  expect_equal(pad_district_id("123"), "0123")
  expect_equal(pad_district_id("1234"), "1234")
  expect_equal(pad_district_id("0470"), "0470")
  expect_equal(pad_district_id("  470  "), "0470")
})

test_that("pad_school_id creates 4-digit IDs", {
  expect_equal(pad_school_id("1"), "0001")
  expect_equal(pad_school_id("12"), "0012")
  expect_equal(pad_school_id("123"), "0123")
  expect_equal(pad_school_id("1234"), "1234")
})

test_that("make_campus_id creates 8-digit IDs", {
  expect_equal(make_campus_id("470", "5"), "04700005")
  expect_equal(make_campus_id("0470", "0005"), "04700005")
  expect_equal(make_campus_id("1", "1"), "00010001")
})

test_that("get_available_years returns valid range", {
  years <- get_available_years()

  expect_true(is.list(years))
  expect_true("min_year" %in% names(years))
  expect_true("max_year" %in% names(years))
  expect_true("years" %in% names(years))
  expect_true("asr_era" %in% names(years))
  expect_true("modern_era" %in% names(years))

  # Should now include historical data back to 1999

  expect_true(years$min_year <= 1999)
  expect_true(years$max_year >= 2024)
  expect_equal(length(years$years), years$max_year - years$min_year + 1)

  # Check era boundaries
  expect_equal(min(years$asr_era), 1999)
  expect_equal(max(years$asr_era), 2011)
  expect_equal(min(years$modern_era), 2012)
})

test_that("fetch_enr validates year parameter", {
  # Years outside the valid range (1999-2024) should error
  expect_error(fetch_enr(1998), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
})

test_that("fetch_enr_multi validates year parameters", {
  # Years outside the valid range (1999-2024) should error
  expect_error(fetch_enr_multi(c(1998, 2024)), "Invalid years")
  expect_error(fetch_enr_multi(c(2024, 2030)), "Invalid years")
})

test_that("get_cache_dir returns valid path", {
  cache_dir <- get_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(grepl("tnschooldata", cache_dir))
})

test_that("cache functions work correctly", {
  # Test cache path generation
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))

  # Test cache_exists returns FALSE for non-existent cache
  # (Assuming no cache exists for year 9999)
  expect_false(cache_exists(9999, "tidy"))
})

test_that("build_tdoe_url constructs valid URLs", {
  url <- build_tdoe_url(2024, "membership")
  expect_true(grepl("tn.gov", url))
  expect_true(grepl("2024", url))
  expect_true(grepl("Membership", url))
})

test_that("create_empty_result returns correct structure", {
  result <- create_empty_result(2024, "Campus")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true("end_year" %in% names(result))
  expect_true("type" %in% names(result))
  expect_true("district_id" %in% names(result))
  expect_true("campus_id" %in% names(result))
  expect_true("row_total" %in% names(result))
})

# Integration tests (require network access)
test_that("fetch_enr downloads and processes data", {
  skip_on_cran()
  skip_if_offline()

  # Use a recent year
  result <- tryCatch(
    fetch_enr(2023, tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  # Skip if download fails (data may not be available)
  skip_if(is.null(result), "Data download failed - may not be available")

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("district_id" %in% names(result))
  expect_true("type" %in% names(result))

  # Check we have state level at minimum
  if (nrow(result) > 0) {
    expect_true("State" %in% result$type)
  }
})

test_that("fetch_enr downloads historical ASR data", {
  skip_on_cran()
  skip_if_offline()

  # Test a historical year (ASR era)
  result <- tryCatch(
    fetch_enr(2005, tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  # Skip if download fails (ASR server may have issues)
  skip_if(is.null(result), "ASR data download failed - server may be unavailable")

  # Check structure - ASR data is district-level only
  expect_true(is.data.frame(result))

  # Should have district names at minimum
  if (nrow(result) > 0) {
    expect_true("district_name" %in% names(result) || any(grepl("district", names(result), ignore.case = TRUE)))
  }
})

test_that("tidy_enr produces correct long format", {
  skip_on_cran()
  skip_if_offline()

  # Create sample wide data for testing
  wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "0470",
    campus_id = NA_character_,
    district_name = "Knox County",
    campus_name = NA_character_,
    county = "Knox",
    region = "2",
    row_total = 1000,
    white = 500,
    black = 200,
    hispanic = 150,
    asian = 50,
    native_american = 10,
    pacific_islander = 5,
    multiracial = 85,
    male = 510,
    female = 490,
    econ_disadv = 400,
    lep = 50,
    special_ed = 100,
    grade_pk = 50,
    grade_k = 80,
    grade_01 = 75,
    grade_02 = 78,
    grade_03 = 80,
    grade_04 = 77,
    grade_05 = 76,
    grade_06 = 79,
    grade_07 = 81,
    grade_08 = 82,
    grade_09 = 85,
    grade_10 = 83,
    grade_11 = 80,
    grade_12 = 94,
    stringsAsFactors = FALSE
  )

  # Tidy it
  tidy_result <- tidy_enr(wide)

  # Check structure
  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check subgroups include expected values
  subgroups <- unique(tidy_result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
  expect_true("hispanic" %in% subgroups)
  expect_true("white" %in% subgroups)

  # Check grade levels
  grade_levels <- unique(tidy_result$grade_level)
  expect_true("TOTAL" %in% grade_levels)
  expect_true("K" %in% grade_levels)
  expect_true("09" %in% grade_levels)
})

test_that("id_enr_aggs adds correct flags", {
  # Create sample tidy data
  tidy <- data.frame(
    end_year = 2024,
    type = c("State", "District", "Campus"),
    district_id = c(NA, "0470", "0470"),
    campus_id = c(NA, NA, "04700005"),
    grade_level = "TOTAL",
    subgroup = "total_enrollment",
    n_students = c(1000000, 50000, 500),
    pct = 1.0,
    stringsAsFactors = FALSE
  )

  result <- id_enr_aggs(tidy)

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_campus))

  # Check mutual exclusivity (each row is only one type)
  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1))

  # Check correct assignment
  expect_true(result$is_state[result$type == "State"])
  expect_true(result$is_district[result$type == "District"])
  expect_true(result$is_campus[result$type == "Campus"])
})

test_that("enr_grade_aggs creates correct aggregations", {
  # Create sample tidy data with grade levels
  tidy <- data.frame(
    end_year = rep(2024, 15),
    type = rep("District", 15),
    district_id = rep("0470", 15),
    campus_id = rep(NA_character_, 15),
    district_name = rep("Knox County", 15),
    campus_name = rep(NA_character_, 15),
    county = rep("Knox", 15),
    region = rep("2", 15),
    grade_level = c("TOTAL", "PK", "K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    subgroup = rep("total_enrollment", 15),
    n_students = c(1000, 50, 80, 75, 78, 80, 77, 76, 79, 81, 82, 85, 83, 80, 94),
    pct = rep(1.0, 15),
    is_state = rep(FALSE, 15),
    is_district = rep(TRUE, 15),
    is_campus = rep(FALSE, 15),
    stringsAsFactors = FALSE
  )

  result <- enr_grade_aggs(tidy)

  # Check we got K8, HS, K12 aggregations
  grade_levels <- unique(result$grade_level)
  expect_true("K8" %in% grade_levels)
  expect_true("HS" %in% grade_levels)
  expect_true("K12" %in% grade_levels)

  # Check K-8 sum (grades K, 01-08)
  k8_sum <- sum(c(80, 75, 78, 80, 77, 76, 79, 81, 82))  # K through 8
  k8_result <- result$n_students[result$grade_level == "K8"]
  expect_equal(k8_result, k8_sum)

  # Check HS sum (grades 09-12)
  hs_sum <- sum(c(85, 83, 80, 94))
  hs_result <- result$n_students[result$grade_level == "HS"]
  expect_equal(hs_result, hs_sum)

  # Check K-12 sum
  k12_sum <- k8_sum + hs_sum
  k12_result <- result$n_students[result$grade_level == "K12"]
  expect_equal(k12_result, k12_sum)
})


# ==============================================================================
# DATA FIDELITY TESTS
# These tests ensure tidy=TRUE output maintains fidelity to raw, unprocessed data
# ==============================================================================

test_that("modern year (2024) data has no Inf/NaN percentages", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  # Check for Inf/NaN percentages
  inf_nan_count <- sum(is.infinite(result$pct) | is.nan(result$pct), na.rm = TRUE)
  expect_equal(inf_nan_count, 0,
    info = paste("Found", inf_nan_count, "Inf/NaN percentages in 2024 data"))
})

test_that("modern year (2024) state-level enrollment is non-zero", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  # Get state total enrollment
  state_total <- result[result$is_state &
                        result$subgroup == "total_enrollment" &
                        result$grade_level == "TOTAL", ]

  expect_true(nrow(state_total) >= 1, info = "No state total enrollment row found")
  expect_true(state_total$n_students[1] > 0,
    info = paste("State enrollment is 0 or negative:", state_total$n_students[1]))
  # Tennessee has approximately 1 million students
  expect_true(state_total$n_students[1] > 500000,
    info = paste("State enrollment implausibly low:", state_total$n_students[1]))
})

test_that("modern year (2024) has expected subgroups", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  subgroups <- unique(result$subgroup)

  # Should have total enrollment
  expect_true("total_enrollment" %in% subgroups)

  # Should have demographic subgroups (from percentage columns)
  expect_true("white" %in% subgroups, info = "Missing white subgroup")
  expect_true("black" %in% subgroups, info = "Missing black subgroup")
  expect_true("hispanic" %in% subgroups, info = "Missing hispanic subgroup")
  expect_true("asian" %in% subgroups, info = "Missing asian subgroup")

  # Should have special population subgroups
  expect_true("econ_disadv" %in% subgroups, info = "Missing econ_disadv subgroup")
  expect_true("lep" %in% subgroups, info = "Missing lep subgroup")
  expect_true("special_ed" %in% subgroups, info = "Missing special_ed subgroup")
})

test_that("modern year (2023) data has no Inf/NaN percentages", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2023, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  inf_nan_count <- sum(is.infinite(result$pct) | is.nan(result$pct), na.rm = TRUE)
  expect_equal(inf_nan_count, 0,
    info = paste("Found", inf_nan_count, "Inf/NaN percentages in 2023 data"))
})

test_that("modern year (2023) state-level enrollment is non-zero", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2023, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  state_total <- result[result$is_state &
                        result$subgroup == "total_enrollment" &
                        result$grade_level == "TOTAL", ]

  expect_true(nrow(state_total) >= 1)
  expect_true(state_total$n_students[1] > 500000,
    info = paste("State enrollment implausibly low:", state_total$n_students[1]))
})

test_that("ASR era (2005) data has no Inf/NaN percentages", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2005, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "ASR data download failed - server may be unavailable")

  inf_nan_count <- sum(is.infinite(result$pct) | is.nan(result$pct), na.rm = TRUE)
  expect_equal(inf_nan_count, 0,
    info = paste("Found", inf_nan_count, "Inf/NaN percentages in 2005 data"))
})

test_that("ASR era (2005) state-level enrollment is non-zero", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2005, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "ASR data download failed - server may be unavailable")

  state_total <- result[result$is_state &
                        result$subgroup == "total_enrollment" &
                        result$grade_level == "TOTAL", ]

  expect_true(nrow(state_total) >= 1)
  expect_true(state_total$n_students[1] > 500000,
    info = paste("2005 state enrollment implausibly low:", state_total$n_students[1]))
})

test_that("ASR era (2005) has grade-level data", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2005, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "ASR data download failed - server may be unavailable")

  grade_levels <- unique(result$grade_level)

  # ASR data should have grade levels
  expect_true("TOTAL" %in% grade_levels)
  expect_true("K" %in% grade_levels, info = "Missing kindergarten grade level")
  expect_true("01" %in% grade_levels, info = "Missing grade 01")
  expect_true("12" %in% grade_levels, info = "Missing grade 12")
})

test_that("district counts match state total (modern year)", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  state_row <- result[result$type == "State", ]
  district_rows <- result[result$type == "District", ]

  skip_if(nrow(state_row) == 0, "No state row found")
  skip_if(nrow(district_rows) == 0, "No district rows found")

  state_total <- state_row$row_total[1]
  district_sum <- sum(district_rows$row_total, na.rm = TRUE)

  # They should be close (may differ slightly due to rounding in pct conversion)
  pct_diff <- abs(state_total - district_sum) / state_total * 100
  expect_true(pct_diff < 5,
    info = paste("State total", state_total, "differs from district sum", district_sum,
                 "by", round(pct_diff, 2), "%"))
})

test_that("no impossible zeros in demographic counts (modern year)", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  state_row <- result[result$type == "State", ]
  skip_if(nrow(state_row) == 0, "No state row found")

  # State should have non-zero values for major demographics
  if ("white" %in% names(state_row)) {
    expect_true(state_row$white[1] > 0, info = "State white count is 0")
  }
  if ("black" %in% names(state_row)) {
    expect_true(state_row$black[1] > 0, info = "State black count is 0")
  }
  if ("hispanic" %in% names(state_row)) {
    expect_true(state_row$hispanic[1] > 0, info = "State hispanic count is 0")
  }
})

test_that("percentages are valid (0-1 range for subgroups)", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(result), "Data download failed")

  # Filter to non-total subgroups (total_enrollment should be 1.0)
  subgroup_data <- result[result$subgroup != "total_enrollment", ]
  subgroup_data <- subgroup_data[!is.na(subgroup_data$pct), ]

  # Percentages should be between 0 and 1 (inclusive)
  expect_true(all(subgroup_data$pct >= 0),
    info = "Found negative percentages")
  expect_true(all(subgroup_data$pct <= 1),
    info = "Found percentages > 1 (100%)")
})
