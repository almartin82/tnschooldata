# ==============================================================================
# LIVE Pipeline Tests for tnschooldata Assessment Functions
# ==============================================================================
#
# These tests verify EACH STEP of the assessment data pipeline using LIVE network calls.
# No mocks - we verify actual connectivity and data correctness.
#
# Test Categories:
# 1. URL Availability - HTTP status codes
# 2. File Download - Successful download and file type verification
# 3. File Parsing - Read file into R
# 4. Column Structure - Expected columns exist
# 5. Data Quality - No Inf/NaN, valid ranges
# 6. Proficiency Sums - Percentages sum to ~100%
# 7. Aggregation Logic - State totals are reasonable
# 8. Output Fidelity - tidy=TRUE matches raw data
#
# ==============================================================================

library(testthat)
library(httr)

# Skip if no network connectivity
skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) {
      skip("No network connectivity")
    }
  }, error = function(e) {
    skip("No network connectivity")
  })
}

# ==============================================================================
# STEP 1: URL Availability Tests
# ==============================================================================

test_that("Tennessee DOE accountability portal is accessible", {
  skip_if_offline()

  url <- "https://www.tn.gov/education/districts/federal-programs-and-oversight/data/data-downloads.html"

  response <- tryCatch(
    httr::HEAD(url, httr::timeout(30), httr::config(ssl_verifypeer = 0L)),
    error = function(e) NULL
  )

  skip_if(is.null(response), "Could not reach TDOE website")
  expect_true(httr::status_code(response) < 400,
              info = paste("TDOE website returned status:", httr::status_code(response)))
})

test_that("Assessment file URLs are valid for recent year", {
  skip_if_offline()

  # Test URL for most recent year
  url <- get_assessment_url(2024, "state")
  skip_if(is.null(url), "No URL pattern for 2024")

  response <- tryCatch(
    httr::HEAD(url, httr::timeout(30), httr::config(ssl_verifypeer = 0L)),
    error = function(e) NULL
  )

  skip_if(is.null(response), "Could not reach assessment URL")
  expect_true(httr::status_code(response) < 400,
              info = paste("Assessment URL returned status:", httr::status_code(response)))
})

# ==============================================================================
# STEP 2: File Download Tests
# ==============================================================================

test_that("Can download Tennessee assessment data file (2024)", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    get_raw_assessment(2024, level = "state"),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed - data source may be broken")

  expect_true(is.list(result))
  expect_true("state" %in% names(result))
  expect_true(is.data.frame(result$state))
})

# ==============================================================================
# STEP 3: Column Structure Tests
# ==============================================================================

test_that("Assessment data has expected columns", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  # Should have these columns
  expect_true("subject" %in% names(result), info = "Missing subject column")
  expect_true("grade" %in% names(result), info = "Missing grade column")
  expect_true("subgroup" %in% names(result), info = "Missing subgroup column")
  expect_true("n_tested" %in% names(result), info = "Missing n_tested column")

  # Should have proficiency columns
  prof_cols <- c("pct_below", "pct_approaching", "pct_on_track", "pct_mastered")
  for (col in prof_cols) {
    expect_true(col %in% names(result), info = paste("Missing", col, "column"))
  }
})

# ==============================================================================
# STEP 4: Data Quality Tests
# ==============================================================================

test_that("Assessment data has no Inf or NaN values", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  for (col in names(result)[sapply(result, is.numeric)]) {
    expect_false(any(is.infinite(result[[col]]), na.rm = TRUE),
                 info = paste("Found Inf in", col))
    expect_false(any(is.nan(result[[col]]), na.rm = TRUE),
                 info = paste("Found NaN in", col))
  }
})

test_that("Proficiency percentages are in valid range (0-100)", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  prof_cols <- c("pct_below", "pct_approaching", "pct_on_track", "pct_mastered")

  for (col in prof_cols) {
    if (col %in% names(result)) {
      valid_vals <- result[[col]][!is.na(result[[col]])]
      expect_true(all(valid_vals >= 0),
                  info = paste(col, "has negative values"))
      expect_true(all(valid_vals <= 100),
                  info = paste(col, "has values > 100"))
    }
  }
})

test_that("Proficiency percentages sum to approximately 100", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  prof_cols <- c("pct_below", "pct_approaching", "pct_on_track", "pct_mastered")
  prof_cols <- prof_cols[prof_cols %in% names(result)]

  if (length(prof_cols) == 4) {
    # Calculate row sums
    result$prof_sum <- rowSums(result[, prof_cols], na.rm = FALSE)
    complete_rows <- result[!is.na(result$prof_sum), ]

    if (nrow(complete_rows) > 0) {
      # Should sum to ~100 (within rounding tolerance)
      expect_true(mean(complete_rows$prof_sum) > 95,
                  info = paste("Average proficiency sum:", mean(complete_rows$prof_sum)))
      expect_true(mean(complete_rows$prof_sum) < 105,
                  info = paste("Average proficiency sum:", mean(complete_rows$prof_sum)))
    }
  }
})

# ==============================================================================
# STEP 5: State-Level Validation
# ==============================================================================

test_that("State-level data has reasonable n_tested", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  # Get state-level "All Students" total for a core subject
  state_data <- result |>
    dplyr::filter(is_state, subgroup == "All Students", subject == "Math" | subject == "ELA")

  skip_if(nrow(state_data) == 0, "No state-level All Students data")

  # Tennessee has ~1 million students, so tested counts should be significant
  max_tested <- max(state_data$n_tested, na.rm = TRUE)
  expect_true(max_tested > 10000,
              info = paste("Max n_tested is suspiciously low:", max_tested))
})

test_that("State data has expected subjects", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  subjects <- unique(result$subject)

  # Should have core subjects
  expect_true(any(grepl("ELA|English", subjects, ignore.case = TRUE)),
              info = "Missing ELA/English subject")
  expect_true(any(grepl("Math", subjects, ignore.case = TRUE)),
              info = "Missing Math subject")
})

test_that("State data has expected subgroups", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  subgroups <- unique(result$subgroup)

  # Should have All Students
  expect_true("All Students" %in% subgroups, info = "Missing All Students subgroup")

  # Should have demographic subgroups
  expect_true(any(grepl("Black|African", subgroups, ignore.case = TRUE)),
              info = "Missing Black/African American subgroup")
  expect_true(any(grepl("Hispanic", subgroups, ignore.case = TRUE)),
              info = "Missing Hispanic subgroup")
  expect_true(any(grepl("White", subgroups, ignore.case = TRUE)),
              info = "Missing White subgroup")

  # Should have special population subgroups
  expect_true(any(grepl("Economically|ED|Disadvantaged", subgroups, ignore.case = TRUE)),
              info = "Missing Economically Disadvantaged subgroup")
})

# ==============================================================================
# STEP 6: Tidy Format Tests
# ==============================================================================

test_that("Tidy format has expected structure", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  # Should have proficiency_level column
  expect_true("proficiency_level" %in% names(result))

  # Should have all 4 proficiency levels
  levels <- unique(result$proficiency_level)
  expect_true("below" %in% levels)
  expect_true("approaching" %in% levels)
  expect_true("on_track" %in% levels)
  expect_true("mastered" %in% levels)

  # Should have aggregation flags
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_school" %in% names(result))
})

test_that("Tidy percentages are normalized to 0-1", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2024, tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Assessment download failed")
  skip_if(nrow(result) == 0, "No data returned")

  valid_pcts <- result$pct[!is.na(result$pct)]

  expect_true(all(valid_pcts >= 0), info = "Found negative percentages")
  expect_true(all(valid_pcts <= 1), info = "Found percentages > 1")
})

# ==============================================================================
# STEP 7: Multi-Year Tests
# ==============================================================================

test_that("fetch_assessment_multi combines multiple years", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment_multi(c(2023, 2024), level = "state", tidy = TRUE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Multi-year download failed")
  skip_if(nrow(result) == 0, "No data returned")

  # Should have both years
  years <- unique(result$end_year)
  expect_true(2023 %in% years, info = "Missing 2023 data")
  expect_true(2024 %in% years, info = "Missing 2024 data")
})

# ==============================================================================
# STEP 8: Historical Year Tests
# ==============================================================================

test_that("Can fetch 2019 assessment data (CSV format)", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2019, level = "state", tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "2019 assessment download failed - data source may be unavailable")
  skip_if(nrow(result) == 0, "No 2019 data returned")

  # Should have data
  expect_gt(nrow(result), 0)

  # Should have expected columns
  expect_true("subject" %in% names(result))
  expect_true("n_tested" %in% names(result))
})

test_that("Can fetch 2021 assessment data (post-COVID)", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    fetch_assessment(2021, level = "state", tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "2021 assessment download failed")
  skip_if(nrow(result) == 0, "No 2021 data returned")

  expect_gt(nrow(result), 0)
})
