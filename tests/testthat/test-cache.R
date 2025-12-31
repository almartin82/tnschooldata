# Tests for cache functions

test_that("get_cache_dir creates directory if needed", {
  cache_dir <- get_cache_dir()

  expect_true(is.character(cache_dir))
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("tnschooldata", cache_dir))
})

test_that("get_cache_path generates correct paths", {
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))

  path <- get_cache_path(2023, "wide")
  expect_true(grepl("enr_wide_2023.rds", path))
})

test_that("cache_exists returns FALSE for missing files", {
  # Year 9999 should never exist
  expect_false(cache_exists(9999, "tidy"))
  expect_false(cache_exists(9999, "wide"))
})

test_that("write_cache and read_cache roundtrip works", {
  # Create test data
  test_data <- data.frame(
    end_year = 2024,
    district_id = "0470",
    value = 100,
    stringsAsFactors = FALSE
  )

  # Write to cache using a test year
  test_year <- 9998
  path <- write_cache(test_data, test_year, "test")

  # Verify file exists
  expect_true(file.exists(path))

  # Read back
  result <- read_cache(test_year, "test")

  # Verify data matches
  expect_equal(result$end_year, test_data$end_year)
  expect_equal(result$district_id, test_data$district_id)
  expect_equal(result$value, test_data$value)

  # Clean up
  unlink(path)
})

test_that("cache_exists respects max_age", {
  # Create test data
  test_data <- data.frame(x = 1)

  # Write to cache
  test_year <- 9997
  path <- write_cache(test_data, test_year, "age_test")

  # Should exist with default max_age
  expect_true(cache_exists(test_year, "age_test"))

  # Should exist with very short max_age (just created)
  expect_true(cache_exists(test_year, "age_test", max_age = 0.01))

  # Clean up
  unlink(path)
})

test_that("clear_cache removes files", {
  # Create test data
  test_data <- data.frame(x = 1)

  # Write multiple cache files
  path1 <- write_cache(test_data, 9990, "tidy")
  path2 <- write_cache(test_data, 9990, "wide")
  path3 <- write_cache(test_data, 9991, "tidy")

  # Clear specific year and type
  expect_true(file.exists(path1))
  clear_cache(9990, "tidy")
  expect_false(file.exists(path1))
  expect_true(file.exists(path2))

  # Clear remaining test files
  clear_cache(9990, "wide")
  clear_cache(9991, "tidy")
})
