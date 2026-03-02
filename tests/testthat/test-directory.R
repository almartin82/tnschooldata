# Tests for school directory functions

test_that("extract_school_name removes trailing number", {
  expect_equal(
    extract_school_name("Central High School - 15"),
    "Central High School"
  )
  expect_equal(
    extract_school_name("Fairley High School - 2195"),
    "Fairley High School"
  )
  # Name without number suffix should be unchanged
  expect_equal(
    extract_school_name("Some School"),
    "Some School"
  )
})

test_that("build_directory_fetchxml creates valid XML", {
  xml <- build_directory_fetchxml(entity_type = "100000001", page_size = 100)
  expect_true(grepl("customertypecode", xml))
  expect_true(grepl("100000001", xml))
  expect_true(grepl('count="100"', xml))
  expect_true(grepl("link-entity", xml))
  expect_true(grepl("contact", xml))
})

test_that("process_directory handles empty input", {
  empty <- dplyr::tibble()
  result <- process_directory(empty)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("district_name" %in% names(result))
  expect_true("school_name" %in% names(result))
  expect_true("entity_type" %in% names(result))
})

test_that("fetch_directory returns data", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- fetch_directory(use_cache = FALSE)

  expect_s3_class(dir_data, "tbl_df")
  expect_gt(nrow(dir_data), 100)
  expect_true("district_name" %in% names(dir_data))
  expect_true("school_name" %in% names(dir_data))
  expect_true("entity_type" %in% names(dir_data))
  expect_true("county_name" %in% names(dir_data))
  expect_true("phone" %in% names(dir_data))
  expect_true("grades_served" %in% names(dir_data))
  expect_true("principal_name" %in% names(dir_data))
  expect_true("superintendent_name" %in% names(dir_data))

  # Should have both schools and districts

  expect_true("school" %in% dir_data$entity_type)
  expect_true("district" %in% dir_data$entity_type)

  # Schools should outnumber districts
  n_schools <- sum(dir_data$entity_type == "school")
  n_districts <- sum(dir_data$entity_type == "district")
  expect_gt(n_schools, n_districts)
  expect_gt(n_schools, 1000)  # TN has ~1800+ public schools
})

test_that("fetch_directory raw format works", {
  skip_on_cran()
  skip_if_offline()

  dir_raw <- fetch_directory(tidy = FALSE, use_cache = FALSE)

  expect_s3_class(dir_raw, "tbl_df")
  expect_gt(nrow(dir_raw), 100)

  # Raw format should have API field names
  expect_true("name" %in% names(dir_raw))
  expect_true("entity_type_code" %in% names(dir_raw))
})

test_that("directory cache works", {
  skip_on_cran()
  skip_if_offline()

  # First fetch caches the data
  dir1 <- fetch_directory(use_cache = TRUE)

  # Second fetch should use cache
  dir2 <- fetch_directory(use_cache = TRUE)

  expect_equal(nrow(dir1), nrow(dir2))

  # Clean up
  clear_directory_cache()
})
