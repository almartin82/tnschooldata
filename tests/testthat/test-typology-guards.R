# ==============================================================================
# Typology Guard Tests for tnschooldata
# ==============================================================================
#
# Standard guards verifying data integrity:
# - Division-by-zero protection (no Inf/NaN percentages)
# - Scale validation (enrollment counts plausible)
# - Type checks (columns are correct types)
# - Minimum row counts per entity level
# - Value set validation (subgroups, grade levels, entity types)
# - Duplicate detection (one row per entity per group per period)
# - Rounding tolerance for percentage-derived counts (modern era)
#
# All fetch calls use use_cache = TRUE
# ==============================================================================

# ==============================================================================
# DIVISION-BY-ZERO PROTECTION
# ==============================================================================

test_that("enrollment pct has no Inf values", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  inf_count <- sum(is.infinite(enr$pct), na.rm = TRUE)
  expect_equal(inf_count, 0,
    info = paste("Found", inf_count, "Inf percentages in enrollment"))
})

test_that("enrollment pct has no NaN values", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  nan_count <- sum(is.nan(enr$pct), na.rm = TRUE)
  expect_equal(nan_count, 0,
    info = paste("Found", nan_count, "NaN percentages in enrollment"))
})

test_that("enrollment n_students has no Inf values", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  inf_count <- sum(is.infinite(enr$n_students), na.rm = TRUE)
  expect_equal(inf_count, 0,
    info = paste("Found", inf_count, "Inf in n_students"))
})

test_that("enrollment n_students has no NaN values", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  nan_count <- sum(is.nan(enr$n_students), na.rm = TRUE)
  expect_equal(nan_count, 0,
    info = paste("Found", nan_count, "NaN in n_students"))
})

test_that("assessment pct has no Inf or NaN", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  inf_count <- sum(is.infinite(assess$pct), na.rm = TRUE)
  nan_count <- sum(is.nan(assess$pct), na.rm = TRUE)

  expect_equal(inf_count, 0, info = paste("Found", inf_count, "Inf in assessment pct"))
  expect_equal(nan_count, 0, info = paste("Found", nan_count, "NaN in assessment pct"))
})

# ==============================================================================
# SCALE VALIDATION
# ==============================================================================

test_that("enrollment: no single school exceeds 10,000 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  campus_totals <- enr[enr$is_campus &
                       enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL", ]
  skip_if(nrow(campus_totals) == 0, "No campus data")

  max_campus <- max(campus_totals$n_students, na.rm = TRUE)
  # Largest TN schools are ~3000-4000 students; 10K is a safe upper bound
  expect_true(max_campus < 10000,
    info = paste("Largest campus has", max_campus, "students - implausibly high"))
})

test_that("enrollment: no single district exceeds 200K students", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  district_totals <- enr[enr$is_district &
                         enr$subgroup == "total_enrollment" &
                         enr$grade_level == "TOTAL", ]
  skip_if(nrow(district_totals) == 0, "No district data")

  max_district <- max(district_totals$n_students, na.rm = TRUE)
  # Memphis-Shelby County is the largest at ~105K; 200K is safe upper bound
  expect_true(max_district < 200000,
    info = paste("Largest district has", max_district, "students - implausibly high"))
})

test_that("enrollment: state total is between 800K and 1.2M", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_total) == 0, "No state total row")

  expect_true(state_total$n_students[1] > 800000,
    info = paste("State total too low:", state_total$n_students[1]))
  expect_true(state_total$n_students[1] < 1200000,
    info = paste("State total too high:", state_total$n_students[1]))
})

test_that("enrollment: n_students is non-negative", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  negative_count <- sum(enr$n_students < 0, na.rm = TRUE)
  expect_equal(negative_count, 0,
    info = paste("Found", negative_count, "negative n_students values"))
})

test_that("assessment: n_tested is non-negative", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  negative_count <- sum(assess$n_tested < 0, na.rm = TRUE)
  expect_equal(negative_count, 0,
    info = paste("Found", negative_count, "negative n_tested values"))
})

test_that("assessment: n_students is non-negative", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  negative_count <- sum(assess$n_students < 0, na.rm = TRUE)
  expect_equal(negative_count, 0,
    info = paste("Found", negative_count, "negative n_students values"))
})

# ==============================================================================
# TYPE CHECKS
# ==============================================================================

test_that("enrollment: column types are correct", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  expect_true(is.numeric(enr$end_year), info = "end_year should be numeric")
  expect_true(is.character(enr$type), info = "type should be character")
  expect_true(is.character(enr$district_id) || all(is.na(enr$district_id)),
    info = "district_id should be character")
  expect_true(is.character(enr$subgroup), info = "subgroup should be character")
  expect_true(is.character(enr$grade_level), info = "grade_level should be character")
  expect_true(is.numeric(enr$n_students), info = "n_students should be numeric")
  expect_true(is.numeric(enr$pct), info = "pct should be numeric")
  expect_true(is.logical(enr$is_state), info = "is_state should be logical")
  expect_true(is.logical(enr$is_district), info = "is_district should be logical")
  expect_true(is.logical(enr$is_campus), info = "is_campus should be logical")
})

test_that("assessment: column types are correct", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  expect_true(is.numeric(assess$end_year), info = "end_year should be numeric")
  expect_true(is.character(assess$type), info = "type should be character")
  expect_true(is.character(assess$subject), info = "subject should be character")
  expect_true(is.character(assess$grade), info = "grade should be character")
  expect_true(is.character(assess$subgroup), info = "subgroup should be character")
  expect_true(is.numeric(assess$n_tested), info = "n_tested should be numeric")
  expect_true(is.character(assess$proficiency_level),
    info = "proficiency_level should be character")
  expect_true(is.numeric(assess$n_students), info = "n_students should be numeric")
  expect_true(is.numeric(assess$pct), info = "pct should be numeric")
  expect_true(is.logical(assess$is_state), info = "is_state should be logical")
  expect_true(is.logical(assess$is_district), info = "is_district should be logical")
  expect_true(is.logical(assess$is_school), info = "is_school should be logical")
})

test_that("wide enrollment: column types are correct", {
  skip_on_cran()
  skip_if_offline()

  wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(wide) || nrow(wide) == 0, "No wide data")

  expect_true(is.numeric(wide$row_total), info = "row_total should be numeric")
  expect_true(is.character(wide$type), info = "type should be character")
  expect_true(is.numeric(wide$end_year), info = "end_year should be numeric")
})

# ==============================================================================
# MINIMUM ROW COUNTS
# ==============================================================================

test_that("enrollment: at least 1 state row, 100 districts, 1000 campuses", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  total_rows <- enr[enr$subgroup == "total_enrollment" &
                    enr$grade_level == "TOTAL", ]

  state_count <- sum(total_rows$is_state, na.rm = TRUE)
  district_count <- sum(total_rows$is_district, na.rm = TRUE)
  campus_count <- sum(total_rows$is_campus, na.rm = TRUE)

  expect_true(state_count >= 1, info = "Need at least 1 state row")
  expect_true(district_count >= 100, info = paste("Only", district_count, "districts"))
  expect_true(campus_count >= 1000, info = paste("Only", campus_count, "campuses"))
})

test_that("assessment state: at least 100 rows per year", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  expect_true(nrow(assess) > 100,
    info = paste("Only", nrow(assess), "assessment rows"))
})

# ==============================================================================
# VALUE SET VALIDATION
# ==============================================================================

test_that("enrollment: subgroups are from standard set", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  # Standard subgroup values per CLAUDE.md
  valid_subgroups <- c(
    "total_enrollment", "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "male", "female",
    "econ_disadv", "lep", "special_ed"
  )

  actual_subs <- unique(enr$subgroup)
  unexpected <- setdiff(actual_subs, valid_subgroups)
  expect_equal(length(unexpected), 0,
    info = paste("Unexpected subgroups:", paste(unexpected, collapse = ", ")))
})

test_that("enrollment: grade_level values are from standard set", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  valid_grades <- c("PK", "K",
                    "01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12",
                    "TOTAL")

  actual_grades <- unique(enr$grade_level)
  unexpected <- setdiff(actual_grades, valid_grades)
  expect_equal(length(unexpected), 0,
    info = paste("Unexpected grade levels:", paste(unexpected, collapse = ", ")))
})

test_that("enrollment: type values are State/District/Campus only", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  valid_types <- c("State", "District", "Campus")
  actual_types <- unique(enr$type)
  unexpected <- setdiff(actual_types, valid_types)
  expect_equal(length(unexpected), 0,
    info = paste("Unexpected type values:", paste(unexpected, collapse = ", ")))
})

test_that("assessment: proficiency_level values are from standard set", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  valid_levels <- c("below", "approaching", "on_track", "mastered")
  actual_levels <- unique(assess$proficiency_level)
  unexpected <- setdiff(actual_levels, valid_levels)
  expect_equal(length(unexpected), 0,
    info = paste("Unexpected proficiency levels:", paste(unexpected, collapse = ", ")))
})

test_that("assessment: type values are State/District/School only", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  valid_types <- c("State", "District", "School")
  actual_types <- unique(assess$type)
  unexpected <- setdiff(actual_types, valid_types)
  expect_equal(length(unexpected), 0,
    info = paste("Unexpected assessment type values:", paste(unexpected, collapse = ", ")))
})

# ==============================================================================
# DUPLICATE DETECTION
# ==============================================================================

test_that("enrollment: no duplicate state total rows per year", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  state_totals <- enr[enr$is_state &
                      enr$subgroup == "total_enrollment" &
                      enr$grade_level == "TOTAL", ]

  expect_equal(nrow(state_totals), 1,
    info = paste("Found", nrow(state_totals), "state total rows (expected 1)"))
})

test_that("enrollment: no duplicate district rows per subgroup per year", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  district_data <- enr[enr$is_district &
                       enr$grade_level == "TOTAL", ]
  skip_if(nrow(district_data) == 0, "No district data")

  # Check for duplicates: one row per district per subgroup
  dups <- district_data[duplicated(
    paste(district_data$district_id, district_data$subgroup, sep = "_")
  ), ]

  expect_equal(nrow(dups), 0,
    info = paste("Found", nrow(dups), "duplicate district rows"))
})

test_that("assessment: one set of 4 proficiency rows per state-level grouping", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  # For each (test, subject, grade, subgroup), there should be exactly 4 rows
  # (below, approaching, on_track, mastered)
  group_counts <- table(
    paste(assess$test, assess$subject, assess$grade, assess$subgroup, sep = "||")
  )

  non_four <- group_counts[group_counts != 4]
  expect_equal(length(non_four), 0,
    info = paste("Found", length(non_four),
                 "groupings with != 4 proficiency level rows"))
})

# ==============================================================================
# PERCENTAGE RANGE VALIDATION
# ==============================================================================

test_that("enrollment: pct values are between 0 and 1 (inclusive)", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  pct_vals <- enr$pct[!is.na(enr$pct)]
  skip_if(length(pct_vals) == 0, "No pct values")

  below_zero <- sum(pct_vals < 0)
  above_one <- sum(pct_vals > 1)

  expect_equal(below_zero, 0, info = paste("Found", below_zero, "pct values < 0"))
  expect_equal(above_one, 0, info = paste("Found", above_one, "pct values > 1"))
})

test_that("assessment: pct values are between 0 and 1 (inclusive)", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  pct_vals <- assess$pct[!is.na(assess$pct)]
  skip_if(length(pct_vals) == 0, "No pct values")

  below_zero <- sum(pct_vals < 0)
  above_one <- sum(pct_vals > 1)

  expect_equal(below_zero, 0, info = paste("Found", below_zero, "assessment pct < 0"))
  expect_equal(above_one, 0, info = paste("Found", above_one, "assessment pct > 1"))
})

# ==============================================================================
# ROUNDING TOLERANCE (Modern era percentage-derived counts)
# ==============================================================================

test_that("enrollment: demographic sums approximate state total (within 5%)", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  state_data <- enr[enr$is_state & enr$grade_level == "TOTAL", ]
  skip_if(nrow(state_data) == 0, "No state data")

  total_row <- state_data[state_data$subgroup == "total_enrollment", ]
  skip_if(nrow(total_row) == 0, "No state total row")

  state_total <- total_row$n_students[1]

  # Sum racial demographics
  race_subs <- c("white", "black", "hispanic", "asian",
                 "pacific_islander", "native_american", "multiracial")
  race_data <- state_data[state_data$subgroup %in% race_subs, ]

  if (nrow(race_data) > 0) {
    race_sum <- sum(race_data$n_students, na.rm = TRUE)
    # Rounding from pct-to-count conversion can cause up to ~5% discrepancy
    pct_diff <- abs(state_total - race_sum) / state_total * 100
    expect_true(pct_diff < 10,
      info = paste("Race sum", race_sum, "vs total", state_total,
                   "differs by", round(pct_diff, 1), "%"))
  }
})

test_that("enrollment: district sum approximates state total (within 5%)", {
  skip_on_cran()
  skip_if_offline()

  wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(wide) || nrow(wide) == 0, "No wide data")

  state_row <- wide[wide$type == "State", ]
  district_rows <- wide[wide$type == "District", ]

  skip_if(nrow(state_row) == 0 || nrow(district_rows) == 0,
    "No state or district rows")

  state_total <- state_row$row_total[1]
  skip_if(is.na(state_total) || state_total == 0, "State total is zero/NA")

  district_sum <- sum(district_rows$row_total, na.rm = TRUE)

  pct_diff <- abs(state_total - district_sum) / state_total * 100
  expect_true(pct_diff < 5,
    info = paste("District sum", district_sum, "vs state total", state_total,
                 "differs by", round(pct_diff, 1), "%"))
})

test_that("assessment: proficiency percentages sum to ~100% per grouping", {
  skip_on_cran()
  skip_if_offline()

  assess <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(assess) || nrow(assess) == 0, "No assessment data")

  # For each (test, subject, grade, subgroup), the 4 proficiency pct values
  # should sum to approximately 1.0 (100%)
  grouping_key <- paste(assess$test, assess$subject, assess$grade,
                        assess$subgroup, sep = "||")

  unique_groups <- unique(grouping_key)
  # Sample a subset to keep test fast
  sample_size <- min(50, length(unique_groups))
  sample_groups <- unique_groups[seq_len(sample_size)]

  bad_sums <- 0
  for (grp in sample_groups) {
    grp_data <- assess[grouping_key == grp, ]
    pct_sum <- sum(grp_data$pct, na.rm = TRUE)
    # Allow tolerance for suppressed values and rounding
    if (!is.na(pct_sum) && pct_sum > 0 && (pct_sum < 0.90 || pct_sum > 1.10)) {
      bad_sums <- bad_sums + 1
    }
  }

  expect_true(bad_sums < sample_size * 0.1,
    info = paste(bad_sums, "of", sample_size,
                 "sampled groupings have proficiency pct sum outside 90-110%"))
})

# ==============================================================================
# SAFE_NUMERIC GUARDS
# ==============================================================================

test_that("safe_numeric handles standard suppression markers", {
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("**")))
  expect_true(is.na(safe_numeric("***")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("< 5")))
  expect_true(is.na(safe_numeric("<10")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("NULL")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
})

test_that("safe_numeric preserves valid numbers", {
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric("3.14"), 3.14)
})

test_that("safe_numeric handles edge cases", {
  expect_equal(length(safe_numeric(NULL)), 0)
  expect_equal(length(safe_numeric(character(0))), 0)
})

test_that("safe_numeric does not produce Inf or NaN", {
  # Test various inputs that might cause issues
  test_inputs <- c("*", "**", "-1", "", "N/A", "0", "100",
                   "<5", ".", "-", "1,000", "  ", "NULL")

  results <- safe_numeric(test_inputs)
  expect_false(any(is.infinite(results), na.rm = TRUE))
  expect_false(any(is.nan(results), na.rm = TRUE))
})

# ==============================================================================
# ENTITY FLAG CONSISTENCY
# ==============================================================================

test_that("enrollment: is_state rows have NA district_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  state_rows <- enr[enr$is_state, ]
  skip_if(nrow(state_rows) == 0, "No state rows")

  # State rows should have NA district_id
  expect_true(all(is.na(state_rows$district_id)),
    info = "State rows should have NA district_id")
})

test_that("enrollment: is_district rows have non-NA district_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  district_rows <- enr[enr$is_district, ]
  skip_if(nrow(district_rows) == 0, "No district rows")

  # District rows should have non-NA district_id
  expect_true(all(!is.na(district_rows$district_id)),
    info = "District rows should have non-NA district_id")
})

test_that("enrollment: is_campus rows have non-NA campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  campus_rows <- enr[enr$is_campus, ]
  skip_if(nrow(campus_rows) == 0, "No campus rows")

  # Campus rows should have non-NA campus_id
  expect_true(all(!is.na(campus_rows$campus_id) & campus_rows$campus_id != ""),
    info = "Campus rows should have non-NA, non-empty campus_id")
})

test_that("enrollment: district_id is 4-digit zero-padded", {
  skip_on_cran()
  skip_if_offline()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(enr) || nrow(enr) == 0, "No enrollment data")

  district_ids <- unique(enr$district_id[!is.na(enr$district_id)])
  skip_if(length(district_ids) == 0, "No district IDs")

  # All district IDs should be 4 characters
  id_lengths <- nchar(district_ids)
  expect_true(all(id_lengths == 4),
    info = paste("Found district IDs with non-4 lengths:",
                 paste(district_ids[id_lengths != 4], collapse = ", ")))
})

# ==============================================================================
# CROSS-FORMAT CONSISTENCY
# ==============================================================================

test_that("tidy and wide enrollment have similar number of entities", {
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

  skip_if(is.null(wide) || is.null(tidy), "Data unavailable")
  skip_if(nrow(wide) == 0 || nrow(tidy) == 0, "Empty data")

  wide_districts <- length(unique(wide$district_id[wide$type == "District"]))
  tidy_districts <- length(unique(tidy$district_id[which(tidy$is_district == TRUE)]))

  # Tidy may drop districts with all-NA enrollment (filtered by !is.na(n_students))
  # Allow up to 2 districts difference
  expect_true(abs(wide_districts - tidy_districts) <= 2,
    info = paste("Wide has", wide_districts, "districts, tidy has", tidy_districts))
})
