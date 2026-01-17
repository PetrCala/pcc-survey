# Tests for calculate_estimator_summary and save_estimator_summary functions

test_that("calculate_estimator_summary excludes 'All meta-analyses' row", {
  # Create test data with multiple meta-analyses and an "All meta-analyses" row
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3", "All meta-analyses"),
    re1_est = c(0.11, 0.21, 0.31, 0.16),
    re2_est = c(0.12, 0.22, 0.32, 0.17),
    uwls1_est = c(0.16, 0.26, 0.36, 0.21),
    uwls2_est = c(0.17, 0.27, 0.37, 0.22),
    uwls3_est = c(0.12, 0.22, 0.32, 0.18),
    hsma_est = c(0.11, 0.21, 0.31, 0.16),
    fishers_z_est = c(0.13, 0.23, 0.33, 0.19),
    waiv2_est = c(0.14, 0.24, 0.34, 0.20)
  )

  result <- calculate_estimator_summary(df)

  # Should have 10 rows (one per statistic)
  expect_equal(nrow(result), 10)

  # Should have Statistic column plus 8 estimator columns
  expect_equal(ncol(result), 9)

  # RE1 mean should be mean of 0.11, 0.21, 0.31 (excluding 0.16 from "All meta-analyses")
  mean_row <- result[result$Statistic == "Mean", ]
  expect_equal(mean_row$RE1, mean(c(0.11, 0.21, 0.31)))
})

test_that("calculate_estimator_summary calculates correct statistics", {
  # Create test data
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3"),
    re1_est = c(0.11, 0.21, 0.31),
    re2_est = c(0.12, 0.22, 0.32),
    uwls1_est = c(0.16, 0.26, 0.36),
    uwls2_est = c(0.17, 0.27, 0.37),
    uwls3_est = c(0.12, 0.22, 0.32),
    hsma_est = c(0.11, 0.21, 0.31),
    fishers_z_est = c(0.13, 0.23, 0.33),
    waiv2_est = c(0.14, 0.24, 0.34)
  )

  result <- calculate_estimator_summary(df)

  # Check count
  count_row <- result[result$Statistic == "count", ]
  expect_equal(count_row$RE1, 3L)
  expect_equal(count_row$RE2, 3L)
  expect_equal(count_row$UWLS1, 3L)
  expect_equal(count_row$UWLS2, 3L)

  # Check RE1 statistics
  expect_equal(result[result$Statistic == "Mean", ]$RE1, 0.21)
  expect_equal(result[result$Statistic == "median", ]$RE1, 0.21)
  expect_equal(result[result$Statistic == "SD", ]$RE1, sd(c(0.11, 0.21, 0.31)), tolerance = 1e-10)

  # Check UWLS1 statistics
  expect_equal(result[result$Statistic == "Mean", ]$UWLS1, 0.26)
  expect_equal(result[result$Statistic == "median", ]$UWLS1, 0.26)
  expect_equal(result[result$Statistic == "SD", ]$UWLS1, sd(c(0.16, 0.26, 0.36)), tolerance = 1e-10)
})

test_that("calculate_estimator_summary handles NA values correctly", {
  # Create test data with some NA values
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3"),
    re1_est = c(0.11, NA_real_, 0.31),
    re2_est = c(0.12, 0.22, 0.32),
    uwls1_est = c(0.16, 0.26, NA_real_),
    uwls2_est = c(0.17, 0.27, 0.37),
    uwls3_est = c(0.12, 0.22, 0.32),
    hsma_est = c(NA_real_, NA_real_, NA_real_),
    fishers_z_est = c(0.13, 0.23, 0.33),
    waiv2_est = c(0.14, 0.24, 0.34)
  )

  result <- calculate_estimator_summary(df)

  # RE1 should use only non-NA values (0.11, 0.31)
  expect_equal(result[result$Statistic == "Mean", ]$RE1, mean(c(0.11, 0.31)))
  expect_equal(result[result$Statistic == "median", ]$RE1, median(c(0.11, 0.31)))
  expect_equal(result[result$Statistic == "SD", ]$RE1, sd(c(0.11, 0.31)), tolerance = 1e-10)
  expect_equal(result[result$Statistic == "missing", ]$RE1, 1L)

  # UWLS1 should use only non-NA values (0.16, 0.26)
  expect_equal(result[result$Statistic == "Mean", ]$UWLS1, mean(c(0.16, 0.26)))
  expect_equal(result[result$Statistic == "median", ]$UWLS1, median(c(0.16, 0.26)))
  expect_equal(result[result$Statistic == "SD", ]$UWLS1, sd(c(0.16, 0.26)), tolerance = 1e-10)
  expect_equal(result[result$Statistic == "missing", ]$UWLS1, 1L)

  # HSMA with all NA should return NA for numeric statistics
  expect_true(is.na(result[result$Statistic == "Mean", ]$HSMA))
  expect_true(is.na(result[result$Statistic == "median", ]$HSMA))
  expect_true(is.na(result[result$Statistic == "SD", ]$HSMA))
  expect_equal(result[result$Statistic == "missing", ]$HSMA, 3L)
})

test_that("calculate_estimator_summary has correct column names and structure", {
  df <- data.frame(
    meta = c("Meta1", "Meta2"),
    re1_est = c(0.11, 0.21),
    re2_est = c(0.12, 0.22),
    uwls1_est = c(0.16, 0.26),
    uwls2_est = c(0.17, 0.27),
    uwls3_est = c(0.12, 0.22),
    hsma_est = c(0.11, 0.21),
    fishers_z_est = c(0.13, 0.23),
    waiv2_est = c(0.14, 0.24)
  )

  result <- calculate_estimator_summary(df)

  # Check column names - should have Statistic plus estimator columns
  expect_true("Statistic" %in% colnames(result))
  expect_true(all(c("RE1", "RE2", "UWLS1", "UWLS2", "UWLS3", "HSMA", "Fisher's z", "WAIV2") %in% colnames(result)))

  # Check that all statistics are present
  expected_stats <- c("count", "minimum", "max", "missing", "skewness", "median", "IQR", "trimmed_mean_10", "Mean", "SD")
  expect_equal(sort(result$Statistic), sort(expected_stats))

  # Check that count and missing have integer values (may be stored as double in data frame)
  count_val <- result[result$Statistic == "count", ]$RE1
  missing_val <- result[result$Statistic == "missing", ]$RE1
  expect_equal(count_val, as.integer(count_val)) # Value is an integer
  expect_equal(missing_val, as.integer(missing_val)) # Value is an integer
})

test_that("calculate_estimator_summary handles single meta-analysis", {
  # Edge case: only one meta-analysis (excluding "All meta-analyses")
  df <- data.frame(
    meta = c("Meta1", "All meta-analyses"),
    re1_est = c(0.11, 0.16),
    re2_est = c(0.12, 0.17),
    uwls1_est = c(0.16, 0.21),
    uwls2_est = c(0.17, 0.22),
    uwls3_est = c(0.12, 0.18),
    hsma_est = c(0.11, 0.16),
    fishers_z_est = c(0.13, 0.19),
    waiv2_est = c(0.14, 0.20)
  )

  result <- calculate_estimator_summary(df)

  # Should still work with single value
  expect_equal(result[result$Statistic == "Mean", ]$RE1, 0.11)
  expect_equal(result[result$Statistic == "median", ]$RE1, 0.11)
  # SD of single value is NA in R
  expect_true(is.na(result[result$Statistic == "SD", ]$RE1))
  # Skewness of single value is NA
  expect_true(is.na(result[result$Statistic == "skewness", ]$RE1))
  # IQR of single value is NA
  expect_true(is.na(result[result$Statistic == "IQR", ]$RE1))
})

test_that("calculate_estimator_summary calculates IQR correctly", {
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3", "Meta4", "Meta5"),
    re1_est = c(0.11, 0.21, 0.31, 0.41, 0.51),
    re2_est = c(0.12, 0.22, 0.32, 0.42, 0.52),
    uwls1_est = c(0.16, 0.26, 0.36, 0.46, 0.56),
    uwls2_est = c(0.17, 0.27, 0.37, 0.47, 0.57),
    uwls3_est = c(0.12, 0.22, 0.32, 0.42, 0.52),
    hsma_est = c(0.11, 0.21, 0.31, 0.41, 0.51),
    fishers_z_est = c(0.13, 0.23, 0.33, 0.43, 0.53),
    waiv2_est = c(0.14, 0.24, 0.34, 0.44, 0.54)
  )

  result <- calculate_estimator_summary(df)

  # IQR should be Q3 - Q1
  re1_iqr <- result[result$Statistic == "IQR", ]$RE1
  re1_quantiles <- quantile(c(0.11, 0.21, 0.31, 0.41, 0.51), probs = c(0.25, 0.75))
  expected_iqr <- as.numeric(re1_quantiles[2]) - as.numeric(re1_quantiles[1])
  expect_equal(re1_iqr, expected_iqr, tolerance = 1e-10)
})

test_that("calculate_estimator_summary calculates trimmed mean correctly", {
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3", "Meta4", "Meta5"),
    re1_est = c(0.11, 0.21, 0.31, 0.41, 0.51),
    re2_est = c(0.12, 0.22, 0.32, 0.42, 0.52),
    uwls1_est = c(0.16, 0.26, 0.36, 0.46, 0.56),
    uwls2_est = c(0.17, 0.27, 0.37, 0.47, 0.57),
    uwls3_est = c(0.12, 0.22, 0.32, 0.42, 0.52),
    hsma_est = c(0.11, 0.21, 0.31, 0.41, 0.51),
    fishers_z_est = c(0.13, 0.23, 0.33, 0.43, 0.53),
    waiv2_est = c(0.14, 0.24, 0.34, 0.44, 0.54)
  )

  result <- calculate_estimator_summary(df)

  # Trimmed mean should match mean(..., trim = 0.1)
  re1_trimmed <- result[result$Statistic == "trimmed_mean_10", ]$RE1
  expected_trimmed <- mean(c(0.11, 0.21, 0.31, 0.41, 0.51), trim = 0.1)
  expect_equal(re1_trimmed, expected_trimmed, tolerance = 1e-10)
})

test_that("save_estimator_summary writes file correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile()
  dir.create(test_dir)

  summary_df <- data.frame(
    Statistic = c("count", "Mean"),
    RE1 = c(3L, 0.21),
    RE2 = c(3L, 0.22),
    UWLS1 = c(3L, 0.26),
    UWLS2 = c(3L, 0.27)
  )

  # Save the file
  save_estimator_summary(
    summary_df = summary_df,
    file_name = "test_summary.csv",
    output_dir = test_dir
  )

  # Check that file was created
  output_path <- file.path(test_dir, "test_summary.csv")
  expect_true(file.exists(output_path))

  # Read and verify contents
  saved_df <- read.csv(output_path)
  expect_equal(nrow(saved_df), 2)
  expect_true("Statistic" %in% colnames(saved_df))
  expect_true(all(c("RE1", "RE2", "UWLS1", "UWLS2") %in% colnames(saved_df)))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})
