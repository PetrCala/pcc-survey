# Tests for calculate_estimator_summary and save_estimator_summary functions

test_that("calculate_estimator_summary excludes 'All meta-analyses' row", {
  # Create test data with multiple meta-analyses and an "All meta-analyses" row
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3", "All meta-analyses"),
    re_est = c(0.1, 0.2, 0.3, 0.15),
    uwls_est = c(0.15, 0.25, 0.35, 0.2),
    uwls3_est = c(0.12, 0.22, 0.32, 0.18),
    hsma_est = c(0.11, 0.21, 0.31, 0.16),
    fishers_z_est = c(0.13, 0.23, 0.33, 0.19)
  )

  result <- calculate_estimator_summary(df)

  # Should have 10 rows (one per statistic)
  expect_equal(nrow(result), 10)

  # Should have Statistic column plus 5 estimator columns
  expect_equal(ncol(result), 6)

  # RE mean should be mean of 0.1, 0.2, 0.3 (excluding 0.15 from "All meta-analyses")
  mean_row <- result[result$Statistic == "Mean", ]
  expect_equal(mean_row$RE, mean(c(0.1, 0.2, 0.3)))
})

test_that("calculate_estimator_summary calculates correct statistics", {
  # Create test data
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3"),
    re_est = c(0.1, 0.2, 0.3),
    uwls_est = c(0.15, 0.25, 0.35),
    uwls3_est = c(0.12, 0.22, 0.32),
    hsma_est = c(0.11, 0.21, 0.31),
    fishers_z_est = c(0.13, 0.23, 0.33)
  )

  result <- calculate_estimator_summary(df)

  # Check count
  count_row <- result[result$Statistic == "count", ]
  expect_equal(count_row$RE, 3L)
  expect_equal(count_row$UWLS, 3L)

  # Check RE statistics
  expect_equal(result[result$Statistic == "Mean", ]$RE, 0.2)
  expect_equal(result[result$Statistic == "median", ]$RE, 0.2)
  expect_equal(result[result$Statistic == "SD", ]$RE, sd(c(0.1, 0.2, 0.3)), tolerance = 1e-10)

  # Check UWLS statistics
  expect_equal(result[result$Statistic == "Mean", ]$UWLS, 0.25)
  expect_equal(result[result$Statistic == "median", ]$UWLS, 0.25)
  expect_equal(result[result$Statistic == "SD", ]$UWLS, sd(c(0.15, 0.25, 0.35)), tolerance = 1e-10)
})

test_that("calculate_estimator_summary handles NA values correctly", {
  # Create test data with some NA values
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3"),
    re_est = c(0.1, NA_real_, 0.3),
    uwls_est = c(0.15, 0.25, NA_real_),
    uwls3_est = c(0.12, 0.22, 0.32),
    hsma_est = c(NA_real_, NA_real_, NA_real_),
    fishers_z_est = c(0.13, 0.23, 0.33)
  )

  result <- calculate_estimator_summary(df)

  # RE should use only non-NA values (0.1, 0.3)
  expect_equal(result[result$Statistic == "Mean", ]$RE, mean(c(0.1, 0.3)))
  expect_equal(result[result$Statistic == "median", ]$RE, median(c(0.1, 0.3)))
  expect_equal(result[result$Statistic == "SD", ]$RE, sd(c(0.1, 0.3)), tolerance = 1e-10)
  expect_equal(result[result$Statistic == "missing", ]$RE, 1L)

  # UWLS should use only non-NA values (0.15, 0.25)
  expect_equal(result[result$Statistic == "Mean", ]$UWLS, mean(c(0.15, 0.25)))
  expect_equal(result[result$Statistic == "median", ]$UWLS, median(c(0.15, 0.25)))
  expect_equal(result[result$Statistic == "SD", ]$UWLS, sd(c(0.15, 0.25)), tolerance = 1e-10)
  expect_equal(result[result$Statistic == "missing", ]$UWLS, 1L)

  # HSMA with all NA should return NA for numeric statistics
  expect_true(is.na(result[result$Statistic == "Mean", ]$HSMA))
  expect_true(is.na(result[result$Statistic == "median", ]$HSMA))
  expect_true(is.na(result[result$Statistic == "SD", ]$HSMA))
  expect_equal(result[result$Statistic == "missing", ]$HSMA, 3L)
})

test_that("calculate_estimator_summary has correct column names and structure", {
  df <- data.frame(
    meta = c("Meta1", "Meta2"),
    re_est = c(0.1, 0.2),
    uwls_est = c(0.15, 0.25),
    uwls3_est = c(0.12, 0.22),
    hsma_est = c(0.11, 0.21),
    fishers_z_est = c(0.13, 0.23)
  )

  result <- calculate_estimator_summary(df)

  # Check column names - should have Statistic plus estimator columns
  expect_true("Statistic" %in% colnames(result))
  expect_true(all(c("RE", "UWLS", "UWLS3", "HSMA", "Fisher's z") %in% colnames(result)))

  # Check that all statistics are present
  expected_stats <- c("count", "minimum", "max", "missing", "skewness", "median", "IQR", "trimmed_mean_10", "Mean", "SD")
  expect_equal(sort(result$Statistic), sort(expected_stats))

  # Check that count and missing have integer values (may be stored as double in data frame)
  count_val <- result[result$Statistic == "count", ]$RE
  missing_val <- result[result$Statistic == "missing", ]$RE
  expect_equal(count_val, as.integer(count_val)) # Value is an integer
  expect_equal(missing_val, as.integer(missing_val)) # Value is an integer
})

test_that("calculate_estimator_summary handles single meta-analysis", {
  # Edge case: only one meta-analysis (excluding "All meta-analyses")
  df <- data.frame(
    meta = c("Meta1", "All meta-analyses"),
    re_est = c(0.1, 0.15),
    uwls_est = c(0.15, 0.2),
    uwls3_est = c(0.12, 0.18),
    hsma_est = c(0.11, 0.16),
    fishers_z_est = c(0.13, 0.19)
  )

  result <- calculate_estimator_summary(df)

  # Should still work with single value
  expect_equal(result[result$Statistic == "Mean", ]$RE, 0.1)
  expect_equal(result[result$Statistic == "median", ]$RE, 0.1)
  # SD of single value is NA in R
  expect_true(is.na(result[result$Statistic == "SD", ]$RE))
  # Skewness of single value is NA
  expect_true(is.na(result[result$Statistic == "skewness", ]$RE))
  # IQR of single value is NA
  expect_true(is.na(result[result$Statistic == "IQR", ]$RE))
})

test_that("calculate_estimator_summary calculates IQR correctly", {
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3", "Meta4", "Meta5"),
    re_est = c(0.1, 0.2, 0.3, 0.4, 0.5),
    uwls_est = c(0.15, 0.25, 0.35, 0.45, 0.55),
    uwls3_est = c(0.12, 0.22, 0.32, 0.42, 0.52),
    hsma_est = c(0.11, 0.21, 0.31, 0.41, 0.51),
    fishers_z_est = c(0.13, 0.23, 0.33, 0.43, 0.53)
  )

  result <- calculate_estimator_summary(df)

  # IQR should be Q3 - Q1
  re_iqr <- result[result$Statistic == "IQR", ]$RE
  re_quantiles <- quantile(c(0.1, 0.2, 0.3, 0.4, 0.5), probs = c(0.25, 0.75))
  expected_iqr <- as.numeric(re_quantiles[2]) - as.numeric(re_quantiles[1])
  expect_equal(re_iqr, expected_iqr, tolerance = 1e-10)
})

test_that("calculate_estimator_summary calculates trimmed mean correctly", {
  df <- data.frame(
    meta = c("Meta1", "Meta2", "Meta3", "Meta4", "Meta5"),
    re_est = c(0.1, 0.2, 0.3, 0.4, 0.5),
    uwls_est = c(0.15, 0.25, 0.35, 0.45, 0.55),
    uwls3_est = c(0.12, 0.22, 0.32, 0.42, 0.52),
    hsma_est = c(0.11, 0.21, 0.31, 0.41, 0.51),
    fishers_z_est = c(0.13, 0.23, 0.33, 0.43, 0.53)
  )

  result <- calculate_estimator_summary(df)

  # Trimmed mean should match mean(..., trim = 0.1)
  re_trimmed <- result[result$Statistic == "trimmed_mean_10", ]$RE
  expected_trimmed <- mean(c(0.1, 0.2, 0.3, 0.4, 0.5), trim = 0.1)
  expect_equal(re_trimmed, expected_trimmed, tolerance = 1e-10)
})

test_that("save_estimator_summary writes file correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile()
  dir.create(test_dir)

  summary_df <- data.frame(
    Statistic = c("count", "Mean"),
    RE = c(3L, 0.2),
    UWLS = c(3L, 0.25)
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
  expect_true(all(c("RE", "UWLS") %in% colnames(saved_df)))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})
