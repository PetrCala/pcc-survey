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

  # Should have 5 rows (one per estimator)
  expect_equal(nrow(result), 5)

  # RE mean should be mean of 0.1, 0.2, 0.3 (excluding 0.15 from "All meta-analyses")
  re_row <- result[result$Estimator == "RE", ]
  expect_equal(re_row$Mean, mean(c(0.1, 0.2, 0.3)))
  expect_equal(re_row$Median, median(c(0.1, 0.2, 0.3)))
  expect_equal(re_row$SD, sd(c(0.1, 0.2, 0.3)))
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

  # Check RE statistics
  re_row <- result[result$Estimator == "RE", ]
  expect_equal(re_row$Mean, 0.2)
  expect_equal(re_row$Median, 0.2)
  expect_equal(re_row$SD, sd(c(0.1, 0.2, 0.3)), tolerance = 1e-10)

  # Check UWLS statistics
  uwls_row <- result[result$Estimator == "UWLS", ]
  expect_equal(uwls_row$Mean, 0.25)
  expect_equal(uwls_row$Median, 0.25)
  expect_equal(uwls_row$SD, sd(c(0.15, 0.25, 0.35)), tolerance = 1e-10)
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
  re_row <- result[result$Estimator == "RE", ]
  expect_equal(re_row$Mean, mean(c(0.1, 0.3)))
  expect_equal(re_row$Median, median(c(0.1, 0.3)))
  expect_equal(re_row$SD, sd(c(0.1, 0.3)), tolerance = 1e-10)

  # UWLS should use only non-NA values (0.15, 0.25)
  uwls_row <- result[result$Estimator == "UWLS", ]
  expect_equal(uwls_row$Mean, mean(c(0.15, 0.25)))
  expect_equal(uwls_row$Median, median(c(0.15, 0.25)))
  expect_equal(uwls_row$SD, sd(c(0.15, 0.25)), tolerance = 1e-10)

  # HSMA with all NA should return NA for all statistics
  hsma_row <- result[result$Estimator == "HSMA", ]
  expect_true(is.na(hsma_row$Mean))
  expect_true(is.na(hsma_row$Median))
  expect_true(is.na(hsma_row$SD))
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

  # Check column names
  expect_equal(colnames(result), c("Estimator", "Mean", "Median", "SD"))

  # Check that all estimators are present
  expect_true(all(c("RE", "UWLS", "UWLS3", "HSMA", "Fisher's z") %in% result$Estimator))

  # Check that all statistics are numeric
  expect_true(is.numeric(result$Mean))
  expect_true(is.numeric(result$Median))
  expect_true(is.numeric(result$SD))
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
  re_row <- result[result$Estimator == "RE", ]
  expect_equal(re_row$Mean, 0.1)
  expect_equal(re_row$Median, 0.1)
  # SD of single value is NA in R
  expect_true(is.na(re_row$SD))
})

test_that("save_estimator_summary writes file correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile()
  dir.create(test_dir)

  summary_df <- data.frame(
    Estimator = c("RE", "UWLS"),
    Mean = c(0.2, 0.25),
    Median = c(0.2, 0.25),
    SD = c(0.1, 0.1)
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
  expect_equal(colnames(saved_df), c("Estimator", "Mean", "Median", "SD"))
  expect_equal(saved_df$Estimator, c("RE", "UWLS"))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})
