# Tests for pcc_sum_stats function

test_that("pcc_sum_stats returns all NA when all sample sizes are missing", {
  # Create a data frame with all NA sample sizes
  df <- data.frame(
    sample_size = c(NA_real_, NA_real_, NA_real_),
    effect = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07)
  )

  result <- pcc_sum_stats(df, log_results = FALSE)

  # All statistics should be NA (not NaN)
  expect_equal(result$k_, 3)
  expect_true(is.na(result$avg_n))
  expect_true(is.na(result$median_n))
  expect_true(is.na(result$quantile_1_n))
  expect_true(is.na(result$quantile_3_n))
  expect_true(is.na(result$ss_lt_50))
  expect_true(is.na(result$ss_lt_100))
  expect_true(is.na(result$ss_lt_200))
  expect_true(is.na(result$ss_lt_400))
  expect_true(is.na(result$ss_lt_1600))
  expect_true(is.na(result$ss_lt_3200))

  # Ensure avg_n is NA, not NaN
  expect_false(is.nan(result$avg_n))
})

test_that("pcc_sum_stats calculates correctly with some missing sample sizes", {
  # Create a data frame with some NA sample sizes
  df <- data.frame(
    sample_size = c(30, NA_real_, 100, 200, NA_real_),
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.05, 0.06, 0.07, 0.08, 0.09)
  )

  result <- pcc_sum_stats(df, log_results = FALSE)

  # k_ should be total number of rows
  expect_equal(result$k_, 5)

  # avg_n should be mean of non-NA values
  expect_equal(result$avg_n, mean(c(30, 100, 200), na.rm = TRUE))

  # median_n should be median of non-NA values
  expect_equal(result$median_n, stats::median(c(30, 100, 200), na.rm = TRUE))

  # ss_lt statistics should be calculated correctly
  expect_equal(result$ss_lt_50, 1 / 5) # 1 value < 50 out of 5 total (30)
  expect_equal(result$ss_lt_100, 1 / 5) # 1 value < 100 out of 5 total (30)
  expect_equal(result$ss_lt_200, 2 / 5) # 2 values < 200 out of 5 total (30, 100)
})

test_that("pcc_sum_stats calculates correctly with no missing sample sizes", {
  # Create a data frame with no NA sample sizes
  df <- data.frame(
    sample_size = c(30, 100, 150, 200, 250),
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.05, 0.06, 0.07, 0.08, 0.09)
  )

  result <- pcc_sum_stats(df, log_results = FALSE)

  # k_ should be total number of rows
  expect_equal(result$k_, 5)

  # avg_n should be mean of all values
  expect_equal(result$avg_n, mean(c(30, 100, 150, 200, 250)))

  # median_n should be median of all values
  expect_equal(result$median_n, stats::median(c(30, 100, 150, 200, 250)))

  # quantiles should be calculated correctly
  quantiles <- stats::quantile(c(30, 100, 150, 200, 250), probs = c(0.25, 0.75))
  expect_equal(result$quantile_1_n, as.numeric(quantiles[1]))
  expect_equal(result$quantile_3_n, as.numeric(quantiles[2]))

  # ss_lt statistics should be calculated correctly
  expect_equal(result$ss_lt_50, 1 / 5) # 1 value < 50 (30)
  expect_equal(result$ss_lt_100, 1 / 5) # 1 value < 100 (30)
  expect_equal(result$ss_lt_200, 3 / 5) # 3 values < 200 (30, 100, 150)
  expect_equal(result$ss_lt_400, 5 / 5) # 5 values < 400 (all)
})

test_that("pcc_sum_stats handles empty data frame", {
  # Create an empty data frame
  df <- data.frame(
    sample_size = numeric(0),
    effect = numeric(0),
    se = numeric(0)
  )

  result <- pcc_sum_stats(df, log_results = FALSE)

  # k_ should be 0
  expect_equal(result$k_, 0)

  # All statistics should be NA
  expect_true(is.na(result$avg_n))
  expect_true(is.na(result$median_n))
  expect_true(is.na(result$quantile_1_n))
  expect_true(is.na(result$quantile_3_n))
})
