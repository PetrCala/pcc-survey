# Tests for pcc_sum_stats function

test_that("pcc_sum_stats calculates correctly with valid sample sizes", {
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
})

test_that("pcc_sum_stats errors when sample_size column is missing", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07)
  )

  expect_error(pcc_sum_stats(df, log_results = FALSE))
})

test_that("pcc_sum_stats errors when sample_size contains NA", {
  df <- data.frame(
    sample_size = c(30, NA_real_, 150),
    effect = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07)
  )

  expect_error(pcc_sum_stats(df, log_results = FALSE))
})

test_that("pcc_sum_stats returns correct structure", {
  df <- data.frame(
    sample_size = c(100, 200, 300),
    effect = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07)
  )

  result <- pcc_sum_stats(df, log_results = FALSE)

  expect_true("k_" %in% names(result))
  expect_true("avg_n" %in% names(result))
  expect_true("median_n" %in% names(result))

  # Exploratory sample-size quantiles and ss_lt_* shares were removed.
  expect_false("quantile_1_n" %in% names(result))
  expect_false("ss_lt_50" %in% names(result))
})
