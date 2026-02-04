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

  # quantiles should be calculated correctly
  quantiles <- stats::quantile(c(30, 100, 150, 200, 250), probs = c(0.25, 0.75))
  expect_equal(result$quantile_1_n, as.numeric(quantiles[1]))
  expect_equal(result$quantile_3_n, as.numeric(quantiles[2]))

  # ss_lt statistics should be calculated correctly
  expect_equal(result$ss_lt_50, 1 / 5)  # 1 value < 50 (30)
  expect_equal(result$ss_lt_100, 1 / 5)  # 1 value < 100 (30)
  expect_equal(result$ss_lt_200, 3 / 5)  # 3 values < 200 (30, 100, 150)
  expect_equal(result$ss_lt_400, 5 / 5)  # 5 values < 400 (all)
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
  expect_true("quantile_1_n" %in% names(result))
  expect_true("quantile_3_n" %in% names(result))
  expect_true("ss_lt_50" %in% names(result))
  expect_true("ss_lt_100" %in% names(result))
  expect_true("ss_lt_200" %in% names(result))
  expect_true("ss_lt_400" %in% names(result))
  expect_true("ss_lt_1600" %in% names(result))
  expect_true("ss_lt_3200" %in% names(result))
})

test_that("pcc_sum_stats ss_lt thresholds are correct", {
  df <- data.frame(
    sample_size = c(25, 75, 150, 350, 1500, 3000),
    effect = rep(0.1, 6),
    se = rep(0.05, 6)
  )

  result <- pcc_sum_stats(df, log_results = FALSE)

  expect_equal(result$ss_lt_50, 1 / 6)     # 25
  expect_equal(result$ss_lt_100, 2 / 6)    # 25, 75
  expect_equal(result$ss_lt_200, 3 / 6)    # 25, 75, 150
  expect_equal(result$ss_lt_400, 4 / 6)    # 25, 75, 150, 350
  expect_equal(result$ss_lt_1600, 5 / 6)   # 25, 75, 150, 350, 1500
  expect_equal(result$ss_lt_3200, 6 / 6)   # all
})
