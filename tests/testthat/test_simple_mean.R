# Tests for simple_mean (OLS / simple unweighted average)

test_that("simple_mean returns the unweighted mean with SD/sqrt(k) SE", {
  effs <- c(0.1, 0.2, 0.3, 0.4)
  df <- data.frame(effect = effs)
  res <- simple_mean(df)

  expect_equal(res$est, mean(effs))

  expected_se <- stats::sd(effs) / sqrt(length(effs))
  expect_equal(res$t_value, mean(effs) / expected_se)
})

test_that("simple_mean t_value is NA for a single observation", {
  res <- simple_mean(data.frame(effect = 0.3))
  expect_equal(res$est, 0.3)
  expect_true(is.na(res$t_value))
})

test_that("simple_mean accepts an explicit effect vector", {
  res <- simple_mean(data.frame(effect = c(1, 2, 3)), effect = c(0.2, 0.4, 0.6))
  expect_equal(res$est, 0.4)
})
