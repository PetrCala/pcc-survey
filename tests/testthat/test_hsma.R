# Tests for hsma function

test_that("hsma calculates correctly with valid sample sizes", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(50, 100, 150),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  result <- hsma(df)

  # r_avg = sum(n * effect) / sum(n) = (50*0.1 + 100*0.2 + 150*0.3) / (50+100+150)
  # = (5 + 20 + 45) / 300 = 70 / 300 = 0.2333333
  expected_r_avg <- (50 * 0.1 + 100 * 0.2 + 150 * 0.3) / (50 + 100 + 150)

  expect_equal(result$est, expected_r_avg, tolerance = 1e-6)

  # t_value should be a numeric value (not NA)
  expect_true(is.numeric(result$t_value))
  expect_false(is.na(result$t_value))
})

test_that("hsma weights by sample size correctly", {
  # Larger sample size should pull estimate toward that study's effect
  df <- data.frame(
    effect = c(0.1, 0.5),
    sample_size = c(10, 1000),
    meta = "test_meta",
    study = c("study1", "study2")
  )

  result <- hsma(df)

  # Estimate should be much closer to 0.5 (larger sample)
  expect_true(result$est > 0.45)
  expect_true(result$est < 0.55)
})

test_that("hsma errors when effect contains NA", {
  df <- data.frame(
    effect = c(0.1, NA_real_, 0.3),
    sample_size = c(50, 100, 150),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  expect_error(hsma(df))
})

test_that("hsma errors when sample_size is missing", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(NA_real_, NA_real_, NA_real_),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  expect_error(hsma(df))
})

test_that("hsma errors when sample_size column is absent", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  expect_error(hsma(df))
})
