# Tests for hsma function

test_that("hsma returns NA when all sample sizes are missing", {
  # Create a data frame with all NA sample sizes
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(NA_real_, NA_real_, NA_real_),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  result <- hsma(df)

  # Should return NA for both est and t_value
  expect_true(is.na(result$est))
  expect_true(is.na(result$t_value))
})

test_that("hsma calculates correctly with some missing sample sizes", {
  # Create a data frame with some NA sample sizes
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4),
    sample_size = c(50, NA_real_, 100, 200),
    meta = "test_meta",
    study = c("study1", "study2", "study3", "study4")
  )

  result <- hsma(df)

  # Should calculate using only non-NA sample sizes
  # r_avg = sum(n * effect) / sum(n) = (50*0.1 + 100*0.3 + 200*0.4) / (50+100+200)
  # = (5 + 30 + 80) / 350 = 115 / 350 = 0.3285714
  expected_r_avg <- (50 * 0.1 + 100 * 0.3 + 200 * 0.4) / (50 + 100 + 200)

  # Check that est is close to expected (allowing for floating point precision)
  expect_equal(result$est, expected_r_avg, tolerance = 1e-6)

  # t_value should be a numeric value (not NA)
  expect_true(is.numeric(result$t_value))
  expect_false(is.na(result$t_value))
})

test_that("hsma calculates correctly with no missing sample sizes", {
  # Create a data frame with no NA sample sizes
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

test_that("hsma errors when effect contains NA", {
  # Create a data frame with NA effect
  df <- data.frame(
    effect = c(0.1, NA_real_, 0.3),
    sample_size = c(50, 100, 150),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  # Should error because stopifnot(sum(is.na(df$effect)) == 0)
  expect_error(hsma(df), "sum\\(is.na\\(df\\$effect\\)\\) == 0 is not TRUE")
})
