# Tests for hsma function

test_that("hsma uses DOF when all sample sizes are missing", {
  # Create a data frame with all NA sample sizes but DOF available
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(NA_real_, NA_real_, NA_real_),
    dof = c(93, 193, 293),  # DOF available as substitute
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  result <- hsma(df)

  # Should calculate using DOF directly as n (per Tom Stanley's guidance)
  # r_avg = sum(dof * effect) / sum(dof) = (93*0.1 + 193*0.2 + 293*0.3) / (93+193+293)
  # = (9.3 + 38.6 + 87.9) / 579 = 135.8 / 579 = 0.2345423
  expected_r_avg <- (93 * 0.1 + 193 * 0.2 + 293 * 0.3) / (93 + 193 + 293)
  
  expect_equal(result$est, expected_r_avg, tolerance = 1e-6)
  expect_true(is.numeric(result$t_value))
  expect_false(is.na(result$t_value))
})

test_that("hsma calculates correctly with some missing sample sizes", {
  # Create a data frame with some NA sample sizes
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4),
    sample_size = c(50, NA_real_, 100, 200),
    dof = c(43, 93, 93, 193),  # DOF available as substitute
    meta = "test_meta",
    study = c("study1", "study2", "study3", "study4")
  )

  result <- hsma(df)

  # Should calculate using sample_size where available, DOF directly where missing
  # Values: 50 (sample_size), 93 (dof), 100 (sample_size), 200 (sample_size)
  # r_avg = sum(n * effect) / sum(n) = (50*0.1 + 93*0.2 + 100*0.3 + 200*0.4) / (50+93+100+200)
  # = (5 + 18.6 + 30 + 80) / 443 = 133.6 / 443 = 0.3015801
  expected_r_avg <- (50 * 0.1 + 93 * 0.2 + 100 * 0.3 + 200 * 0.4) / (50 + 93 + 100 + 200)

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
    dof = c(43, 93, 143),  # DOF available
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
    dof = c(43, 93, 143),  # DOF available
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  # Should error because stopifnot(sum(is.na(df$effect)) == 0)
  expect_error(hsma(df), "sum\\(is.na\\(df\\$effect\\)\\) == 0 is not TRUE")
})
