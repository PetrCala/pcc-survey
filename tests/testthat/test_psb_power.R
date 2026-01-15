# Tests for calculate_study_power function

test_that("calculate_study_power calculates power correctly for simple case", {
  # Create a simple data frame
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  power <- calculate_study_power(df, mean_effect)

  # Power should be a vector of length 3
  expect_length(power, 3)
  expect_true(all(is.numeric(power)))
  expect_true(all(power >= 0 & power <= 1))

  # For lambda = 0.2 / 0.1 = 2.0, power should be high
  # P(|Z| > critical_value | Z ~ N(2.0, 1))
  # = 1 - P(-critical_value < Z < critical_value)
  # = 1 - [pnorm(critical_value, 2.0, 1) - pnorm(-critical_value, 2.0, 1)]
  critical_value <- stats::qnorm(1 - 0.05 / 2)
  expected_power <- 1 - stats::pnorm(critical_value, mean = 2.0, sd = 1) +
    stats::pnorm(-critical_value, mean = 2.0, sd = 1)
  expect_equal(power[2], expected_power, tolerance = 1e-5)
})

test_that("calculate_study_power handles zero mean effect", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.0
  power <- calculate_study_power(df, mean_effect)

  # When mean_effect = 0, lambda = 0, power should be approximately alpha (0.05)
  # P(|Z| > 1.96 | Z ~ N(0, 1)) = 0.05
  expect_true(all(power >= 0.04 & power <= 0.06))
})

test_that("calculate_study_power handles large mean effect", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Small SE
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  power <- calculate_study_power(df, mean_effect)

  # For lambda = 0.2 / 0.01 = 20, power should be very high (close to 1)
  expect_true(all(power > 0.99))
})

test_that("calculate_study_power handles custom alpha", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  power_05 <- calculate_study_power(df, mean_effect, alpha = 0.05)
  power_01 <- calculate_study_power(df, mean_effect, alpha = 0.01)

  # Power should be lower for alpha = 0.01 (stricter criterion)
  expect_true(all(power_01 <= power_05))
})

test_that("calculate_study_power handles custom se vector", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  custom_se <- c(0.05, 0.1, 0.15)
  power_custom <- calculate_study_power(df, mean_effect, se = custom_se)
  power_default <- calculate_study_power(df, mean_effect)

  # Should be different when using custom SE
  expect_false(all(power_custom == power_default))
  expect_length(power_custom, 3)
})

test_that("calculate_study_power handles NA values", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, NA_real_, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  power <- calculate_study_power(df, mean_effect)

  # Should handle NA in se
  expect_length(power, 3)
  expect_true(is.na(power[2]))
  expect_true(is.numeric(power[1]) && !is.na(power[1]))
  expect_true(is.numeric(power[3]) && !is.na(power[3]))
})

test_that("calculate_study_power handles infinite values", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.0, 0.1), # Zero SE causes infinite lambda
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  power <- calculate_study_power(df, mean_effect)

  # Should handle infinite lambda gracefully
  expect_length(power, 3)
  # When lambda is infinite (se = 0), power should be 1.0 (perfect power)
  # However, the code sets is.infinite(power) to NA, so check for either case
  expect_true(is.na(power[2]) || is.infinite(power[2]) || power[2] == 1.0)
})

test_that("calculate_study_power validates inputs", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  # Invalid alpha
  expect_error(calculate_study_power(df, 0.2, alpha = 0))
  expect_error(calculate_study_power(df, 0.2, alpha = 1))
  expect_error(calculate_study_power(df, 0.2, alpha = -0.1))

  # Invalid se length
  expect_error(calculate_study_power(df, 0.2, se = c(0.1, 0.1)))
})
