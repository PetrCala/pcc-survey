# Tests for calculate_falsely_positive function

test_that("calculate_falsely_positive calculates correctly", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  falsely_positive <- calculate_falsely_positive(df, mean_effect)

  # Falsely positive = ESS / total_studies
  ess <- calculate_ess(df, mean_effect)
  n_total <- nrow(df)
  expected_fp <- ess / n_total

  expect_equal(falsely_positive, expected_fp, tolerance = 1e-6)
  expect_true(falsely_positive >= -1 && falsely_positive <= 1) # Proportion
})

test_that("calculate_falsely_positive returns positive when ESS > 0", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(2.5, 2.5, 2.5), # All significant
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.0 # Low expected
  falsely_positive <- calculate_falsely_positive(df, mean_effect)

  # ESS should be positive, so falsely_positive should be positive
  expect_true(falsely_positive > 0)
})

test_that("calculate_falsely_positive returns negative when ESS < 0", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Very small SE
    t_value = c(0.5, 0.5, 0.5), # None significant
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2 # High expected
  falsely_positive <- calculate_falsely_positive(df, mean_effect)

  # ESS should be negative, so falsely_positive should be negative
  expect_true(falsely_positive < 0)
})

test_that("calculate_falsely_positive handles zero ESS", {
  # This is unlikely but test the edge case
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 1.0, 1.0), # None significant
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.0 # Very low expected
  falsely_positive <- calculate_falsely_positive(df, mean_effect)

  # Should be close to 0 (may be slightly negative)
  expect_true(abs(falsely_positive) < 0.1)
})

test_that("calculate_falsely_positive handles empty data frame", {
  df <- data.frame(
    effect = numeric(0),
    se = numeric(0),
    t_value = numeric(0),
    meta = character(0),
    study = character(0)
  )

  mean_effect <- 0.2
  falsely_positive <- calculate_falsely_positive(df, mean_effect)

  expect_true(is.na(falsely_positive))
})

test_that("calculate_falsely_positive handles custom se vector", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  custom_se <- c(0.05, 0.1, 0.15)
  fp_custom <- calculate_falsely_positive(df, mean_effect, se = custom_se)
  fp_default <- calculate_falsely_positive(df, mean_effect)

  expect_false(fp_custom == fp_default)
})

test_that("calculate_falsely_positive handles custom alpha", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(2.0, 2.5, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  fp_05 <- calculate_falsely_positive(df, mean_effect, alpha = 0.05)
  fp_01 <- calculate_falsely_positive(df, mean_effect, alpha = 0.01)

  expect_false(fp_05 == fp_01)
})
