# Tests for calculate_psst and calculate_psss functions

test_that("calculate_psst calculates correctly", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  psst <- calculate_psst(df, mean_effect)

  # PSST = (observed_prop) / (expected_prop)
  observed_count <- calculate_observed_significant(df)
  expected_count <- calculate_expected_significant(df, mean_effect)
  n_total <- nrow(df)

  observed_prop <- observed_count / n_total
  expected_prop <- expected_count / n_total
  expected_psst <- observed_prop / expected_prop

  expect_equal(psst, expected_psst, tolerance = 1e-6)
  expect_true(psst >= 0)
})

test_that("calculate_psst handles observed > expected", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(2.5, 2.5, 2.5), # All significant
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.0 # Low expected
  psst <- calculate_psst(df, mean_effect)

  # Observed = 3/3 = 1.0, expected <U+2248> 0.05, so PSST should be large (> 1)
  expect_true(psst > 1)
})

test_that("calculate_psst handles observed < expected", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Very small SE
    t_value = c(0.5, 0.5, 0.5), # None significant
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2 # High expected
  psst <- calculate_psst(df, mean_effect)

  # Observed = 0/3 = 0, expected <U+2248> 1.0, so PSST should be 0
  expect_equal(psst, 0, tolerance = 1e-6)
})

test_that("calculate_psst handles zero expected proportion", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(0.5, 0.5, 0.5), # None significant
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.0 # Very low expected
  psst <- calculate_psst(df, mean_effect)

  # Should return a valid result (numeric or NA)
  expect_true(is.numeric(psst) || is.na(psst))

  # If expected_prop is very close to 0 and observed is also 0, should return 1
  # If expected_prop is 0 but observed > 0, should return NA
  expected_count <- calculate_expected_significant(df, mean_effect)
  observed_count <- calculate_observed_significant(df)

  if (expected_count == 0) {
    if (observed_count == 0) {
      expect_equal(psst, 1.0)
    } else {
      expect_true(is.na(psst))
    }
  } else {
    # If expected is not zero, PSST should be a valid number
    expect_true(is.numeric(psst) && !is.na(psst))
  }
})

test_that("calculate_psst handles empty data frame", {
  df <- data.frame(
    effect = numeric(0),
    se = numeric(0),
    t_value = numeric(0),
    meta = character(0),
    study = character(0)
  )

  mean_effect <- 0.2
  psst <- calculate_psst(df, mean_effect)

  expect_true(is.na(psst))
})

test_that("calculate_psss calculates correctly", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  psss <- calculate_psss(df, mean_effect)

  # Psss = ESS / (1 - expected_prop)
  ess <- calculate_ess(df, mean_effect)
  expected_count <- calculate_expected_significant(df, mean_effect)
  n_total <- nrow(df)
  expected_prop <- expected_count / n_total
  expected_psss <- ess / (1 - expected_prop)

  expect_equal(psss, expected_psss, tolerance = 1e-6)
})

test_that("calculate_psss handles high expected proportion", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Very small SE
    t_value = c(2.0, 2.0, 2.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2 # High expected
  psss <- calculate_psss(df, mean_effect)

  # When expected_prop is close to 1, denominator is close to 0
  # Should handle this gracefully (may return NA or large value)
  expect_true(is.numeric(psss) || is.na(psss))
})

test_that("calculate_psss handles zero expected proportion", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(2.5, 2.5, 2.5), # All significant
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.0 # Very low expected
  psss <- calculate_psss(df, mean_effect)

  # When expected_prop <U+2248> 0, denominator <U+2248> 1, so Psss <U+2248> ESS
  ess <- calculate_ess(df, mean_effect)
  expect_true(abs(psss - ess) < 1) # Should be close to ESS
})

test_that("calculate_psss handles empty data frame", {
  df <- data.frame(
    effect = numeric(0),
    se = numeric(0),
    t_value = numeric(0),
    meta = character(0),
    study = character(0)
  )

  mean_effect <- 0.2
  psss <- calculate_psss(df, mean_effect)

  expect_true(is.na(psss))
})

test_that("calculate_psss handles division by zero", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Very small SE
    t_value = c(2.0, 2.0, 2.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  expected_count <- calculate_expected_significant(df, mean_effect)
  expected_prop <- expected_count / nrow(df)

  # If expected_prop is exactly 1, denominator is 0, should return NA
  if (abs(expected_prop - 1.0) < 1e-10) {
    psss <- calculate_psss(df, mean_effect)
    expect_true(is.na(psss))
  }
})
