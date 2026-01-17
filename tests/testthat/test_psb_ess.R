# Tests for calculate_ess function

test_that("calculate_ess calculates correctly", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    dof = c(43, 93, 143)
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test

  ess <- calculate_ess(df, uwls_estimate, tau2)

  # ESS = observed - expected
  observed <- calculate_observed_significant(df)
  expected <- calculate_expected_significant(df, uwls_estimate, tau2)
  expected_ess <- observed - expected

  expect_equal(ess, expected_ess, tolerance = 1e-6)
})

test_that("calculate_ess returns positive when observed > expected", {
  # Create scenario with high observed significance
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(2.5, 2.5, 2.5), # All significant
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    dof = c(43, 93, 143)
  )

  uwls_estimate <- 0.0 # Low mean effect, so low expected significance
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  ess <- calculate_ess(df, uwls_estimate, tau2)

  # Observed = 3, expected should be low, so ESS should be positive
  expect_true(ess > 0)
})

test_that("calculate_ess returns negative when observed < expected", {
  # Create scenario with low observed significance
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Very small SE
    t_value = c(0.5, 0.5, 0.5), # None significant
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    dof = c(43, 93, 143)
  )

  uwls_estimate <- 0.2 # High mean effect, so high expected significance
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  ess <- calculate_ess(df, uwls_estimate, tau2)

  # Observed = 0, expected should be high, so ESS should be negative
  expect_true(ess < 0)
})

test_that("calculate_ess handles zero ESS", {
  # Create scenario where observed <U+2248> expected
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 1.0, 1.0), # None significant
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    dof = c(43, 93, 143)
  )

  uwls_estimate <- 0.0 # Zero mean effect, so expected should be low
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  ess <- calculate_ess(df, uwls_estimate, tau2)

  # Observed = 0, expected should be low, so ESS should be negative
  expect_true(ess < 0)
  expect_true(abs(ess) < 1)
})

test_that("calculate_ess handles custom se vector", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    dof = c(43, 93, 143)
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  custom_se <- c(0.05, 0.1, 0.15)
  ess_custom <- calculate_ess(df, uwls_estimate, tau2, se = custom_se)
  ess_default <- calculate_ess(df, uwls_estimate, tau2)

  # Should be different when using custom SE
  expect_false(ess_custom == ess_default)
})

test_that("calculate_ess handles custom alpha", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(2.0, 2.5, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    dof = c(43, 93, 143)
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  ess_05 <- calculate_ess(df, uwls_estimate, tau2, alpha = 0.05)
  ess_01 <- calculate_ess(df, uwls_estimate, tau2, alpha = 0.01)

  # Different alpha should give different ESS
  expect_false(ess_05 == ess_01)
})

test_that("calculate_ess handles NA values", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, NA_real_, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    dof = c(43, 93, 143)
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  ess <- calculate_ess(df, uwls_estimate, tau2)

  # Should handle NA gracefully
  expect_true(is.numeric(ess))
  expect_false(is.na(ess))
})
