# Tests for calculate_observed_significant and calculate_expected_significant

test_that("calculate_observed_significant counts correctly (positive only)", {
  df <- data.frame(
    t_value = c(0.5, 1.5, 2.0, 2.5, -1.0, -2.0, -2.5),
    dof = c(50, 50, 50, 50, 50, 50, 50),
    meta = "test_meta",
    study = paste0("study", 1:7)
  )

  # One-sided test: only count positive significant results
  # Critical value for alpha = 0.05 with df=50 is approximately 1.676
  # Positive significant: 2.0, 2.5 (2 studies)
  # Negative values (-1.0, -2.0, -2.5) are NOT counted
  observed <- calculate_observed_significant(df, alpha = 0.05)
  expect_equal(observed, 2)
})

test_that("calculate_observed_significant handles boundary cases (positive only)", {
  df <- data.frame(
    t_value = c(1.96, -1.96, 1.65, 1.961),
    dof = c(50, 50, 50, 50),
    meta = "test_meta",
    study = paste0("study", 1:4)
  )

  # One-sided test with t-distribution: only count positive significant results
  # Critical value for alpha=0.05 with df=50 is approximately 1.676
  # Positive significant: 1.96, 1.961 (2 studies)
  # Negative value (-1.96) and value below critical (1.65 < 1.676) are NOT counted
  observed <- calculate_observed_significant(df, alpha = 0.05)
  expect_equal(observed, 2) # Only 1.96 and 1.961 are positive and significant
})

test_that("calculate_observed_significant handles NA values", {
  df <- data.frame(
    t_value = c(0.5, NA_real_, 2.5, -2.0, NA_real_),
    dof = c(50, 50, 50, 50, 50),
    meta = "test_meta",
    study = paste0("study", 1:5)
  )

  observed <- calculate_observed_significant(df, alpha = 0.05)
  # Should count only positive non-NA significant values: 2.5 (1 study)
  # -2.0 is negative, so not counted
  expect_equal(observed, 1)
})

test_that("calculate_observed_significant handles custom alpha", {
  df <- data.frame(
    t_value = c(1.5, 2.0, 2.5, 2.576, 3.0),
    dof = c(50, 50, 50, 50, 50),
    meta = "test_meta",
    study = paste0("study", 1:5)
  )

  # For alpha = 0.05, critical value with df=50 <U+2248> 1.676, so 2.0, 2.5, 2.576, 3.0 are significant (4)
  observed_05 <- calculate_observed_significant(df, alpha = 0.05)
  expect_equal(observed_05, 4)

  # For alpha = 0.01, critical value with df=50 <U+2248> 2.403, so only 2.5, 2.576, 3.0 are significant (3)
  observed_01 <- calculate_observed_significant(df, alpha = 0.01)
  expect_equal(observed_01, 3)
})

test_that("calculate_expected_significant sums E_sigi correctly", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    dof = c(50, 50, 50),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  expected <- calculate_expected_significant(df, uwls_estimate, tau2)

  # Should be sum of E_sigi values
  esigi <- calculate_esigi(df, uwls_estimate, tau2)
  expected_manual <- sum(esigi, na.rm = TRUE)

  expect_equal(expected, expected_manual, tolerance = 1e-6)
  expect_true(expected >= 0)
  expect_true(expected <= nrow(df)) # Cannot exceed number of studies
})

test_that("calculate_expected_significant handles zero uwls_estimate", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    dof = c(50, 50, 50),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  uwls_estimate <- 0.0
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  expected <- calculate_expected_significant(df, uwls_estimate, tau2)

  # When uwls_estimate = 0, expected should be low (one-sided test)
  expect_true(expected >= 0 && expected <= nrow(df))
})

test_that("calculate_expected_significant handles large uwls_estimate", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Small SE
    dof = c(50, 50, 50),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  expected <- calculate_expected_significant(df, uwls_estimate, tau2)

  # With very small SE, E_sigi should be high, so expected should be close to 3
  expect_true(expected > 2.5)
})

test_that("calculate_expected_significant handles custom se vector", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    dof = c(50, 50, 50),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  custom_se <- c(0.05, 0.1, 0.15)
  expected_custom <- calculate_expected_significant(df, uwls_estimate, tau2, se = custom_se)
  expected_default <- calculate_expected_significant(df, uwls_estimate, tau2)

  expect_false(expected_custom == expected_default)
  expect_true(expected_custom >= 0 && expected_custom <= nrow(df))
})

test_that("calculate_expected_significant handles NA values", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, NA_real_, 0.1),
    dof = c(50, 50, 50),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  uwls_estimate <- 0.2
  tau2 <- get_re1_tau2(df)
  if (is.na(tau2)) tau2 <- 0.0 # Fallback for test
  expected <- calculate_expected_significant(df, uwls_estimate, tau2)

  # Should sum only non-NA E_sigi values
  esigi <- calculate_esigi(df, uwls_estimate, tau2)
  expected_manual <- sum(esigi, na.rm = TRUE)

  expect_equal(expected, expected_manual, tolerance = 1e-6)
})
