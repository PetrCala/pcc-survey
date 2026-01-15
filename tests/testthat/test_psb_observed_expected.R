# Tests for calculate_observed_significant and calculate_expected_significant

test_that("calculate_observed_significant counts correctly", {
  df <- data.frame(
    t_value = c(0.5, 1.5, 2.0, 2.5, -1.0, -2.0, -2.5),
    meta = "test_meta",
    study = paste0("study", 1:7)
  )

  # Critical value for alpha = 0.05 is 1.96
  # Significant: 2.0, 2.5, -2.0, -2.5 (4 studies)
  observed <- calculate_observed_significant(df, alpha = 0.05)
  expect_equal(observed, 4)
})

test_that("calculate_observed_significant handles boundary cases", {
  df <- data.frame(
    t_value = c(1.96, -1.96, 1.959, 1.961),
    meta = "test_meta",
    study = paste0("study", 1:4)
  )

  # Critical value for alpha=0.05 is approximately 1.959964 (not exactly 1.96)
  # So |1.96| = 1.96 > 1.959964 = TRUE (significant)
  # |-1.96| = 1.96 > 1.959964 = TRUE (significant)
  # |1.959| = 1.959 < 1.959964 = FALSE (not significant)
  # |1.961| = 1.961 > 1.959964 = TRUE (significant)
  # Therefore, 3 studies are significant
  observed <- calculate_observed_significant(df, alpha = 0.05)
  expect_equal(observed, 3) # 1.96, -1.96, and 1.961 are significant
})

test_that("calculate_observed_significant handles NA values", {
  df <- data.frame(
    t_value = c(0.5, NA_real_, 2.5, -2.0, NA_real_),
    meta = "test_meta",
    study = paste0("study", 1:5)
  )

  observed <- calculate_observed_significant(df, alpha = 0.05)
  # Should count only non-NA significant values: 2.5, -2.0 (2 studies)
  expect_equal(observed, 2)
})

test_that("calculate_observed_significant handles custom alpha", {
  df <- data.frame(
    t_value = c(1.5, 2.0, 2.5, 2.576, 3.0),
    meta = "test_meta",
    study = paste0("study", 1:5)
  )

  # For alpha = 0.05, critical value = 1.96, so 2.0, 2.5, 2.576, 3.0 are significant (4)
  observed_05 <- calculate_observed_significant(df, alpha = 0.05)
  expect_equal(observed_05, 4)

  # For alpha = 0.01, critical value = 2.576, so only 2.576, 3.0 are significant (2)
  observed_01 <- calculate_observed_significant(df, alpha = 0.01)
  expect_equal(observed_01, 2)
})

test_that("calculate_expected_significant sums power correctly", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  expected <- calculate_expected_significant(df, mean_effect)

  # Should be sum of power values
  power <- calculate_study_power(df, mean_effect)
  expected_manual <- sum(power, na.rm = TRUE)

  expect_equal(expected, expected_manual, tolerance = 1e-6)
  expect_true(expected >= 0)
  expect_true(expected <= nrow(df)) # Cannot exceed number of studies
})

test_that("calculate_expected_significant handles zero mean effect", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.0
  expected <- calculate_expected_significant(df, mean_effect)

  # When mean_effect = 0, expected should be approximately n * alpha
  # For 3 studies and alpha = 0.05, expected <U+2248> 0.15
  expect_true(expected >= 0.1 && expected <= 0.2)
})

test_that("calculate_expected_significant handles large mean effect", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.01, 0.01), # Small SE
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  expected <- calculate_expected_significant(df, mean_effect)

  # With very small SE, power should be very high, so expected should be close to 3
  expect_true(expected > 2.9)
})

test_that("calculate_expected_significant handles custom se vector", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  custom_se <- c(0.05, 0.1, 0.15)
  expected_custom <- calculate_expected_significant(df, mean_effect, se = custom_se)
  expected_default <- calculate_expected_significant(df, mean_effect)

  expect_false(expected_custom == expected_default)
  expect_true(expected_custom >= 0 && expected_custom <= nrow(df))
})

test_that("calculate_expected_significant handles NA values", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, NA_real_, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  expected <- calculate_expected_significant(df, mean_effect)

  # Should sum only non-NA power values
  power <- calculate_study_power(df, mean_effect)
  expected_manual <- sum(power, na.rm = TRUE)

  expect_equal(expected, expected_manual, tolerance = 1e-6)
})
