# Tests for convert_inverse_relationships function

test_that("convert_inverse_relationships flips meta with negative median", {
  # Create a meta-analysis with negative median PCC
  df <- data.frame(
    meta = rep("meta_negative", 3),
    effect = c(-0.3, -0.2, -0.1),
    t_value = c(-3.0, -2.0, -1.0),
    se = c(0.1, 0.1, 0.1),
    study = c("study1", "study2", "study3")
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # All effects should be flipped to positive
  expect_equal(result$effect, c(0.3, 0.2, 0.1))

  # t-values should also be flipped
  expect_equal(result$t_value, c(3.0, 2.0, 1.0))

  # Standard errors should remain unchanged
  expect_equal(result$se, c(0.1, 0.1, 0.1))
})

test_that("convert_inverse_relationships does not flip meta with positive median", {
  # Create a meta-analysis with positive median PCC
  df <- data.frame(
    meta = rep("meta_positive", 3),
    effect = c(0.1, 0.2, 0.3),
    t_value = c(1.0, 2.0, 3.0),
    se = c(0.1, 0.1, 0.1),
    study = c("study1", "study2", "study3")
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # All effects should remain unchanged
  expect_equal(result$effect, c(0.1, 0.2, 0.3))

  # t-values should remain unchanged
  expect_equal(result$t_value, c(1.0, 2.0, 3.0))

  # Standard errors should remain unchanged
  expect_equal(result$se, c(0.1, 0.1, 0.1))
})

test_that("convert_inverse_relationships does not flip meta with zero median", {
  # Create a meta-analysis with zero median PCC
  df <- data.frame(
    meta = rep("meta_zero", 3),
    effect = c(-0.1, 0.0, 0.1),
    t_value = c(-1.0, 0.0, 1.0),
    se = c(0.1, 0.1, 0.1),
    study = c("study1", "study2", "study3")
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # Effects should remain unchanged (median is 0, not < 0)
  expect_equal(result$effect, c(-0.1, 0.0, 0.1))

  # t-values should remain unchanged
  expect_equal(result$t_value, c(-1.0, 0.0, 1.0))
})

test_that("convert_inverse_relationships handles multiple metas correctly", {
  # Create data with 3 meta-analyses
  df <- data.frame(
    meta = c(
      rep("meta_A", 3),
      rep("meta_B", 3),
      rep("meta_C", 3)
    ),
    effect = c(
      -0.3, -0.2, -0.1, # Meta A: median = -0.2 (should flip)
      0.1, 0.2, 0.3, # Meta B: median = 0.2 (should not flip)
      -0.15, -0.1, -0.05 # Meta C: median = -0.1 (should flip)
    ),
    t_value = c(
      -3.0, -2.0, -1.0, # Meta A
      1.0, 2.0, 3.0, # Meta B
      -1.5, -1.0, -0.5 # Meta C
    ),
    se = rep(0.1, 9),
    study = paste0("study", 1:9)
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # Meta A should be flipped
  expect_equal(result$effect[result$meta == "meta_A"], c(0.3, 0.2, 0.1))
  expect_equal(result$t_value[result$meta == "meta_A"], c(3.0, 2.0, 1.0))

  # Meta B should not be flipped
  expect_equal(result$effect[result$meta == "meta_B"], c(0.1, 0.2, 0.3))
  expect_equal(result$t_value[result$meta == "meta_B"], c(1.0, 2.0, 3.0))

  # Meta C should be flipped
  expect_equal(result$effect[result$meta == "meta_C"], c(0.15, 0.1, 0.05))
  expect_equal(result$t_value[result$meta == "meta_C"], c(1.5, 1.0, 0.5))
})

test_that("convert_inverse_relationships handles missing values correctly", {
  # Create meta with some NA effects
  df <- data.frame(
    meta = rep("meta_with_na", 5),
    effect = c(-0.3, NA_real_, -0.2, -0.1, NA_real_),
    t_value = c(-3.0, NA_real_, -2.0, -1.0, NA_real_),
    se = c(0.1, 0.1, 0.1, 0.1, 0.1),
    study = paste0("study", 1:5)
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # Median of non-NA effects: median(-0.3, -0.2, -0.1) = -0.2 (should flip)
  # Non-NA effects should be flipped
  expect_equal(result$effect[!is.na(result$effect)], c(0.3, 0.2, 0.1))
  expect_equal(result$t_value[!is.na(result$t_value)], c(3.0, 2.0, 1.0))

  # NA values should remain NA
  expect_true(all(is.na(result$effect[is.na(df$effect)])))
  expect_true(all(is.na(result$t_value[is.na(df$t_value)])))
})

test_that("convert_inverse_relationships handles all NA effects gracefully", {
  # Create meta with all NA effects
  df <- data.frame(
    meta = rep("meta_all_na", 3),
    effect = c(NA_real_, NA_real_, NA_real_),
    t_value = c(NA_real_, NA_real_, NA_real_),
    se = c(0.1, 0.1, 0.1),
    study = c("study1", "study2", "study3")
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # Should return unchanged (median is NA, so no conversion)
  expect_true(all(is.na(result$effect)))
  expect_true(all(is.na(result$t_value)))
})

test_that("convert_inverse_relationships works without t_value column", {
  # Create meta without t_value column
  df <- data.frame(
    meta = rep("meta_no_t", 3),
    effect = c(-0.3, -0.2, -0.1),
    se = c(0.1, 0.1, 0.1),
    study = c("study1", "study2", "study3")
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # Effects should be flipped
  expect_equal(result$effect, c(0.3, 0.2, 0.1))

  # Should not error even without t_value column
  expect_false("t_value" %in% colnames(result))
})

test_that("convert_inverse_relationships maintains t = effect/se relationship", {
  # Create meta with known relationship
  df <- data.frame(
    meta = rep("meta_test", 3),
    effect = c(-0.3, -0.2, -0.1),
    t_value = c(-3.0, -2.0, -1.0),
    se = c(0.1, 0.1, 0.1),
    study = c("study1", "study2", "study3")
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # After flipping, t should still equal effect/se
  expect_equal(result$t_value, result$effect / result$se)
})

test_that("convert_inverse_relationships uses median when mean and median differ", {
  # Create a meta-analysis where mean and median differ
  # Effects: [-0.5, -0.4, -0.3, 0.5]
  # Mean = (-0.5 + -0.4 + -0.3 + 0.5) / 4 = -0.175 (would flip with mean)
  # Median = median(-0.5, -0.4, -0.3, 0.5) = -0.35 (flips with median)
  # Both would flip, so let's use a case where they differ in decision:

  # Effects: [-0.3, -0.2, 0.1, 0.2, 0.3]
  # Mean = (-0.3 + -0.2 + 0.1 + 0.2 + 0.3) / 5 = 0.02 (would NOT flip with mean)
  # Median = median(-0.3, -0.2, 0.1, 0.2, 0.3) = 0.1 (would NOT flip with median)
  # Actually both don't flip... let me use a better example:

  # Effects: [-0.4, -0.3, -0.2, 0.1, 0.2]
  # Mean = (-0.4 + -0.3 + -0.2 + 0.1 + 0.2) / 5 = -0.12 (would flip with mean)
  # Median = median(-0.4, -0.3, -0.2, 0.1, 0.2) = -0.2 (flips with median)
  # Both flip... need a case where mean < 0 but median >= 0:

  # Effects: [-0.5, -0.4, 0.1, 0.2, 0.3]
  # Mean = (-0.5 + -0.4 + 0.1 + 0.2 + 0.3) / 5 = -0.06 (would flip with mean)
  # Median = median(-0.5, -0.4, 0.1, 0.2, 0.3) = 0.1 (would NOT flip with median)

  df <- data.frame(
    meta = rep("meta_mean_median_diff", 5),
    effect = c(-0.5, -0.4, 0.1, 0.2, 0.3),
    t_value = c(-5.0, -4.0, 1.0, 2.0, 3.0),
    se = c(0.1, 0.1, 0.1, 0.1, 0.1),
    study = paste0("study", 1:5)
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # With median-based logic: median = 0.1 (positive), so should NOT flip
  # Effects should remain unchanged
  expect_equal(result$effect, c(-0.5, -0.4, 0.1, 0.2, 0.3))
  expect_equal(result$t_value, c(-5.0, -4.0, 1.0, 2.0, 3.0))

  # Verify: mean would have flipped, but median doesn't
  mean_effect <- mean(df$effect)
  median_effect <- median(df$effect)
  expect_true(mean_effect < 0) # Mean is negative
  expect_true(median_effect >= 0) # Median is non-negative
})
