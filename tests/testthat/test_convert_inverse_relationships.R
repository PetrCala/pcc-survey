# Tests for convert_inverse_relationships function

test_that("convert_inverse_relationships flips meta with negative mean", {
  # Create a meta-analysis with negative mean PCC
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

test_that("convert_inverse_relationships does not flip meta with positive mean", {
  # Create a meta-analysis with positive mean PCC
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

test_that("convert_inverse_relationships does not flip meta with zero mean", {
  # Create a meta-analysis with zero mean PCC
  df <- data.frame(
    meta = rep("meta_zero", 3),
    effect = c(-0.1, 0.0, 0.1),
    t_value = c(-1.0, 0.0, 1.0),
    se = c(0.1, 0.1, 0.1),
    study = c("study1", "study2", "study3")
  )

  result <- convert_inverse_relationships(df, log_results = FALSE)

  # Effects should remain unchanged (mean is 0, not < 0)
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
      -0.3, -0.2, -0.1, # Meta A: mean = -0.2 (should flip)
      0.1, 0.2, 0.3, # Meta B: mean = 0.2 (should not flip)
      -0.15, -0.1, -0.05 # Meta C: mean = -0.1 (should flip)
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

  # Mean of non-NA effects: (-0.3 + -0.2 + -0.1) / 3 = -0.2 (should flip)
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

  # Should return unchanged (mean is NA, so no conversion)
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
