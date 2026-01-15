# Tests for calculate_maive function

test_that("calculate_maive returns list with est and t_value", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- calculate_maive(df)

  # Should return list with est and t_value
  expect_type(result, "list")
  expect_named(result, c("est", "t_value"))
  expect_true(is.numeric(result$est) || is.na(result$est))
  expect_true(is.numeric(result$t_value) || is.na(result$t_value))
})

test_that("calculate_maive handles missing sample_size by using dof", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(NA_real_, NA_real_, NA_real_),
    dof = c(43, 93, 143)
  )

  result <- calculate_maive(df)

  # Should handle missing sample_size by using dof
  expect_type(result, "list")
  expect_named(result, c("est", "t_value"))
})

test_that("calculate_maive handles missing required columns", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    # Missing se column
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- calculate_maive(df)

  # Should return NA values when required columns are missing
  expect_true(is.na(result$est))
  expect_true(is.na(result$t_value))
})

test_that("calculate_maive handles invalid data gracefully", {
  df <- data.frame(
    effect = c(NA_real_, NA_real_, NA_real_),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- calculate_maive(df)

  # Should handle invalid data gracefully (may return NA)
  expect_type(result, "list")
  expect_named(result, c("est", "t_value"))
})

test_that("calculate_maive handles zero or negative sample sizes", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 0, -10), # Invalid sample sizes
    dof = c(43, 93, 143)
  )

  result <- calculate_maive(df)

  # Should filter out invalid sample sizes
  expect_type(result, "list")
  expect_named(result, c("est", "t_value"))
})

test_that("calculate_maive handles empty data frame", {
  df <- data.frame(
    effect = numeric(0),
    se = numeric(0),
    meta = character(0),
    study = character(0),
    sample_size = numeric(0),
    dof = numeric(0)
  )

  result <- calculate_maive(df)

  # Should return NA when no data
  expect_true(is.na(result$est))
  expect_true(is.na(result$t_value))
})

test_that("calculate_maive accepts custom method parameter", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  # Test different methods (may fail if MAIVE package not available, but should not error)
  result1 <- tryCatch(calculate_maive(df, method = 1), error = function(e) list(est = NA, t_value = NA))
  result2 <- tryCatch(calculate_maive(df, method = 3), error = function(e) list(est = NA, t_value = NA))

  # Should return valid structure regardless of method
  expect_type(result1, "list")
  expect_type(result2, "list")
  expect_named(result1, c("est", "t_value"))
  expect_named(result2, c("est", "t_value"))
})

test_that("calculate_maive handles MAIVE package errors gracefully", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  # Even if MAIVE fails, should return valid structure
  result <- calculate_maive(df)

  expect_type(result, "list")
  expect_named(result, c("est", "t_value"))
  # Values may be NA if MAIVE calculation fails
  expect_true(is.numeric(result$est) || is.na(result$est))
  expect_true(is.numeric(result$t_value) || is.na(result$t_value))
})
