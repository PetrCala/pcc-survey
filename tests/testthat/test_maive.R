# Tests for calculate_maive function

test_that("calculate_maive returns list with est and t_value", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150)
  )

  result <- calculate_maive(df)

  # Should return list with est and t_value
  expect_type(result, "list")
  expect_named(result, c("est", "t_value"))
  expect_true(is.numeric(result$est))
  expect_true(is.numeric(result$t_value))
})

test_that("calculate_maive errors when required columns are missing", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    # Missing se and sample_size columns
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  expect_error(calculate_maive(df))
})

test_that("calculate_maive errors on empty data frame", {
  df <- data.frame(
    effect = numeric(0),
    se = numeric(0),
    meta = character(0),
    study = character(0),
    sample_size = numeric(0)
  )

  expect_error(calculate_maive(df))
})

test_that("calculate_maive accepts custom method parameter", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150)
  )

  # Test different methods
  result1 <- calculate_maive(df, method = 1)
  result2 <- calculate_maive(df, method = 3)

  expect_type(result1, "list")
  expect_type(result2, "list")
  expect_named(result1, c("est", "t_value"))
  expect_named(result2, c("est", "t_value"))
})
