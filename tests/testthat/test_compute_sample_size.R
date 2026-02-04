# Tests for compute_sample_size function

test_that("compute_sample_size fills missing values using dof + 7", {
  df <- data.frame(
    sample_size = c(NA_real_, NA_real_, NA_real_),
    dof = c(43, 93, 143)
  )

  result <- compute_sample_size(df)

  expect_equal(result$sample_size, c(50, 100, 150))
})

test_that("compute_sample_size preserves existing sample_size values", {
  df <- data.frame(
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- compute_sample_size(df)

  expect_equal(result$sample_size, c(50, 100, 150))
})

test_that("compute_sample_size fills only missing values, preserving existing", {
  df <- data.frame(
    sample_size = c(50, NA_real_, 150, NA_real_),
    dof = c(43, 93, 143, 193)
  )

  result <- compute_sample_size(df)

  # Existing values should be preserved, missing should be dof + 7

  expect_equal(result$sample_size, c(50, 100, 150, 200))
})

test_that("compute_sample_size handles all non-missing values (no-op)", {
  df <- data.frame(
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- compute_sample_size(df)

  expect_equal(result$sample_size, c(50, 100, 150))
})

test_that("compute_sample_size errors when required columns are missing", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3)
  )

  expect_error(compute_sample_size(df))
})

test_that("compute_sample_size does not modify other columns", {
  df <- data.frame(
    sample_size = c(NA_real_, 100),
    dof = c(43, 93),
    effect = c(0.1, 0.2),
    meta = c("m1", "m2")
  )

  result <- compute_sample_size(df)

  expect_equal(result$effect, c(0.1, 0.2))
  expect_equal(result$meta, c("m1", "m2"))
  expect_equal(result$dof, c(43, 93))
})
