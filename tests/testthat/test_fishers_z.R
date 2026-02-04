# Tests for fishers_z function

test_that("fishers_z calculates correctly with pre-computed columns", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(50, 100, 150),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  # Pre-compute Fisher's Z columns (as done by compute_derived_quantities)
  df$fishers_z <- 0.5 * log((1 + df$effect) / (1 - df$effect))
  df$fishers_z_se <- 1 / sqrt(df$sample_size - 3)

  result <- fishers_z(df, method = "ML")

  # est and t_value should be numeric (not NA)
  expect_true(is.numeric(result$est))
  expect_false(is.na(result$est))
  expect_true(is.numeric(result$t_value))
  expect_false(is.na(result$t_value))

  # est should be between -1 and 1 (it's a correlation coefficient)
  expect_true(result$est >= -1 && result$est <= 1)
})

test_that("fishers_z errors when pre-computed columns are missing", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(50, 100, 150),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  # Missing fishers_z and fishers_z_se columns
  expect_error(fishers_z(df, method = "ML"))
})

test_that("fishers_z correctly converts estimates back to PCC", {
  # Test that the Fisher's z transformation correctly converts back to PCC
  test_correlations <- c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, -0.1, -0.3, -0.5, -0.7)

  for (true_r in test_correlations) {
    df <- data.frame(
      effect = true_r,
      sample_size = 1000,
      meta = "test_meta",
      study = "study1"
    )

    # Pre-compute Fisher's Z columns
    df$fishers_z <- 0.5 * log((1 + df$effect) / (1 - df$effect))
    df$fishers_z_se <- 1 / sqrt(df$sample_size - 3)

    result <- fishers_z(df, method = "ML")

    expect_true(is.numeric(result$est))
    expect_false(is.na(result$est))
    expect_true(result$est >= -1 && result$est <= 1)

    # For a single study with large sample size, the estimate should be very close
    expect_true(abs(result$est - true_r) < 1e-6,
                info = paste0("For true_r = ", true_r, ", got est = ", result$est))
  }
})

test_that("fishers_z forward and inverse transformations are mathematically correct", {
  test_values <- c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9, -0.1, -0.3, -0.5, -0.7, -0.9)

  for (r in test_values) {
    # Forward transformation
    z <- 0.5 * log((1 + r) / (1 - r))

    # Inverse transformation
    r_back <- (exp(2 * z) - 1) / (exp(2 * z) + 1)

    # The round-trip should recover the original value
    expect_true(abs(r - r_back) < 1e-14,
                info = paste0("For r = ", r, ", round-trip error = ", abs(r - r_back)))
  }
})

test_that("fishers_z conversion works correctly with multiple studies", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4),
    sample_size = c(50, 100, 150, 200),
    meta = "test_meta",
    study = c("study1", "study2", "study3", "study4")
  )

  # Pre-compute Fisher's Z columns
  df$fishers_z <- 0.5 * log((1 + df$effect) / (1 - df$effect))
  df$fishers_z_se <- 1 / sqrt(df$sample_size - 3)

  result <- fishers_z(df, method = "ML")

  # The result should be a valid correlation coefficient
  expect_true(is.numeric(result$est))
  expect_false(is.na(result$est))
  expect_true(result$est >= -1 && result$est <= 1)

  # The estimate should be between the min and max input correlations
  expect_true(result$est >= min(df$effect))
  expect_true(result$est <= max(df$effect))

  # With these inputs (0.1 to 0.4), the weighted average should be positive
  expect_true(result$est > 0)
})
