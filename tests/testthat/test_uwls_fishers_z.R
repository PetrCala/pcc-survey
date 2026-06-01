# Tests for uwls_fishers_z function (UWLS on Fisher's z, "UWLSz")

make_uwlsz_df <- function(effect, sample_size) {
  df <- data.frame(
    effect = effect,
    sample_size = sample_size,
    meta = "test_meta",
    study = paste0("study", seq_along(effect))
  )
  df$fishers_z <- 0.5 * log((1 + df$effect) / (1 - df$effect))
  df$fishers_z_se <- 1 / sqrt(df$sample_size - 3)
  df
}

test_that("uwls_fishers_z returns a valid PCC estimate", {
  df <- make_uwlsz_df(c(0.1, 0.2, 0.3, 0.4), c(50, 100, 150, 200))

  result <- uwls_fishers_z(df)

  expect_true(is.numeric(result$est))
  expect_false(is.na(result$est))
  expect_true(is.numeric(result$t_value))
  expect_true(result$est >= -1 && result$est <= 1)
  # weighted average of positive correlations should be positive
  expect_true(result$est > 0)
})

test_that("uwls_fishers_z errors when pre-computed columns are missing", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(50, 100, 150),
    meta = "test_meta",
    study = c("s1", "s2", "s3")
  )

  expect_error(uwls_fishers_z(df))
})

test_that("uwls_fishers_z recovers a single large-sample correlation", {
  for (true_r in c(0.1, 0.3, 0.5, -0.2, -0.4)) {
    df <- make_uwlsz_df(true_r, 100000)
    result <- uwls_fishers_z(df)
    expect_true(abs(result$est - true_r) < 1e-4,
                info = paste0("true_r = ", true_r, ", est = ", result$est))
  }
})

test_that("uwls_fishers_z matches manual UWLS-on-z then back-transform", {
  df <- make_uwlsz_df(c(0.1, 0.25, 0.35), c(60, 120, 180))

  # Manual: UWLS regression of t on precision (no intercept) on the Fisher's z scale
  t_ <- df$fishers_z / df$fishers_z_se
  precision <- 1 / df$fishers_z_se
  est_z <- coef(stats::lm(t_ ~ precision - 1))[[1]]
  expected <- (exp(2 * est_z) - 1) / (exp(2 * est_z) + 1)

  result <- uwls_fishers_z(df)
  expect_equal(result$est, expected, tolerance = 1e-10)
})
