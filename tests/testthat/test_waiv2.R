# Smoke test for waiv2(). WAIV2 is no longer in the survey outputs (it was
# exploratory) but the estimator is retained for a possible future paper.

test_that("waiv2 runs and returns a finite estimate", {
  skip_if_not_installed("AER")

  df <- data.frame(
    effect = c(0.10, 0.20, 0.15, 0.25, 0.30),
    se = c(0.05, 0.06, 0.04, 0.07, 0.08),
    sample_size = c(100, 150, 200, 120, 90)
  )

  res <- waiv2(df)
  expect_true(is.finite(res$est))
  expect_true(is.finite(res$t_value))
})
