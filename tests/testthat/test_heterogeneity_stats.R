# Tests for heterogeneity statistics exposed by re() and uwls()

het_df <- function() {
  data.frame(
    effect = c(0.1, 0.2, 0.15, 0.25, 0.3),
    se = c(0.05, 0.04, 0.06, 0.05, 0.03),
    meta = "test_meta",
    study = paste0("s", 1:5)
  )
}

test_that("re() returns tau2, Q and I2", {
  result <- re(het_df(), method = "ML")

  expect_true(all(c("est", "t_value", "tau2", "Q", "I2") %in% names(result)))
  expect_true(is.numeric(result$tau2) && !is.na(result$tau2))
  expect_true(is.numeric(result$Q) && !is.na(result$Q))
  expect_true(is.numeric(result$I2) && !is.na(result$I2))
  expect_true(result$tau2 >= 0)
  expect_true(result$I2 >= 0 && result$I2 <= 100)
})

test_that("re() tau2/Q/I2 match a direct metafor fit", {
  df <- het_df()
  fit <- metafor::rma(yi = df$effect, sei = df$se, method = "ML")

  result <- re(df, method = "ML")
  expect_equal(result$tau2, as.numeric(fit$tau2), tolerance = 1e-8)
  expect_equal(result$Q, as.numeric(fit$QE), tolerance = 1e-8)
  expect_equal(result$I2, as.numeric(fit$I2), tolerance = 1e-8)
})

test_that("uwls() returns gamma and gamma-derived Q and I2", {
  df <- het_df()
  result <- uwls(df)

  expect_true(all(c("est", "t_value", "gamma", "Q", "I2") %in% names(result)))

  # gamma equals the squared residual SE of the WLS fit (t ~ precision - 1)
  t_ <- df$effect / df$se
  precision <- 1 / df$se
  expected_gamma <- summary(stats::lm(t_ ~ precision - 1))$sigma^2
  expect_equal(result$gamma, expected_gamma, tolerance = 1e-10)

  k <- nrow(df)
  expect_equal(result$Q, (k - 1) * result$gamma, tolerance = 1e-10)
  expect_equal(result$I2, max(0, 1 - 1 / result$gamma) * 100, tolerance = 1e-10)
})

test_that("uwls() I2 is floored at 0 for under-dispersed data", {
  # effect constant and se varying => t is exactly proportional to precision,
  # so the WLS fit is perfect, gamma = 0 and I2 floors to 0 (not negative).
  df <- data.frame(
    effect = c(0.2, 0.2, 0.2, 0.2),
    se = c(0.05, 0.04, 0.03, 0.02),
    meta = "test_meta",
    study = paste0("s", 1:4)
  )
  result <- uwls(df)
  expect_true(result$I2 >= 0)
})

test_that("uwls() heterogeneity is NA for a single study", {
  df <- data.frame(
    effect = 0.2,
    se = 0.05,
    meta = "test_meta",
    study = "s1"
  )
  result <- uwls(df)
  expect_true(is.na(result$gamma))
  expect_true(is.na(result$Q))
  expect_true(is.na(result$I2))
  expect_false(is.na(result$est))
})
