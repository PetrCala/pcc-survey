# Tests for calculate_smallest_estimate_counts

test_that("calculate_smallest_estimate_counts tallies smallest signed and negatives", {
  df <- data.frame(
    meta = c("M1", "M2", "M3"),
    re1_est = c(0.10, 0.05, -0.02),
    uwls1_est = c(0.20, 0.30, 0.40),
    hsma_est = c(0.15, 0.25, 0.35)
  )

  res <- calculate_smallest_estimate_counts(df)

  # RE1 is the smallest signed estimate in all 3 metas, and negative once (M3)
  re1 <- res[res$estimator == "RE1", ]
  expect_equal(re1$times_smallest, 3L)
  expect_equal(re1$times_negative, 1L)

  expect_equal(res[res$estimator == "UWLS1", ]$times_smallest, 0L)
  expect_equal(res[res$estimator == "UWLS1", ]$times_negative, 0L)
  expect_equal(res[res$estimator == "HSMA", ]$times_smallest, 0L)

  expect_true(all(res$n_metas == 3L))
})

test_that("calculate_smallest_estimate_counts excludes 'All meta-analyses' row", {
  df <- data.frame(
    meta = c("M1", "M2", "All meta-analyses"),
    re1_est = c(0.1, 0.2, 0.0),
    uwls1_est = c(0.3, 0.4, 0.5)
  )

  res <- calculate_smallest_estimate_counts(df)

  expect_true(all(res$n_metas == 2L))
  # RE1 smallest in both individual rows; the All row (0.0) is excluded
  expect_equal(res[res$estimator == "RE1", ]$times_smallest, 2L)
})

test_that("calculate_smallest_estimate_counts counts ties for each tied estimator", {
  df <- data.frame(
    meta = "M1",
    re1_est = 0.1,
    uwls1_est = 0.1,
    hsma_est = 0.2
  )

  res <- calculate_smallest_estimate_counts(df)

  expect_equal(res[res$estimator == "RE1", ]$times_smallest, 1L)
  expect_equal(res[res$estimator == "UWLS1", ]$times_smallest, 1L)
  expect_equal(res[res$estimator == "HSMA", ]$times_smallest, 0L)
})

test_that("calculate_smallest_estimate_counts errors when no estimator columns", {
  df <- data.frame(meta = c("M1", "M2"), foo = c(1, 2))
  expect_error(calculate_smallest_estimate_counts(df))
})
