# Tests for compute_derived_quantities function

make_valid_df <- function() {
  data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07),
    t_value = c(2.0, 3.5, 4.5),
    dof = c(50, 100, 150),
    sample_size = c(57, 107, 157),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )
}

test_that("compute_derived_quantities adds all 6 derived columns", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  expect_true("pcc_var" %in% colnames(result))
  expect_true("se_s1" %in% colnames(result))
  expect_true("se_s2" %in% colnames(result))
  expect_true("fishers_z" %in% colnames(result))
  expect_true("fishers_z_se" %in% colnames(result))
  expect_true("pcc3" %in% colnames(result))
})

test_that("compute_derived_quantities computes pcc_var correctly", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  expected <- (1 - df$effect^2)^2 / df$dof
  expect_equal(result$pcc_var, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities computes se_s1 correctly", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  r_p <- df$t_value / sqrt(df$t_value^2 + df$dof)
  expected <- sqrt((1 - r_p^2) / df$dof)
  expect_equal(result$se_s1, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities computes se_s2 correctly", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  r_p <- df$t_value / sqrt(df$t_value^2 + df$dof)
  expected <- (1 - r_p^2) / sqrt(df$dof)
  expect_equal(result$se_s2, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities computes fishers_z correctly", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  expected <- 0.5 * log((1 + df$effect) / (1 - df$effect))
  expect_equal(result$fishers_z, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities computes fishers_z_se correctly", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  expected <- 1 / sqrt(df$sample_size - 3)
  expect_equal(result$fishers_z_se, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities computes pcc3 correctly", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  expected <- df$t_value / sqrt(df$t_value^2 + df$dof + 3)
  expect_equal(result$pcc3, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities all outputs are finite", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  derived_cols <- c("pcc_var", "se_s1", "se_s2", "fishers_z", "fishers_z_se", "pcc3")
  for (col in derived_cols) {
    expect_true(all(is.finite(result[[col]])),
                info = paste("Column", col, "should be all finite"))
  }
})

test_that("compute_derived_quantities preserves original columns", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  expect_equal(result$effect, df$effect)
  expect_equal(result$se, df$se)
  expect_equal(result$t_value, df$t_value)
  expect_equal(result$dof, df$dof)
  expect_equal(result$sample_size, df$sample_size)
  expect_equal(result$meta, df$meta)
  expect_equal(result$study, df$study)
})

test_that("compute_derived_quantities errors on missing columns", {
  df <- data.frame(effect = c(0.1, 0.2))
  expect_error(compute_derived_quantities(df))
})

test_that("compute_derived_quantities se_s1 matches pcc_se_s1 utility", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  # pcc_se_s1 is the standalone utility in pcc.R
  expected <- pcc_se_s1(df)
  expect_equal(result$se_s1, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities se_s2 matches pcc_se_s2 utility", {
  df <- make_valid_df()
  result <- compute_derived_quantities(df)

  # pcc_se_s2 is the standalone utility in pcc.R
  expected <- pcc_se_s2(df)
  expect_equal(result$se_s2, expected, tolerance = 1e-10)
})

test_that("compute_derived_quantities handles negative effects correctly", {
  df <- make_valid_df()
  df$effect <- c(-0.1, -0.2, -0.3)
  df$t_value <- c(-2.0, -3.5, -4.5)
  result <- compute_derived_quantities(df)

  # fishers_z should be negative for negative effects
  expect_true(all(result$fishers_z < 0))

  # pcc3 should be negative for negative t_values
  expect_true(all(result$pcc3 < 0))

  # se_s1, se_s2, pcc_var should still be positive
  expect_true(all(result$se_s1 > 0))
  expect_true(all(result$se_s2 > 0))
  expect_true(all(result$pcc_var > 0))
})
