# Tests for the supplement's estimator helpers: meta_estimates() and the
# column-wise estimators behind Tables S3 and S9.

tight <- list(threshold = 1e-12, maxiter = 10000)

test_that("re_matrix matches metafor's ML fit column by column", {
  set.seed(11)
  k <- 30
  v <- matrix(runif(k * 3, 0.001, 0.05), k, 3)
  y <- matrix(rnorm(k * 3, 0.2, 0.1), k, 3) + matrix(rnorm(k * 3, 0, sqrt(v)), k, 3)
  fit <- re_matrix(y, v)
  for (j in 1:3) {
    mf <- metafor::rma(yi = y[, j], vi = v[, j], method = "ML", control = tight)
    expect_equal(fit$est[j], as.numeric(mf$beta[1]), tolerance = 1e-7)
    expect_equal(fit$tau2[j], as.numeric(mf$tau2), tolerance = 1e-7)
  }
})

test_that("re_matrix converges when the sampling variances differ by orders of magnitude", {
  set.seed(5)
  v <- c(runif(20, 1e-7, 1e-6), runif(20, 0.01, 0.1))
  y <- 0.1 + rnorm(40, 0, sqrt(v + 3e-5))
  fit <- re_matrix(y, v)
  mf <- metafor::rma(yi = y, vi = v, method = "ML", control = tight)
  expect_equal(fit$tau2, as.numeric(mf$tau2), tolerance = 1e-6)
  expect_equal(fit$est, as.numeric(mf$beta[1]), tolerance = 1e-6)
})

test_that("re_matrix leaves out NA rows, handles k = 1 and applies weight multipliers", {
  y <- c(0.1, 0.3, 0.2, NA)
  v <- c(0.01, 0.02, 0.015, 0.01)
  expect_equal(re_matrix(y, v)$est, re_matrix(y[1:3], v[1:3])$est)
  one <- re_matrix(0.25, 0.01)
  expect_equal(one$est, 0.25)
  expect_equal(one$tau2, 0)
  # With tau2 = 0 (homogeneous data) the multiplier gives the weighted mean of
  # 1 / v * mult
  y0 <- c(0.2, 0.2, 0.2)
  expect_equal(re_matrix(y0, c(0.01, 0.02, 0.04), mult = c(1, 0.5, 0.25))$est, 0.2)
  fit <- re_matrix(y[1:3], v[1:3])
  w <- 1 / (v[1:3] + fit$tau2) * c(1, 1, 0.5)
  expect_equal(re_matrix(y[1:3], v[1:3], mult = c(1, 1, 0.5))$est, sum(w * y[1:3]) / sum(w))
})

test_that("uwls_matrix and pet_peese_matrix match uwls() and fat_pet_peese()", {
  df <- make_supplement_data(n_meta = 3, seed = 2)
  for (meta_df in split(df, df$meta)) {
    expect_equal(uwls_matrix(meta_df$effect, meta_df$se_s1),
                 uwls(meta_df, se = meta_df$se_s1)$est, tolerance = 1e-12)
    expect_equal(pet_peese_matrix(meta_df$effect, meta_df$se_s1),
                 fat_pet_peese(meta_df, se = meta_df$se_s1)$est, tolerance = 1e-10)
  }
  # Both branches of the PET-PEESE rule
  se <- c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07)
  sig <- data.frame(effect = c(0.30, 0.31, 0.33, 0.34, 0.36, 0.38), se_s1 = se)
  null <- data.frame(effect = -0.2 + 2 * se + c(0.002, -0.003, 0.001, -0.002, 0.003, -0.001), se_s1 = se)
  expect_equal(fat_pet_peese(sig)$type, "PEESE")
  expect_equal(fat_pet_peese(null)$type, "PET")
  expect_equal(pet_peese_matrix(cbind(sig$effect, null$effect), se),
               c(fat_pet_peese(sig)$est, fat_pet_peese(null)$est), tolerance = 1e-10)
  expect_true(is.na(pet_peese_matrix(c(0.1, 0.2), c(0.05, 0.1))))
})

test_that("estimators_matrix reproduces the per-MA Table 1 estimates", {
  df <- make_supplement_data(n_meta = 3, seed = 3)
  # One estimate with df <= 1 is left out of REz / UWLSz in both
  df$fishers_z_se[3] <- NA
  for (meta_df in split(df, df$meta)) {
    ref <- meta_estimates(meta_df)
    eng <- estimators_matrix(
      meta_df$effect, meta_df$se_s1, meta_df$se_s2, meta_df$pcc3,
      meta_df$fishers_z, meta_df$fishers_z_se, meta_df$sample_size
    )
    expect_equal(colnames(eng), unname(supplement_estimators()))
    # RE differs only by metafor's convergence threshold
    expect_equal(unname(eng[1, ]), unname(ref), tolerance = 1e-4)
    no_re <- !names(ref) %in% c("re1", "re2", "fishers_z")
    expect_equal(unname(eng[1, no_re]), unname(ref[no_re]), tolerance = 1e-10)
  }
})

test_that("estimators_matrix with weights divided by m averages study means", {
  df <- make_supplement_data(n_meta = 1, seed = 4)
  m <- as.numeric(table(df$study)[df$study])
  eng <- estimators_matrix(df$effect, df$se_s1, df$se_s2, df$pcc3, df$fishers_z,
                           df$fishers_z_se, df$sample_size, mult = 1 / m)
  expect_equal(unname(eng[1, "Mean"]), mean(tapply(df$effect, df$study, mean)))
  w <- df$sample_size / m
  expect_equal(unname(eng[1, "HS"]), sum(w * df$effect) / sum(w))
  w <- 1 / (df$se_s1^2 * m)
  expect_equal(unname(eng[1, "UWLS1"]), sum(w * df$effect) / sum(w))
})

test_that("meta_estimates matches get_pcc_survey_metaflavours and its variations", {
  df <- make_supplement_data(n_meta = 1, seed = 6)
  flav <- get_pcc_survey_metaflavours(df)
  est <- meta_estimates(df)
  expect_named(est, names(supplement_estimators()))
  expect_equal(est[["re1"]], flav$re1_est)
  expect_equal(est[["uwlsz"]], flav$uwlsz_est)
  expect_equal(est[["petpeese"]], flav$petpeese)

  # HS with df weights
  expect_equal(meta_estimates(df, "hsma", hs_weights = "dof")[["hsma"]],
               sum(df$dof * df$effect) / sum(df$dof))
  # UWLSz under 1 / (n - 3)
  se_n3 <- 1 / sqrt(df$sample_size - 3)
  w <- 1 / se_n3^2
  expect_equal(meta_estimates(df, "uwlsz", fishers_z_variance = "n_minus_3")[["uwlsz"]],
               tanh(sum(w * df$fishers_z) / sum(w)))
  # Other tau2 estimators reach metafor
  dl <- metafor::rma(yi = df$effect, sei = df$se_s1, method = "DL")
  expect_equal(meta_estimates(df, "re1", re_method = "DL")[["re1"]], as.numeric(dl$beta[1]))
  # A failing fit gives NA rather than an error
  expect_true(is.na(meta_estimates(df[1:2, ], "petpeese")[["petpeese"]]))
})

test_that("results_estimates labels the per-MA columns as in Table 1", {
  res <- data.frame(meta = c("a", "b", "All meta-analyses"), re1_est = 1:3, petpeese = 4:6,
                    hsma_est = 7:9)
  out <- results_estimates(res)
  expect_named(out, c("RE1", "HS", "PP"))
  expect_equal(out$RE1, 1:2)
})

test_that("meta_estimates retries a non-converging RE fit with a halved step", {
  df <- make_supplement_data(n_meta = 1, seed = 6)
  controls <- list()
  local_mocked_bindings(re = function(df, effect = NULL, se = NULL, method = "ML", control = list()) {
    controls[[length(controls) + 1]] <<- control
    if (length(control) == 0) stop("Fisher scoring algorithm did not converge.")
    list(est = 0.5, t_value = 1, tau2 = 0, weights = rep(1 / nrow(df), nrow(df)))
  })
  est <- meta_estimates(df, c("re1", "fishers_z"))
  expect_equal(est[["re1"]], 0.5)
  expect_equal(est[["fishers_z"]], tanh(0.5))
  expect_equal(controls[[1]], list())
  expect_equal(controls[[2]], list(stepadj = 0.5, maxiter = 1000))
})
