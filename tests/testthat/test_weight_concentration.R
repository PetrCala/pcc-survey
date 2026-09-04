# Tests for the weight-concentration diagnostic (R2 point 3) and the per-estimate
# weights returned by the estimators.

test_that("top_study_weight_share sums weights within a primary study", {
  # Study A has two estimates (0.3 + 0.2 = 0.5), B one (0.4), C one (0.1)
  w <- c(0.3, 0.4, 0.2, 0.1)
  study <- c("A", "B", "A", "C")

  expect_equal(top_study_weight_share(w, study, top_n = 1L), 0.5)
  expect_equal(top_study_weight_share(w, study, top_n = 2L), 0.9)
  expect_equal(top_study_weight_share(w, study, top_n = 3L), 1.0)
  # More studies requested than present -> the whole weight
  expect_equal(top_study_weight_share(w, study, top_n = 10L), 1.0)
})

test_that("top_study_weight_share is scale invariant and normalises internally", {
  w <- c(3, 4, 2, 1)
  study <- c("A", "B", "A", "C")
  expect_equal(top_study_weight_share(w, study, 1L), 0.5)
  expect_equal(top_study_weight_share(w * 1000, study, 1L), 0.5)
})

test_that("top_study_weight_share handles a single study and NA weights", {
  expect_equal(top_study_weight_share(c(1, 2, 3), c("A", "A", "A"), 1L), 1.0)
  expect_true(is.na(top_study_weight_share(c(NA_real_, NA_real_), c("A", "B"), 1L)))
  # NA weights are dropped, the rest renormalised
  expect_equal(top_study_weight_share(c(NA, 1, 3), c("A", "B", "C"), 1L), 0.75)
})

test_that("top_study_weight_share rejects negative weights and length mismatches", {
  expect_error(top_study_weight_share(c(-1, 2), c("A", "B"), 1L))
  expect_error(top_study_weight_share(c(1, 2, 3), c("A", "B"), 1L))
})

test_that("top_weight_share at the estimate level ignores study grouping", {
  w <- c(0.3, 0.4, 0.2, 0.1)
  study <- c("A", "B", "A", "C")
  expect_equal(top_weight_share(w, study = NULL, top_n = 1L), 0.4)
  expect_equal(top_weight_share(w, study = NULL, top_n = 3L), 0.9)
  # Same weights, paper level: A = 0.5
  expect_equal(top_weight_share(w, study = study, top_n = 1L), 0.5)
  expect_equal(top_weight_share(c(NA, 1, 3), study = NULL, top_n = 1L), 0.75)
})

test_that("weight_concentration returns estimate- and paper-level top1/top3 shares", {
  w <- c(0.5, 0.2, 0.15, 0.1, 0.05)
  study <- c("A", "B", "A", "C", "D")
  wc <- weight_concentration(w, study)
  expect_named(wc, c("top1_est", "top3_est", "top1_study", "top3_study"))
  expect_equal(wc$top1_est, 0.5)
  expect_equal(wc$top3_est, 0.85)
  expect_equal(wc$top1_study, 0.65)
  expect_equal(wc$top3_study, 0.95)
})

test_that("estimators return normalised per-estimate weights with the expected structure", {
  df <- data.frame(
    meta = "m",
    study = c("s1", "s1", "s2", "s3"),
    effect = c(0.10, 0.15, 0.30, 0.05),
    se = c(0.05, 0.10, 0.02, 0.20),
    sample_size = c(400, 100, 2500, 25),
    t_value = c(2, 1.5, 15, 0.25),
    dof = c(395, 95, 2495, 20)
  )
  df <- compute_derived_quantities(df)

  # Simple mean: equal weights
  expect_equal(simple_mean(df)$weights, rep(0.25, 4))

  # UWLS: inverse-variance weights on the supplied SE
  w_uwls1 <- uwls(df, se = df$se_s1)$weights
  expect_equal(w_uwls1, (1 / df$se_s1^2) / sum(1 / df$se_s1^2))
  expect_equal(sum(w_uwls1), 1)
  # UWLS+3 uses the reported se column
  expect_equal(uwls3(df)$weights, (1 / df$se^2) / sum(1 / df$se^2))

  # HS: sample-size weights
  expect_equal(hsma(df)$weights, df$sample_size / sum(df$sample_size))

  # RE: 1 / (se^2 + tau2), normalised; matches metafor's own weights
  re_res <- re(df, se = df$se_s1, method = "ML")
  expect_equal(re_res$weights, (1 / (df$se_s1^2 + re_res$tau2)) / sum(1 / (df$se_s1^2 + re_res$tau2)))
  fit <- metafor::rma(yi = df$effect, sei = df$se_s1, method = "ML")
  expect_equal(re_res$weights, as.numeric(stats::weights(fit)) / 100, tolerance = 1e-8)

  # Fisher's z variants carry the weights of the underlying z fit
  expect_equal(sum(fishers_z(df)$weights), 1)
  w_uwlsz <- uwls_fishers_z(df)$weights
  expect_equal(w_uwlsz, (df$sample_size - 3) / sum(df$sample_size - 3))
})

test_that("get_pcc_survey_metaflavours reports estimate- and paper-level top-1/top-3 weight shares", {
  df <- data.frame(
    meta = "m",
    study = c("s1", "s1", "s2", "s3"),
    effect = c(0.10, 0.15, 0.30, 0.05),
    se = c(0.05, 0.10, 0.02, 0.20),
    sample_size = c(400, 100, 2500, 25),
    t_value = c(2, 1.5, 15, 0.25),
    dof = c(395, 95, 2495, 20)
  )
  df <- compute_derived_quantities(df)

  res <- get_pcc_survey_metaflavours(df)

  # Simple mean, estimate level: 1/k and 3/k; paper level: s1 carries 2 of 4
  expect_equal(res$simple_mean_w_top1_estimate, 0.25)
  expect_equal(res$simple_mean_w_top3_estimate, 0.75)
  expect_equal(res$simple_mean_w_top1_study, 0.5)
  expect_equal(res$simple_mean_w_top3_study, 1.0)

  # HS: the n = 2500 estimate (s2) dominates at both levels
  expect_equal(res$hsma_w_top1_estimate, 2500 / 3025)
  expect_equal(res$hsma_w_top3_estimate, 3000 / 3025)
  expect_equal(res$hsma_w_top1_study, 2500 / 3025)
  expect_equal(res$hsma_w_top3_study, 1.0)

  # All share columns present and within [0, 1]; none for PET-PEESE
  share_cols <- grep("_w_top[13]_(estimate|study)$", colnames(res), value = TRUE)
  expect_length(share_cols, 36)
  expect_true(all(unlist(res[share_cols]) >= 0 & unlist(res[share_cols]) <= 1))
  expect_false(any(grepl("^petpeese_w_top", colnames(res))))
})
