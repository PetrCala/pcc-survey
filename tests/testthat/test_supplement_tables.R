# Tests for the online-supplement tables (S2 to S8, S10, Section 6)

test_that("top_weight_studies sums weights by study and breaks ties by name", {
  w <- c(0.3, 0.4, 0.2, 0.1)
  study <- c("A", "B", "A", "C")
  expect_equal(top_weight_studies(w, study, 1L), "A")
  expect_equal(top_weight_studies(w, study, 3L), c("A", "B", "C"))
  expect_equal(top_weight_studies(c(1, 1, NA), c("B", "A", "C"), 1L), "A")
  expect_equal(top_weight_studies(w, study, 10L), c("A", "B", "C"))
})

test_that("calculate_drop_top_studies re-estimates without the highest-weight studies", {
  df <- make_supplement_data(n_meta = 3, seed = 21)
  out <- calculate_drop_top_studies(df)
  expect_equal(out$estimator, unname(supplement_estimators())[1:9])
  expect_true(all(out$drop1_n_metas == 3 & out$drop3_n_metas == 3))

  # Recompute HS by hand: drop the study with the largest summed sample size
  hs <- vapply(split(df, df$meta), function(m) {
    by_study <- tapply(m$sample_size, m$study, sum)
    top <- names(by_study)[order(-by_study, names(by_study))][1]
    rest <- m[m$study != top, ]
    c(sum(m$sample_size * m$effect) / sum(m$sample_size),
      sum(rest$sample_size * rest$effect) / sum(rest$sample_size))
  }, numeric(2))
  row <- out[out$estimator == "HS", ]
  expect_equal(row$full_mean, mean(hs[1, ]))
  expect_equal(row$drop1_mean, mean(hs[2, ]))
  expect_equal(row$drop1_change, mean(hs[2, ] - hs[1, ]))
})

test_that("study_medians collapses each study to its median PCC, df and sample size", {
  df <- make_supplement_data(n_meta = 2, seed = 22)
  med <- study_medians(df)
  expect_equal(nrow(med), length(unique(paste(df$meta, df$study))))
  s <- med$study[1]
  expect_equal(med$effect[1], median(df$effect[df$study == s]))
  expect_equal(med$dof[1], median(df$dof[df$study == s]))
  expect_equal(med$sample_size[1], median(df$sample_size[df$study == s]))
  # t and SE are rebuilt so that r_p is the median PCC
  r_p <- med$t_value / sqrt(med$t_value^2 + med$dof)
  expect_equal(r_p, med$effect, tolerance = 1e-12)
  expect_true(all(c("se_s1", "se_s2", "fishers_z", "fishers_z_se", "pcc3") %in% colnames(med)))
})

test_that("calculate_study_dependence changes nothing when every study has one estimate", {
  df <- make_supplement_data(n_meta = 3, seed = 23, one_per_study = TRUE)
  res <- make_supplement_results(df)
  out <- calculate_study_dependence(df, res, n_draws = 5, three_level = FALSE)
  expect_equal(out$estimator, unname(supplement_estimators()))
  expect_equal(out$study_median, out$baseline, tolerance = 1e-4)
  expect_equal(out$random_draw, out$baseline, tolerance = 1e-4)
  expect_equal(out$weights_by_m, out$baseline, tolerance = 1e-4)
  expect_true(all(is.na(out$three_level)))
})

test_that("calculate_study_dependence is reproducible and fits three-level models", {
  df <- make_supplement_data(n_meta = 2, seed = 24)
  res <- make_supplement_results(df)
  a <- calculate_study_dependence(df, res, n_draws = 10, seed = 1)
  b <- calculate_study_dependence(df, res, n_draws = 10, seed = 1)
  expect_identical(a, b)
  expect_equal(a$baseline[a$estimator == "RE1"], mean(res$re1_est))
  expect_false(is.na(a$three_level[a$estimator == "RE1"]))
  expect_equal(a$three_level_n_metas[a$estimator %in% c("RE1", "REz")], c(2L, 2L))
  expect_true(all(is.na(a$three_level[!a$estimator %in% c("RE1", "REz")])))

  # Three-level RE1 for the first MA, by hand
  m <- df[df$meta == "MA1", ]
  m$id <- seq_len(nrow(m))
  fit <- metafor::rma.mv(effect, se_s1^2, random = ~ 1 | study / id, data = m, method = "REML")
  expect_equal(three_level_estimate(m$effect, m$se_s1^2, m$study), as.numeric(fit$beta[1]),
               tolerance = 1e-6)
})

test_that("calculate_bootstrap_contrasts gives percentile intervals of the mean difference", {
  res <- data.frame(meta = paste0("m", 1:6), re1_est = c(0.1, 0.2, 0.3, 0.2, 0.1, 0.4),
                    uwls1_est = c(0.05, 0.1, 0.3, 0.1, 0.1, 0.2), hsma_est = 0.1)
  out <- calculate_bootstrap_contrasts(res, list(c("RE1", "UWLS1"), c("RE1", "HS")),
                                       n_boot = 500, seed = 3)
  expect_equal(out$contrast, c("RE1 - UWLS1", "RE1 - HS"))
  expect_equal(out$mean_diff[1], mean(res$re1_est - res$uwls1_est))
  expect_true(all(out$ci_lower <= out$mean_diff & out$mean_diff <= out$ci_upper))
  expect_identical(out, calculate_bootstrap_contrasts(res, list(c("RE1", "UWLS1"), c("RE1", "HS")),
                                                      n_boot = 500, seed = 3))
  # A constant difference has a degenerate interval
  res$uwls1_est <- res$re1_est - 0.05
  const <- calculate_bootstrap_contrasts(res, list(c("RE1", "UWLS1")), n_boot = 200, seed = 1)
  expect_equal(c(const$ci_lower, const$ci_upper, const$boot_se), c(0.05, 0.05, 0), tolerance = 1e-12)
})

test_that("calculate_subsample_means restricts to the stated subsamples", {
  res <- data.frame(meta = paste0("m", 1:4), n_studies = c(5, 12, 25, 60), k_ = c(10, 40, 30, 100),
                    flipped = c(TRUE, FALSE, FALSE, TRUE), re1_est = c(0.1, 0.2, 0.3, 0.4),
                    hsma_est = c(0.0, 0.1, 0.2, 0.3))
  out <- calculate_subsample_means(res)
  means <- out[out$statistic == "Mean", ]
  expect_equal(means$subsample, c("All", ">= 10 studies", ">= 20 studies", ">= 50 studies",
                                  ">= 30 estimates", "Flipped", "Not flipped"))
  expect_equal(means$n_metas, c(4, 3, 2, 1, 3, 2, 2))
  expect_equal(means$RE1, c(0.25, 0.3, 0.35, 0.4, 0.3, 0.25, 0.25))
  expect_equal(out$HS[out$statistic == "Median" & out$subsample == ">= 10 studies"], 0.2)
})

test_that("calculate_tau2_hs_variants compares REML / DL and df-weighted HS with the paper", {
  df <- make_supplement_data(n_meta = 3, seed = 25)
  res <- make_supplement_results(df)
  out <- calculate_tau2_hs_variants(df, res)
  expect_equal(nrow(out), 3 * 3 + 2)
  expect_equal(unique(out$estimator), c("RE1", "RE2", "HS", "REz"))
  paper <- out[grepl("paper", out$variant), ]
  expect_true(all(paper$mean_diff == 0))
  dl <- vapply(split(df, df$meta), function(m) {
    as.numeric(metafor::rma(yi = m$effect, sei = m$se_s1, method = "DL")$beta[1])
  }, numeric(1))
  row <- out[out$estimator == "RE1" & out$variant == "DL", ]
  expect_equal(row$mean, mean(dl))
  expect_equal(row$mean_diff, mean(dl - res$re1_est))
  hs_df <- vapply(split(df, df$meta), function(m) sum(m$dof * m$effect) / sum(m$dof), numeric(1))
  expect_equal(out$mean[out$variant == "df weights"], mean(hs_df))
})

test_that("calculate_fishers_z_variants uses 1 / (n - 3) for the alternative", {
  df <- make_supplement_data(n_meta = 2, seed = 26)
  df$fishers_z_se[1] <- NA # one estimate with df <= 1
  res <- make_supplement_results(df)
  out <- calculate_fishers_z_variants(df, res)
  expect_equal(out$estimator, c("REz", "REz", "UWLSz", "UWLSz"))
  expect_equal(out$n_estimates, rep(c(nrow(df) - 1, nrow(df)), 2))
  uwlsz_n3 <- vapply(split(df, df$meta), function(m) {
    w <- m$sample_size - 3
    tanh(sum(w * m$fishers_z) / sum(w))
  }, numeric(1))
  expect_equal(out$mean[out$estimator == "UWLSz" & out$variance == "1 / (n - 3)"], mean(uwlsz_n3))
  expect_equal(out$mean[out$estimator == "UWLSz" & grepl("paper", out$variance)], mean(res$uwlsz_est))
})

test_that("ols_hc3 matches the textbook HC3 sandwich", {
  set.seed(7)
  n <- 40
  x <- cbind(`(Intercept)` = 1, a = rnorm(n), b = runif(n))
  y <- drop(x %*% c(1, 0.5, -0.3)) + rnorm(n) * (1 + abs(x[, 2]))
  fit <- ols_hc3(y, x)
  lm_fit <- stats::lm(y ~ x - 1)
  expect_equal(fit$estimate, unname(stats::coef(lm_fit)))
  h <- stats::hatvalues(lm_fit)
  e <- stats::residuals(lm_fit)
  bread <- solve(t(x) %*% x)
  meat <- t(x) %*% diag(e^2 / (1 - h)^2) %*% x
  expect_equal(fit$se_hc3, unname(sqrt(diag(bread %*% meat %*% bread))))
  expect_equal(attr(fit, "r_squared"), summary(stats::lm(y ~ x[, -1]))$r.squared)
  expect_equal(fit$p_value[2], 2 * stats::pt(abs(fit$t_value[2]), n - 3, lower.tail = FALSE))
})

test_that("calculate_gap_regressions builds the indicators and fits each gap", {
  set.seed(8)
  n <- 30
  res <- data.frame(
    meta = paste0("m", 1:n), re1_est = runif(n, 0.1, 0.3), uwls1_est = runif(n, 0.1, 0.3),
    fat = rnorm(n), fat_se = runif(n, 0.5, 1), k_ = sample(10:500, n), n_studies = sample(5:50, n),
    median_n = sample(50:500, n), i2_uwls2 = runif(n, 20, 99), flipped = rep(c(TRUE, FALSE), n / 2)
  )
  psb <- data.frame(meta = rev(res$meta), ess_uwls = rnorm(n, 0.05, 0.05),
                    esig_uwls = runif(n, 0.2, 0.6), pss = runif(n, 0.3, 0.8), k = rev(res$k_))
  out <- calculate_gap_regressions(res, psb, contrasts = list(c("RE1", "UWLS1")))
  expect_equal(unique(out$indicator), c("ESS", "PSST", "Egger_t"))
  expect_equal(unique(out$term[out$indicator == "PSST"]),
               c("(Intercept)", "PSST", "log_k", "log_median_n", "i2", "flipped"))

  p <- psb[match(res$meta, psb$meta), ]
  psst <- (p$pss - p$esig_uwls) / sqrt(p$esig_uwls * (1 - p$esig_uwls) / p$k)
  gap <- res$re1_est - res$uwls1_est
  ref <- stats::lm(gap ~ psst + log(res$k_) + log(res$median_n) + I(res$i2_uwls2 / 100) +
                     as.numeric(res$flipped))
  expect_equal(out$estimate[out$indicator == "PSST"], unname(stats::coef(ref)))
  expect_equal(out$n[1], n)
  expect_error(calculate_gap_regressions(res, psb[-1, ]), "every meta-analysis")
})

test_that("calculate_leave_one_out reports the range over omitted meta-analyses", {
  res <- data.frame(meta = paste0("m", 1:5), re1_est = c(0.1, 0.2, 0.3, 0.4, 1.0),
                    uwls1_est = c(0.1, 0.1, 0.2, 0.3, 0.5), hsma_est = c(0.0, 0.1, 0.2, 0.2, 0.4),
                    uwlsz_est = c(0.05, 0.1, 0.25, 0.3, 0.45), fishers_z_est = c(0.1, 0.2, 0.3, 0.5, 1.1))
  out <- calculate_leave_one_out(res, contrasts = list(c("RE1", "UWLS1")))
  row <- out[out$statistic == "Mean RE1", ]
  expect_equal(row$full_sample, 0.4)
  expect_equal(row$loo_min, 0.25)
  expect_equal(row$dropped_at_min, "m5")
  expect_equal(row$loo_max, 0.475)
  expect_equal(row$dropped_at_max, "m1")
  d <- res$re1_est - res$uwls1_est
  expect_equal(out$full_sample[out$statistic == "Paired t RE1 - UWLS1"],
               unname(stats::t.test(d)$statistic))
  expect_equal(out$full_sample[out$statistic == "corr(HS, UWLSz)"], cor(res$hsma_est, res$uwlsz_est))
})

test_that("calculate_section6_tests reproduces t.test and cor", {
  set.seed(9)
  res <- data.frame(meta = paste0("m", 1:20), fishers_z_est = runif(20, 0.1, 0.4),
                    re1_est = runif(20, 0.1, 0.4), uwls1_est = runif(20, 0.1, 0.4),
                    uwlsz_est = runif(20, 0.1, 0.4), hsma_est = runif(20, 0.1, 0.4))
  out <- calculate_section6_tests(res)
  expect_equal(out$test, c(rep("Paired t", 3), rep("Correlation", 2)))
  tt <- stats::t.test(res$fishers_z_est, res$re1_est, paired = TRUE)
  expect_equal(out$t_value[1], unname(tt$statistic))
  expect_equal(out$p_value[1], tt$p.value)
  expect_equal(out$correlation[5], cor(res$hsma_est, res$fishers_z_est))
})

test_that("supplement_contrasts starts with the Section 6 comparisons", {
  cc <- supplement_contrasts()
  expect_equal(cc[1:3], list(c("REz", "RE1"), c("UWLS1", "UWLSz"), c("UWLSz", "HS")))
  labels <- unname(supplement_estimators())
  expect_true(all(unlist(cc) %in% labels))
})
