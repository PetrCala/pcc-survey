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
  expect_equal(out$estimator, unname(supplement_estimators()))
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
  expect_equal(row$baseline_mean, mean(hs[1, ]))
  expect_equal(row$baseline_median, median(hs[1, ]))
  expect_equal(row$drop1_mean, mean(hs[2, ]))
  expect_equal(row$drop1_median, median(hs[2, ]))

  # PET-PEESE drops the studies with the most weight in its WLS fits, 1 / S1^2
  pp <- vapply(split(df, df$meta), function(m) {
    by_study <- tapply(1 / m$se_s1^2, m$study, sum)
    top <- names(by_study)[order(-by_study, names(by_study))][1:3]
    fat_pet_peese(m[!m$study %in% top, ])$est
  }, numeric(1))
  expect_equal(out$drop3_mean[out$estimator == "PP"], mean(pp))
})

test_that("calculate_drop_top_studies leaves out MAs where PET-PEESE cannot be refitted", {
  df <- make_supplement_data(n_meta = 2, seed = 27, one_per_study = TRUE)
  # MA1 has 6 single-estimate studies: dropping 3 leaves 3, enough for PET-PEESE;
  # trim it to 5 studies so that only 2 estimates are left
  df <- df[!(df$meta == "MA1" & df$study == "MA1-S6"), ]
  out <- calculate_drop_top_studies(df)
  expect_equal(out$drop3_n_metas[out$estimator == "PP"], 1L)
  expect_equal(out$drop3_n_metas[out$estimator == "HS"], 2L)
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
  expect_equal(unique(out$analysis), c("Baseline", "Study's median estimate",
                                       "One random estimate per study", "Weights divided by m",
                                       "Three-level model (REML)"))
  expect_equal(colnames(out), c("analysis", "statistic", "n_metas", supplement_table_estimators()))
  est_cols <- supplement_table_estimators()
  base <- out[out$analysis == "Baseline", est_cols]
  for (a in c("Study's median estimate", "One random estimate per study", "Weights divided by m")) {
    expect_equal(unname(as.matrix(out[out$analysis == a, est_cols])), unname(as.matrix(base)),
                 tolerance = 1e-4)
  }
  expect_true(all(is.na(out[out$analysis == "Three-level model (REML)", est_cols])))
})

test_that("calculate_study_dependence is reproducible and fits three-level models", {
  df <- make_supplement_data(n_meta = 2, seed = 24)
  res <- make_supplement_results(df)
  a <- calculate_study_dependence(df, res, n_draws = 10, seed = 1)
  b <- calculate_study_dependence(df, res, n_draws = 10, seed = 1)
  expect_identical(a, b)
  base <- a[a$analysis == "Baseline", ]
  expect_equal(base$RE1, c(mean(res$re1_est), median(res$re1_est)))
  tl <- a[a$analysis == "Three-level model (REML)", ]
  expect_false(anyNA(tl[, c("RE1", "REz")]))
  expect_true(all(is.na(tl[, c("Mean", "UWLS1", "HS", "PP")])))
  expect_equal(tl$n_metas, c(2L, 2L))

  # Three-level RE1 for the first MA, by hand
  m <- df[df$meta == "MA1", ]
  m$id <- seq_len(nrow(m))
  fit <- metafor::rma.mv(effect, se_s1^2, random = ~ 1 | study / id, data = m, method = "REML")
  expect_equal(three_level_estimate(m$effect, m$se_s1^2, m$study), as.numeric(fit$beta[1]),
               tolerance = 1e-6)
})

test_that("calculate_study_dependence re-estimates tau2 with the down-weighted data", {
  df <- make_supplement_data(n_meta = 1, seed = 28)
  res <- make_supplement_results(df)
  out <- calculate_study_dependence(df, res, n_draws = 2, three_level = FALSE)
  m <- as.numeric(table(df$study)[df$study])
  a <- 1 / m
  ll <- function(t2) {
    w <- a / (df$se_s1^2 + t2)
    mu <- sum(w * df$effect) / sum(w)
    sum(a * (-0.5 * log(df$se_s1^2 + t2) - 0.5 * (df$effect - mu)^2 / (df$se_s1^2 + t2)))
  }
  t2 <- optimize(ll, c(0, 1), maximum = TRUE, tol = 1e-12)$maximum
  if (ll(0) > ll(t2)) t2 <- 0
  w <- a / (df$se_s1^2 + t2)
  by_m <- out[out$analysis == "Weights divided by m" & out$statistic == "Mean", ]
  expect_equal(by_m$RE1, sum(w * df$effect) / sum(w), tolerance = 1e-6)
  expect_equal(by_m$HS, sum(a * df$sample_size * df$effect) / sum(a * df$sample_size))
})

test_that("calculate_bootstrap_contrasts gives percentile intervals and paired t-tests", {
  res <- data.frame(meta = paste0("m", 1:6), re1_est = c(0.1, 0.2, 0.3, 0.2, 0.1, 0.4),
                    uwls1_est = c(0.05, 0.1, 0.3, 0.1, 0.1, 0.2), hsma_est = 0.1)
  out <- calculate_bootstrap_contrasts(res, list(c("RE1", "UWLS1"), c("RE1", "HS")),
                                       n_boot = 500, seed = 3)
  expect_equal(colnames(out), c("contrast", "mean_diff", "ci_lower", "ci_upper", "paired_t",
                                "p_value", "n_metas"))
  expect_equal(out$contrast, c("RE1 - UWLS1", "RE1 - HS"))
  expect_equal(out$mean_diff[1], mean(res$re1_est - res$uwls1_est))
  tt <- stats::t.test(res$re1_est, res$uwls1_est, paired = TRUE)
  expect_equal(out$paired_t[1], unname(tt$statistic))
  expect_equal(out$p_value[1], tt$p.value)
  expect_true(all(out$ci_lower <= out$mean_diff & out$mean_diff <= out$ci_upper))
  expect_identical(out, calculate_bootstrap_contrasts(res, list(c("RE1", "UWLS1"), c("RE1", "HS")),
                                                      n_boot = 500, seed = 3))
  # A constant difference has a degenerate interval
  res$uwls1_est <- res$re1_est - 0.05
  const <- calculate_bootstrap_contrasts(res, list(c("RE1", "UWLS1")), n_boot = 200, seed = 1)
  expect_equal(c(const$ci_lower, const$ci_upper), c(0.05, 0.05), tolerance = 1e-12)
  expect_true(is.na(const$paired_t))
})

test_that("calculate_subsample_means restricts to the stated subsamples", {
  res <- data.frame(meta = paste0("m", 1:4), n_studies = c(5, 12, 25, 60), k_ = c(10, 40, 30, 100),
                    flipped = c(TRUE, FALSE, FALSE, TRUE), re1_est = c(0.1, 0.2, 0.3, 0.4),
                    re2_est = 1, hsma_est = c(0.0, 0.1, 0.2, 0.3))
  out <- calculate_subsample_means(res)
  expect_false("RE2" %in% colnames(out))
  means <- out[out$statistic == "Mean", ]
  expect_equal(means$subsample, c("All", "At least 10 studies", "At least 20 studies",
                                  "At least 50 studies", "At least 30 estimates", "Flipped",
                                  "Not flipped"))
  expect_equal(means$n_metas, c(4, 3, 2, 1, 3, 2, 2))
  expect_equal(means$RE1, c(0.25, 0.3, 0.35, 0.4, 0.3, 0.25, 0.25))
  expect_equal(out$HS[out$statistic == "Median" & out$subsample == "At least 10 studies"], 0.2)
})

test_that("calculate_tau2_hs_variants compares REML / DL and df-weighted HS with the baseline", {
  df <- make_supplement_data(n_meta = 3, seed = 25)
  res <- make_supplement_results(df)
  out <- calculate_tau2_hs_variants(df, res)
  expect_equal(out$change, c(rep("REML", 3), rep("DerSimonian-Laird", 3), "HS with df weights"))
  expect_equal(out$estimator, c("RE1", "RE2", "REz", "RE1", "RE2", "REz", "HS"))
  dl <- vapply(split(df, df$meta), function(m) {
    as.numeric(metafor::rma(yi = m$effect, sei = m$se_s1, method = "DL")$beta[1])
  }, numeric(1))
  row <- out[out$estimator == "RE1" & out$change == "DerSimonian-Laird", ]
  expect_equal(row$baseline_mean, mean(res$re1_est))
  expect_equal(row$new_mean, mean(dl))
  expect_equal(row$new_median, median(dl))
  expect_equal(row$mean_change, mean(dl - res$re1_est))
  hs_df <- vapply(split(df, df$meta), function(m) sum(m$dof * m$effect) / sum(m$dof), numeric(1))
  expect_equal(out$new_mean[out$change == "HS with df weights"], mean(hs_df))
})

test_that("calculate_fishers_z_variants reports both variances as in the draft's Table S7", {
  df <- make_supplement_data(n_meta = 3, seed = 26)
  df$fishers_z_se[1] <- NA # one estimate with df <= 1
  res <- make_supplement_results(df)
  out <- calculate_fishers_z_variants(df, res)
  expect_equal(out$estimator, rep(c("REz", "UWLSz", "HS"), 2))
  expect_equal(out$n_estimates[out$estimator != "HS"], c(nrow(df) - 1, nrow(df) - 1, nrow(df), nrow(df)))
  n3 <- out[out$variance == "1/(n - 3) (first revision)", ]
  uwlsz_n3 <- vapply(split(df, df$meta), function(m) {
    w <- m$sample_size - 3
    tanh(sum(w * m$fishers_z) / sum(w))
  }, numeric(1))
  expect_equal(n3$mean[n3$estimator == "UWLSz"], mean(uwlsz_n3))
  expect_equal(n3$corr_with_hs[n3$estimator == "UWLSz"], cor(uwlsz_n3, res$hsma_est))
  expect_equal(n3$paired_t[n3$estimator == "UWLSz"],
               unname(stats::t.test(uwlsz_n3, res$hsma_est, paired = TRUE)$statistic))
  paper <- out[out$variance == "1/(df - 1) (paper)", ]
  expect_equal(paper$mean[paper$estimator == "UWLSz"], mean(res$uwlsz_est))
  expect_equal(paper$flipped[paper$estimator == "REz"], mean(res$fishers_z_est[res$flipped]))
  counts <- calculate_smallest_estimate_counts(res)
  expect_equal(paper$smallest, counts$times_smallest[match(c("REz", "UWLSz", "HS"), counts$estimator)])
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

test_that("selection_measures matches the PSB analysis and the PSST formula", {
  df <- make_supplement_data(n_meta = 3, seed = 29)
  res <- make_supplement_results(df)
  sel <- selection_measures(df, res)
  psb <- do.call(rbind, lapply(split(df, df$meta), get_psb_metaflavours))
  expect_equal(sel$ESS, psb$ess_uwls)
  expect_equal(sel$PSST, (psb$pss - psb$esig_uwls) / sqrt(psb$esig_uwls * (1 - psb$esig_uwls) / psb$k))
  expect_equal(sel$Egger_t, res$fat / res$fat_se)
  expect_equal(sel$Abs_Egger_t, abs(sel$Egger_t))
  m <- split(df, df$meta)[[1]]
  tau2 <- get_re1_tau2(m)
  expect_equal(sel$tau2_re1[1], tau2)
  expect_equal(sel$ESS_RE[1], calculate_psb_measures(m, res$re1_est[1], tau2)$ess_prop)
})

test_that("calculate_gap_regressions scales the slope to one SD and splits by selection", {
  df <- make_supplement_data(n_meta = 12, seed = 30)
  res <- make_supplement_results(df)
  out <- calculate_gap_regressions(res, df)
  expect_equal(unique(out$selection_measure)[1:5],
               c("ESS", "PSST", "Egger t", "Absolute Egger t", "ESS with RE-based expected significance"))
  expect_equal(unique(out$gap), c("RE1 - UWLS1", "RE1 - HS", "Mean - UWLS1"))

  sel <- selection_measures(df, res)
  gap <- res$re1_est - res$uwls1_est
  ref <- stats::lm(gap ~ sel$PSST + sel$tau2_re1 + sel$i2_re1 + log(res$k_) + log(res$n_studies) +
                     log(res$median_n))
  row <- out[out$selection_measure == "PSST" & out$gap == "RE1 - UWLS1", ]
  expect_equal(row$estimate, unname(stats::coef(ref)[2]) * sd(sel$PSST))
  expect_equal(row$n_metas, 12)

  no_sign <- sel$ESS <= 0 & abs(sel$Egger_t) < 1.645
  sub <- out[is.na(out$se_hc3) & out$gap == "RE1 - UWLS1", ]
  expect_equal(sub$n_metas, c(sum(no_sign), sum(!no_sign)))
  expect_equal(sub$selection_measure[1],
               sprintf("Mean gap in %d MAs with no sign of selection", sum(no_sign)))
})

test_that("calculate_leave_one_out reports the range of the mean differences", {
  res <- data.frame(meta = paste0("m", 1:5), re1_est = c(0.1, 0.2, 0.3, 0.4, 1.0),
                    uwls1_est = c(0.1, 0.1, 0.2, 0.3, 0.5))
  out <- calculate_leave_one_out(res, contrasts = list(c("RE1", "UWLS1")))
  d <- res$re1_est - res$uwls1_est
  expect_equal(out$full_sample, mean(d))
  loo <- vapply(1:5, function(i) mean(d[-i]), numeric(1))
  expect_equal(out$minimum, min(loo))
  expect_equal(out$maximum, max(loo))
  expect_equal(out$dropped_at_min, "m5")
  expect_equal(out$dropped_at_max, "m1")
})

test_that("calculate_section6_tests reproduces t.test and cor", {
  set.seed(9)
  cols <- c("re1_est", "re2_est", "uwls1_est", "uwls3_est", "hsma_est", "fishers_z_est", "uwlsz_est")
  res <- data.frame(meta = paste0("m", 1:20))
  for (col in cols) res[[col]] <- runif(20, 0.1, 0.4)
  out <- calculate_section6_tests(res)
  expect_equal(out$test, c(rep("Paired t", 8), rep("Correlation", 2)))
  expect_equal(paste(out$estimator_a, out$estimator_b)[1:2], c("RE2 RE1", "REz RE1"))
  tt <- stats::t.test(res$fishers_z_est, res$re1_est, paired = TRUE)
  expect_equal(out$t_value[2], unname(tt$statistic))
  expect_equal(out$p_value[2], tt$p.value)
  expect_equal(out$correlation[10], cor(res$hsma_est, res$fishers_z_est))
})

test_that("supplement_contrasts are the draft's five contrasts", {
  cc <- supplement_contrasts()
  expect_equal(vapply(cc, contrast_label, character(1)),
               c("RE1 - UWLS1", "UWLS1 - HS", "REz - RE1", "UWLSz - HS", "UWLS+3 - HS"))
  labels <- unname(supplement_estimators())
  expect_true(all(unlist(c(cc, section6_pairs())) %in% labels))
})
