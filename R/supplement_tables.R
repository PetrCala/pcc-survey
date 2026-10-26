# Online supplement: robustness tables S2 to S8 and S10, and the Section 6
# paired t-tests and correlations
#
# Table S1 (weight concentration) is calculate_weight_shares() in
# R/weight_concentration.R and Table S9 (the calibrated simulation) is in
# R/supplement_simulation.R. run_supplement_analysis() runs them all and writes
# one CSV per table to output/supplement/. The definitions follow the text and
# table notes of the online-supplement draft (October 2026).
#
# Shared conventions, as in Table 1: the 172 meta-analyses after
# validate_pcc_observations() and the sign alignment of
# convert_inverse_relationships(); RE with the ML tau2; S1 / S2 standard
# errors; HS weighted by sample size; Fisher's z with variance 1 / (df - 1),
# leaving estimates with df <= 1 out of REz and UWLSz only; PET-PEESE as in
# fat_pet_peese(). Every meta-analysis counts equally: a table's "mean" and
# "median" are taken across meta-analyses of the per-MA estimates. Estimators
# are named by their Table 1 labels (Mean, RE1, RE2, UWLS1, UWLS2, UWLS+3, HS,
# REz, UWLSz, PP).

#' Paired estimator contrasts of Tables S4 and S10
#'
#' Each contrast is `c(a, b)` for the per-MA difference a - b, with Table 1
#' labels: RE1 - UWLS1, UWLS1 - HS, REz - RE1, UWLSz - HS and UWLS+3 - HS.
#'
#' @return [list] Character pairs.
#' @export
supplement_contrasts <- function() {
  list(
    c("RE1", "UWLS1"),
    c("UWLS1", "HS"),
    c("REz", "RE1"),
    c("UWLSz", "HS"),
    c("UWLS+3", "HS")
  )
}

#' Paired comparisons quoted in Section 6 of the paper
#'
#' RE2 - RE1 and the seven comparisons of the paragraph on systematic
#' differences: REz - RE1, RE1 - UWLS1, UWLS1 - UWLS+3, UWLS1 - UWLSz,
#' RE1 - HS, UWLSz - HS and UWLS+3 - HS.
#'
#' @return [list] Character pairs.
#' @export
section6_pairs <- function() {
  list(
    c("RE2", "RE1"),
    c("REz", "RE1"),
    c("RE1", "UWLS1"),
    c("UWLS1", "UWLS+3"),
    c("UWLS1", "UWLSz"),
    c("RE1", "HS"),
    c("UWLSz", "HS"),
    c("UWLS+3", "HS")
  )
}

# Estimators shown in Tables S3 and S5 (and simulated in Table S9): Table 1
# without RE2 and UWLS2.
supplement_table_estimators <- function() {
  c("Mean", "RE1", "UWLS1", "UWLS+3", "HS", "REz", "UWLSz", "PP")
}

contrast_label <- function(contrast) paste(contrast[1], "-", contrast[2])

# Per-MA results reordered to follow `metas` (the meta-analysis names of
# split(pcc_df, pcc_df$meta)).
align_results <- function(results, metas) {
  idx <- match(metas, results$meta)
  if (anyNA(idx)) cli::cli_abort("The per-MA results do not cover every meta-analysis")
  results[idx, , drop = FALSE]
}

# Mean and median across meta-analyses, ignoring NA
mean_median <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(c(NA_real_, NA_real_))
  c(mean(x), stats::median(x))
}

# Paired t-test of a - b across meta-analyses (complete pairs), as
# stats::t.test(paired = TRUE); t and p are NA when the differences are
# essentially constant (t.test's own rule), where t.test stops with an error.
paired_test <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  d <- a[ok] - b[ok]
  n <- length(d)
  se <- if (n > 1) stats::sd(d) / sqrt(n) else NA_real_
  constant <- is.na(se) || se < 10 * .Machine$double.eps * abs(mean(d))
  t_value <- if (!constant) mean(d) / se else NA_real_
  p <- if (is.na(t_value)) NA_real_ else 2 * stats::pt(abs(t_value), n - 1, lower.tail = FALSE)
  list(n = n, mean_diff = mean(d), t = t_value, df = n - 1, p = p)
}

# Fit one estimator on one meta-analysis and return its estimate and
# per-estimate weights (NA weights, e.g. REz rows with df <= 1, count as zero).
# PET-PEESE's weights are those of its WLS fits, 1 / S1^2.
fit_estimator_with_weights <- function(df, key, re_method = "ML") {
  fit <- switch(key,
    simple_mean = simple_mean(df),
    re1 = re(df, se = df$se_s1, method = re_method),
    re2 = re(df, se = df$se_s2, method = re_method),
    uwls1 = uwls(df, se = df$se_s1),
    uwls2 = uwls(df, se = df$se_s2),
    uwls3 = uwls3(df),
    hsma = hsma(df),
    fishers_z = fishers_z(df, method = re_method),
    uwlsz = uwls_fishers_z(df),
    petpeese = list(est = fat_pet_peese(df, se = df$se_s1, alpha = 0.1)$est,
                    weights = 1 / df$se_s1^2),
    cli::cli_abort("No weights for estimator {.val {key}}")
  )
  w <- fit$weights
  w[is.na(w)] <- 0
  list(est = fit$est, weights = w)
}

#' Primary studies carrying the most weight
#'
#' @param weights [numeric] Per-estimate weights.
#' @param study [vector] Primary-study identifier per estimate.
#' @param top_n [integer] Number of studies to return.
#' @return [character] The `top_n` studies with the largest summed weight
#'   (ties broken by study name).
#' @keywords internal
top_weight_studies <- function(weights, study, top_n = 1L) {
  weights[is.na(weights)] <- 0
  by_study <- tapply(weights, as.character(study), sum)
  ord <- order(-by_study, names(by_study))
  names(by_study)[ord[seq_len(min(top_n, length(by_study)))]]
}

#' Table S2: dropping the highest-weight primary studies
#'
#' For each estimator and meta-analysis, finds the primary study (top 1) and the
#' three primary studies (top 3) that carry the most weight under that
#' estimator (a study's weight is the sum of its estimates' weights, as in
#' Table S1; for PET-PEESE, the weights 1 / S1^2 of its WLS fits), drops them
#' and re-estimates the same estimator on the remaining estimates. A
#' meta-analysis drops out of a column when the estimator cannot be computed
#' on what is left (PET-PEESE needs three estimates).
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param re_method [character] tau2 estimator for RE1, RE2 and REz.
#' @return [data.frame] One row per Table 1 estimator: the mean and median
#'   across meta-analyses at baseline (`baseline_mean`, `baseline_median`) and
#'   after dropping the top study (`drop1_*`) and the top three studies
#'   (`drop3_*`), and the number of meta-analyses re-estimated
#'   (`drop1_n_metas`, `drop3_n_metas`).
#' @export
calculate_drop_top_studies <- function(pcc_df, re_method = "ML") {
  keys <- names(supplement_estimators())
  metas <- split(pcc_df, pcc_df$meta)

  per_meta <- lapply(metas, function(df) {
    n_studies <- length(unique(df$study))
    vapply(keys, function(key) {
      full <- fit_estimator_with_weights(df, key, re_method)
      dropped <- vapply(c(1L, 3L), function(top_n) {
        if (n_studies <= top_n) return(NA_real_)
        drop <- top_weight_studies(full$weights, df$study, top_n)
        rest <- df[!as.character(df$study) %in% drop, , drop = FALSE]
        meta_estimates(rest, estimators = key, re_method = re_method)[[key]]
      }, numeric(1))
      c(full$est, dropped)
    }, numeric(3))
  })

  rows <- lapply(seq_along(keys), function(i) {
    vals <- vapply(per_meta, function(m) m[, i], numeric(3))
    base <- mean_median(vals[1, ])
    d1 <- mean_median(vals[2, ])
    d3 <- mean_median(vals[3, ])
    data.frame(
      estimator = unname(supplement_estimators()[keys[i]]),
      baseline_mean = base[1],
      baseline_median = base[2],
      drop1_mean = d1[1],
      drop1_median = d1[2],
      drop3_mean = d3[1],
      drop3_median = d3[2],
      drop1_n_metas = sum(!is.na(vals[2, ])),
      drop3_n_metas = sum(!is.na(vals[3, ]))
    )
  })
  do.call(rbind, rows)
}

#' Collapse each primary study to its median estimate
#'
#' Within each meta-analysis, replaces a study's estimates by one row with the
#' median PCC, median degrees of freedom and median sample size. The S1
#' standard error and t-value are then recomputed from that PCC and df (so
#' r_p = t / sqrt(t^2 + df) is the median PCC), and the derived columns follow
#' from [compute_derived_quantities()].
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @return [data.frame] One row per study and meta-analysis, with the derived
#'   columns.
#' @export
study_medians <- function(pcc_df) {
  key <- paste(pcc_df$meta, pcc_df$study, sep = "\r")
  groups <- split(seq_len(nrow(pcc_df)), key)
  first <- vapply(groups, `[`, integer(1), 1L)
  med <- function(col) vapply(groups, function(i) stats::median(pcc_df[[col]][i]), numeric(1))

  out <- data.frame(
    meta = pcc_df$meta[first],
    study = pcc_df$study[first],
    effect = med("effect"),
    dof = med("dof"),
    sample_size = med("sample_size"),
    stringsAsFactors = FALSE
  )
  out$se <- sqrt((1 - out$effect^2) / out$dof)
  out$t_value <- out$effect / out$se
  out <- out[order(out$meta, out$study), , drop = FALSE]
  rownames(out) <- NULL
  compute_derived_quantities(out)
}

# Random one-estimate-per-study draws for one meta-analysis: the per-draw
# estimates averaged over draws.
random_draw_estimates <- function(df, n_draws) {
  rows_by_study <- split(seq_len(nrow(df)), as.character(df$study))
  idx <- do.call(rbind, lapply(rows_by_study, function(rows) {
    rows[sample.int(length(rows), n_draws, replace = TRUE)]
  }))
  pick <- function(col) matrix(df[[col]][idx], nrow = nrow(idx))
  est <- estimators_matrix(
    effect = pick("effect"), se_s1 = pick("se_s1"), se_s2 = pick("se_s2"),
    pcc3 = pick("pcc3"), fishers_z = pick("fishers_z"),
    fishers_z_se = pick("fishers_z_se"), sample_size = pick("sample_size")
  )
  colMeans(est)
}

# Three-level random-effects model (estimates nested in studies) fitted by
# REML; returns NA if the fit fails.
three_level_estimate <- function(yi, vi, study) {
  keep <- !is.na(yi) & !is.na(vi)
  dat <- data.frame(yi = yi[keep], vi = vi[keep], study = as.character(study[keep]))
  dat$estimate_id <- seq_len(nrow(dat))
  fit <- tryCatch(
    suppressWarnings(metafor::rma.mv(
      yi, vi, random = ~ 1 | study / estimate_id, data = dat,
      method = "REML", sparse = TRUE
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) NA_real_ else as.numeric(fit$beta[1])
}

#' Table S3: within-study dependence
#'
#' Sensitivity of the estimators to multiple estimates per study, as the mean
#' and median across meta-analyses:
#'
#' * Baseline: the Table 1 estimates.
#' * Study's median estimate: each study collapsed to its median estimate
#'   ([study_medians()]), then every estimator applied as in Table 1.
#' * One random estimate per study: one estimate drawn at random from each
#'   study, `n_draws` times; a meta-analysis's value is its average over the
#'   draws.
#' * Weights divided by m: all estimates kept and each estimate's weight
#'   divided by m, its study's number of estimates; for RE and REz, tau2 is
#'   re-estimated with the down-weighted data (each estimate's likelihood
#'   contribution also divided by m, see [re_matrix()]).
#' * Three-level model (REML): RE1 and REz from a three-level model with
#'   between-study and within-study variances, on the S1 variance and on
#'   Fisher's z respectively.
#'
#' The random draws and the m-weighted estimates use the column-wise
#' estimators of [estimators_matrix()].
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param results [data.frame|NULL] Per-MA results of [pcc_survey_analyse()]
#'   for the baseline rows; recomputed with [meta_estimates()] when NULL.
#' @param n_draws [integer] Number of random draws.
#' @param seed [integer] Seed for the random draws.
#' @param re_method [character] tau2 estimator for the study-median fits.
#' @param three_level [logical] Whether to fit the three-level models (slow on
#'   the largest meta-analyses).
#' @return [data.frame] Columns `analysis`, `statistic` ("Mean" or "Median"),
#'   `n_metas` and one column per estimator (Mean, RE1, UWLS1, UWLS+3, HS, REz,
#'   UWLSz, PP); the three-level rows fill RE1 and REz only.
#' @export
calculate_study_dependence <- function(pcc_df, results = NULL, n_draws = 200L,
                                       seed = 2026L, re_method = "ML",
                                       three_level = TRUE) {
  all_labels <- unname(supplement_estimators())
  shown <- supplement_table_estimators()
  metas <- split(pcc_df, pcc_df$meta)

  baseline <- if (is.null(results)) {
    est <- t(vapply(metas, meta_estimates, numeric(length(all_labels)), re_method = re_method))
    colnames(est) <- all_labels
    est
  } else {
    as.matrix(results_estimates(align_results(results, names(metas))))
  }

  medians <- study_medians(pcc_df)
  median_est <- t(vapply(split(medians, medians$meta), meta_estimates,
                         numeric(length(all_labels)), re_method = re_method))
  colnames(median_est) <- all_labels

  set.seed(seed)
  draw_est <- t(vapply(metas, random_draw_estimates, numeric(length(all_labels)),
                       n_draws = n_draws))

  by_m_est <- t(vapply(metas, function(df) {
    m <- as.numeric(table(df$study)[as.character(df$study)])
    estimators_matrix(
      effect = df$effect, se_s1 = df$se_s1, se_s2 = df$se_s2, pcc3 = df$pcc3,
      fishers_z = df$fishers_z, fishers_z_se = df$fishers_z_se,
      sample_size = df$sample_size, mult = 1 / m
    )[1, ]
  }, numeric(length(all_labels))))

  three_level_est <- matrix(NA_real_, nrow = length(metas), ncol = length(all_labels),
                            dimnames = list(NULL, all_labels))
  if (three_level) {
    fits <- t(vapply(metas, function(df) {
      c(
        three_level_estimate(df$effect, df$se_s1^2, df$study),
        tanh(three_level_estimate(df$fishers_z, df$fishers_z_se^2, df$study))
      )
    }, numeric(2)))
    three_level_est[, "RE1"] <- fits[, 1]
    three_level_est[, "REz"] <- fits[, 2]
  }

  analyses <- list(
    "Baseline" = baseline,
    "Study's median estimate" = median_est,
    "One random estimate per study" = draw_est,
    "Weights divided by m" = by_m_est,
    "Three-level model (REML)" = three_level_est
  )
  rows <- list()
  for (name in names(analyses)) {
    stats_by_est <- vapply(shown, function(l) mean_median(analyses[[name]][, l]), numeric(2))
    n_metas <- max(colSums(!is.na(analyses[[name]][, shown, drop = FALSE])))
    for (i in 1:2) {
      rows[[length(rows) + 1]] <- data.frame(
        analysis = name, statistic = c("Mean", "Median")[i], n_metas = n_metas,
        t(stats_by_est[i, ]), check.names = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Table S4: paired differences between estimators
#'
#' For each contrast a - b: the mean across meta-analyses of the per-MA
#' difference, its 95% percentile bootstrap interval (meta-analyses resampled
#' with replacement `n_boot` times, the same resamples for every contrast) and
#' the paired t-test that treats the meta-analyses as independent.
#'
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()].
#' @param contrasts [list] Pairs of Table 1 labels (see [supplement_contrasts()]).
#' @param n_boot [integer] Number of bootstrap resamples.
#' @param seed [integer] Seed for the resampling.
#' @param conf [numeric] Confidence level.
#' @return [data.frame] One row per contrast: `contrast`, `mean_diff`,
#'   `ci_lower`, `ci_upper`, `paired_t`, `p_value` (two-sided) and `n_metas`.
#' @export
calculate_bootstrap_contrasts <- function(results, contrasts = supplement_contrasts(),
                                          n_boot = 10000L, seed = 2026L, conf = 0.95) {
  est <- results_estimates(results)
  n <- nrow(est)
  set.seed(seed)
  idx <- matrix(sample.int(n, n * n_boot, replace = TRUE), nrow = n)
  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)

  rows <- lapply(contrasts, function(cc) {
    d <- est[[cc[1]]] - est[[cc[2]]]
    boot <- colMeans(matrix(d[idx], nrow = n), na.rm = TRUE)
    q <- stats::quantile(boot, probs = probs, names = FALSE, na.rm = TRUE)
    tt <- paired_test(est[[cc[1]]], est[[cc[2]]])
    data.frame(
      contrast = contrast_label(cc),
      mean_diff = tt$mean_diff,
      ci_lower = q[1],
      ci_upper = q[2],
      paired_t = tt$t,
      p_value = tt$p,
      n_metas = tt$n
    )
  })
  do.call(rbind, rows)
}

#' Table S5: estimates in subsamples of meta-analyses
#'
#' The mean and median across meta-analyses of each estimator in subsamples:
#' all, at least 10, 20 and 50 primary studies (distinct studies), at least 30
#' estimates, and sign-flipped and not flipped (see
#' [convert_inverse_relationships()]).
#'
#' @param results [data.frame] Per-MA results with `n_studies`, `k_` and
#'   `flipped`.
#' @return [data.frame] Columns `subsample`, `n_metas`, `statistic` ("Mean" or
#'   "Median") and one column per estimator (Mean, RE1, UWLS1, UWLS+3, HS, REz,
#'   UWLSz, PP).
#' @export
calculate_subsample_means <- function(results) {
  results <- results[results$meta != "All meta-analyses", , drop = FALSE]
  est <- results_estimates(results)
  est <- est[, intersect(supplement_table_estimators(), colnames(est)), drop = FALSE]
  flipped <- as.logical(results$flipped)
  subsamples <- list(
    "All" = rep(TRUE, nrow(results)),
    "At least 10 studies" = results$n_studies >= 10,
    "At least 20 studies" = results$n_studies >= 20,
    "At least 50 studies" = results$n_studies >= 50,
    "At least 30 estimates" = results$k_ >= 30,
    "Flipped" = flipped,
    "Not flipped" = !flipped
  )

  rows <- list()
  for (name in names(subsamples)) {
    sel <- subsamples[[name]] & !is.na(subsamples[[name]])
    vals <- vapply(est, function(x) mean_median(x[sel]), numeric(2))
    for (i in 1:2) {
      rows[[length(rows) + 1]] <- data.frame(
        subsample = name, n_metas = sum(sel), statistic = c("Mean", "Median")[i],
        t(vals[i, ]), check.names = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Table S6: alternative heterogeneity estimators and HS weights
#'
#' Changes one convention at a time: the tau2 estimator in RE1, RE2 and REz
#' (REML or DerSimonian-Laird instead of ML) and the HS weights (degrees of
#' freedom instead of sample size).
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()] (the
#'   baseline).
#' @param re_methods [character] Alternative tau2 estimators, named by their
#'   label in the table.
#' @return [data.frame] One row per change and estimator: `change`,
#'   `estimator`, `baseline_mean`, `new_mean`, `new_median`, `mean_change` (the
#'   mean within-MA difference between the new and the baseline estimate) and
#'   `n_metas`.
#' @export
calculate_tau2_hs_variants <- function(pcc_df, results,
                                       re_methods = c(REML = "REML", "DerSimonian-Laird" = "DL")) {
  metas <- split(pcc_df, pcc_df$meta)
  base <- results_estimates(align_results(results, names(metas)))
  re_keys <- c(re1 = "RE1", re2 = "RE2", fishers_z = "REz")

  variant_row <- function(change, label, new) {
    both <- !is.na(new) & !is.na(base[[label]])
    data.frame(
      change = change, estimator = label,
      baseline_mean = mean(base[[label]], na.rm = TRUE),
      new_mean = mean(new, na.rm = TRUE),
      new_median = stats::median(new, na.rm = TRUE),
      mean_change = mean(new[both] - base[[label]][both]),
      n_metas = sum(!is.na(new))
    )
  }

  rows <- list()
  for (change in names(re_methods)) {
    est <- t(vapply(metas, meta_estimates, numeric(length(re_keys)),
                    estimators = names(re_keys), re_method = re_methods[[change]]))
    for (key in names(re_keys)) {
      rows[[length(rows) + 1]] <- variant_row(change, re_keys[[key]], est[, key])
    }
  }
  hs_dof <- vapply(metas, function(df) meta_estimates(df, "hsma", hs_weights = "dof")[["hsma"]],
                   numeric(1))
  rows[[length(rows) + 1]] <- variant_row("HS with df weights", "HS", hs_dof)

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Table S7: Fisher's z under the two variance conventions
#'
#' REz and UWLSz under the paper's partial-correlation variance 1 / (df - 1)
#' (estimates with df <= 1 left out) and the first revision's 1 / (n - 3)
#' (every estimate kept, since validation requires n > 3): mean and median
#' across meta-analyses, the number of meta-analyses in which the estimator is
#' the smallest of the nine weighted averages of Table 1 ([calculate_smallest_estimate_counts()],
#' with REz and UWLSz under that variance), the mean over the sign-flipped
#' meta-analyses, the paired t-test against RE1 (REz) or HS (UWLSz), and the
#' correlation with HS. An HS row per variance gives HS's Smallest count.
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()] (the
#'   paper's 1 / (df - 1) versions).
#' @return [data.frame] One row per variance and estimator: `variance`,
#'   `estimator`, `mean`, `median`, `smallest`, `flipped`, `paired_t`,
#'   `corr_with_hs` and `n_estimates` (estimates used by REz and UWLSz).
#' @export
calculate_fishers_z_variants <- function(pcc_df, results) {
  metas <- split(pcc_df, pcc_df$meta)
  res <- align_results(results, names(metas))
  n3 <- t(vapply(metas, meta_estimates, numeric(2), estimators = c("fishers_z", "uwlsz"),
                 fishers_z_variance = "n_minus_3"))
  res_n3 <- res
  res_n3$fishers_z_est <- n3[, "fishers_z"]
  res_n3$uwlsz_est <- n3[, "uwlsz"]
  variants <- list(
    "1/(df - 1) (paper)" = list(res = res, n_est = sum(!is.na(pcc_df$fishers_z_se))),
    "1/(n - 3) (first revision)" = list(res = res_n3, n_est = sum(pcc_df$sample_size > 3))
  )
  comparator <- c(REz = "RE1", UWLSz = "HS")

  rows <- list()
  for (name in names(variants)) {
    v <- variants[[name]]
    est <- results_estimates(v$res)
    smallest <- calculate_smallest_estimate_counts(v$res)
    flipped <- as.logical(v$res$flipped)
    for (label in c("REz", "UWLSz", "HS")) {
      x <- est[[label]]
      mm <- mean_median(x)
      rows[[length(rows) + 1]] <- data.frame(
        variance = name, estimator = label, mean = mm[1], median = mm[2],
        smallest = smallest$times_smallest[smallest$estimator == label],
        flipped = mean(x[flipped], na.rm = TRUE),
        paired_t = if (label == "HS") NA_real_ else paired_test(x, est[[comparator[[label]]]])$t,
        corr_with_hs = if (label == "HS") NA_real_ else stats::cor(x, est$HS, use = "complete.obs"),
        n_estimates = if (label == "HS") nrow(pcc_df) else v$n_est
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' OLS with HC3 standard errors
#'
#' @param y [numeric] Outcome.
#' @param x [matrix] Regressors, including the intercept column.
#' @return [data.frame] One row per regressor: `term`, `estimate`, `se_hc3`,
#'   `t_value` and `p_value` (t distribution with n - p df), with the sample
#'   size `n` and `r_squared` as attributes.
#' @keywords internal
ols_hc3 <- function(y, x) {
  xtx_inv <- solve(crossprod(x))
  beta <- drop(xtx_inv %*% crossprod(x, y))
  resid <- drop(y - x %*% beta)
  hat <- rowSums((x %*% xtx_inv) * x)
  meat <- crossprod(x * (resid / (1 - hat)))
  vcov <- xtx_inv %*% meat %*% xtx_inv
  se <- sqrt(diag(vcov))
  df_resid <- nrow(x) - ncol(x)
  out <- data.frame(
    term = colnames(x),
    estimate = beta,
    se_hc3 = se,
    t_value = beta / se,
    p_value = 2 * stats::pt(abs(beta / se), df = df_resid, lower.tail = FALSE),
    row.names = NULL
  )
  attr(out, "n") <- nrow(x)
  attr(out, "r_squared") <- 1 - sum(resid^2) / sum((y - mean(y))^2)
  out
}

#' Per-MA measures of selection intensity (Table S8)
#'
#' * `ESS`: the observed share of positive estimates significant at the 5%
#'   level (one-sided) minus the share expected under the UWLS1 estimate and
#'   RE1 heterogeneity, as in psb_results.csv ([get_psb_metaflavours()]).
#' * `PSST`: (Pss - Esig) / sqrt(Esig (1 - Esig) / k), with the same observed
#'   and expected shares (as in secondary-stats/IndividualPSST.dta).
#' * `Egger_t`: the FAT (Egger) coefficient over its standard error, from the
#'   PET fit of [fat_pet_peese()]; `Abs_Egger_t` its absolute value.
#' * `ESS_RE`: ESS with the expected share computed under the RE1 estimate
#'   instead of UWLS1 (RE1 heterogeneity in both).
#' * `tau2_re1`, `i2_re1`: the heterogeneity of the RE1 fit, the controls of
#'   Table S8.
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()].
#' @param alpha [numeric] One-sided significance level.
#' @return [data.frame] One row per meta-analysis (in `split()` order): `meta`
#'   and the measures above.
#' @export
selection_measures <- function(pcc_df, results, alpha = 0.05) {
  metas <- split(pcc_df, pcc_df$meta)
  res <- align_results(results, names(metas))
  rows <- lapply(seq_along(metas), function(j) {
    df <- metas[[j]]
    re1 <- re(df, se = df$se_s1)
    tau2 <- max(0, re1$tau2)
    by_uwls <- calculate_psb_measures(df, uwls(df)$est, tau2, alpha)
    by_re <- calculate_psb_measures(df, res$re1_est[j], tau2, alpha)
    egger_t <- res$fat[j] / res$fat_se[j]
    data.frame(
      meta = names(metas)[j],
      ESS = by_uwls$ess_prop,
      PSST = by_uwls$ess_prop / sqrt(by_uwls$esig * (1 - by_uwls$esig) / by_uwls$k),
      Egger_t = egger_t,
      Abs_Egger_t = abs(egger_t),
      ESS_RE = by_re$ess_prop,
      tau2_re1 = tau2,
      i2_re1 = re1$I2
    )
  })
  do.call(rbind, rows)
}

#' Table S8: estimator gaps and selection intensity
#'
#' Regresses each per-MA gap (RE1 - UWLS1, RE1 - HS, Mean - UWLS1) across the
#' meta-analyses on one measure of selection intensity at a time
#' ([selection_measures()]: ESS, PSST, the Egger t, the absolute Egger t, and
#' ESS with RE-based expected significance), controlling for heterogeneity
#' (tau2 and I2 of the RE1 fit, the heterogeneity behind ESS), the logs of the
#' numbers of estimates and studies, and the log of the median sample size.
#' OLS with HC3 standard errors; the coefficient and its standard error are
#' scaled to a one-standard-deviation increase in the selection measure. Two
#' further rows give the mean gap in the meta-analyses with no sign of
#' selection (ESS <= 0 and an absolute Egger t below 1.645) and in the
#' others. With these definitions every cell of the draft's Table S8 is
#' reproduced; the draft's note says "an Egger t below 1.645", and the signed
#' reading gives 53 such meta-analyses instead of 35.
#'
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()].
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param gaps [list] Gaps as pairs of Table 1 labels.
#' @return [data.frame] Columns `selection_measure`, `gap`, `estimate`
#'   (coefficient per SD, or the mean gap for the two subgroup rows),
#'   `se_hc3`, `t_value`, `p_value` and `n_metas`.
#' @export
calculate_gap_regressions <- function(results, pcc_df,
                                      gaps = list(c("RE1", "UWLS1"), c("RE1", "HS"),
                                                  c("Mean", "UWLS1"))) {
  sel <- selection_measures(pcc_df, results)
  res <- align_results(results, sel$meta)
  est <- results_estimates(res)
  controls <- cbind(
    tau2 = sel$tau2_re1,
    i2 = sel$i2_re1,
    log_estimates = log(res$k_),
    log_studies = log(res$n_studies),
    log_median_n = log(res$median_n)
  )
  measures <- c(
    "ESS" = "ESS",
    "PSST" = "PSST",
    "Egger t" = "Egger_t",
    "Absolute Egger t" = "Abs_Egger_t",
    "ESS with RE-based expected significance" = "ESS_RE"
  )

  rows <- list()
  for (m in names(measures)) {
    z <- sel[[measures[[m]]]]
    for (g in gaps) {
      gap <- est[[g[1]]] - est[[g[2]]]
      x <- cbind(`(Intercept)` = 1, measure = z, controls)
      ok <- stats::complete.cases(x) & !is.na(gap)
      fit <- ols_hc3(gap[ok], x[ok, , drop = FALSE])
      s <- stats::sd(z[ok])
      row <- fit[fit$term == "measure", ]
      rows[[length(rows) + 1]] <- data.frame(
        selection_measure = m, gap = contrast_label(g),
        estimate = row$estimate * s, se_hc3 = row$se_hc3 * s,
        t_value = row$t_value, p_value = row$p_value, n_metas = sum(ok)
      )
    }
  }
  no_sign <- sel$ESS <= 0 & sel$Abs_Egger_t < 1.645
  groups <- list(no_sign, !no_sign)
  names(groups) <- c(
    sprintf("Mean gap in %d MAs with no sign of selection", sum(no_sign, na.rm = TRUE)),
    sprintf("Mean gap in the other %d MAs", sum(!no_sign, na.rm = TRUE))
  )
  for (name in names(groups)) {
    for (g in gaps) {
      gap <- est[[g[1]]] - est[[g[2]]]
      in_group <- groups[[name]] & !is.na(groups[[name]])
      rows[[length(rows) + 1]] <- data.frame(
        selection_measure = name, gap = contrast_label(g),
        estimate = mean(gap[in_group], na.rm = TRUE), se_hc3 = NA_real_,
        t_value = NA_real_, p_value = NA_real_, n_metas = sum(in_group)
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Table S10: leave-one-meta-analysis-out ranges of the mean differences
#'
#' For each contrast, the mean difference across all meta-analyses and the
#' smallest and largest mean differences obtained by leaving out one
#' meta-analysis at a time.
#'
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()].
#' @param contrasts [list] Pairs of Table 1 labels.
#' @return [data.frame] One row per contrast: `contrast`, `full_sample`,
#'   `minimum`, `maximum` and the meta-analysis whose omission gives the
#'   minimum and the maximum (`dropped_at_min`, `dropped_at_max`).
#' @export
calculate_leave_one_out <- function(results, contrasts = supplement_contrasts()) {
  results <- results[results$meta != "All meta-analyses", , drop = FALSE]
  est <- results_estimates(results)
  rows <- lapply(contrasts, function(cc) {
    d <- est[[cc[1]]] - est[[cc[2]]]
    ok <- !is.na(d)
    loo <- rep(NA_real_, length(d))
    loo[ok] <- (sum(d[ok]) - d[ok]) / (sum(ok) - 1)
    data.frame(
      contrast = contrast_label(cc),
      full_sample = mean(d[ok]),
      minimum = min(loo, na.rm = TRUE),
      maximum = max(loo, na.rm = TRUE),
      dropped_at_min = results$meta[which.min(loo)],
      dropped_at_max = results$meta[which.max(loo)]
    )
  })
  do.call(rbind, rows)
}

#' Section 6: paired t-tests and correlations between estimators
#'
#' The paired t-tests of the per-MA differences between estimators and the
#' correlations quoted in Section 6 of the paper, across meta-analyses.
#'
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()].
#' @param pairs [list] Pairs of Table 1 labels for the paired t-tests (a - b).
#' @param correlations [list] Pairs of Table 1 labels to correlate (Pearson).
#' @return [data.frame] One row per test: `test` ("Paired t" or
#'   "Correlation"), `estimator_a`, `estimator_b`, `n_metas`, `mean_diff`,
#'   `t_value`, `df`, `p_value` (two-sided) and `correlation`.
#' @export
calculate_section6_tests <- function(results, pairs = section6_pairs(),
                                     correlations = list(c("HS", "UWLSz"), c("HS", "REz"))) {
  est <- results_estimates(results)
  t_rows <- lapply(pairs, function(cc) {
    tt <- paired_test(est[[cc[1]]], est[[cc[2]]])
    data.frame(
      test = "Paired t", estimator_a = cc[1], estimator_b = cc[2], n_metas = tt$n,
      mean_diff = tt$mean_diff, t_value = tt$t, df = tt$df, p_value = tt$p,
      correlation = NA_real_
    )
  })
  cor_rows <- lapply(correlations, function(cc) {
    ok <- !is.na(est[[cc[1]]]) & !is.na(est[[cc[2]]])
    data.frame(
      test = "Correlation", estimator_a = cc[1], estimator_b = cc[2], n_metas = sum(ok),
      mean_diff = NA_real_, t_value = NA_real_, df = NA_real_, p_value = NA_real_,
      correlation = stats::cor(est[[cc[1]]][ok], est[[cc[2]]][ok])
    )
  })
  do.call(rbind, c(t_rows, cor_rows))
}
