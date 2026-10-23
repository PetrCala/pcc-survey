# Online supplement: robustness tables S2 to S8 and S10, and the Section 6
# paired t-tests and correlations
#
# Table S1 (weight concentration) is calculate_weight_shares() in
# R/weight_concentration.R and Table S9 (the calibrated simulation) is in
# R/supplement_simulation.R. run_supplement_analysis() runs them all and writes
# one CSV per table to output/supplement/.
#
# Shared conventions, as in Table 1: the 172 meta-analyses after
# validate_pcc_observations() and the sign alignment of
# convert_inverse_relationships(); RE with the ML tau2; S1 / S2 standard
# errors; HS weighted by sample size; Fisher's z with variance 1 / (df - 1),
# leaving estimates with df <= 1 out of REz and UWLSz only; PET-PEESE as in
# fat_pet_peese(). Every meta-analysis counts equally: a table's "mean" is the
# mean across meta-analyses of the per-MA estimates. Estimators are named by
# their Table 1 labels (Mean, RE1, RE2, UWLS1, UWLS2, UWLS+3, HS, REz, UWLSz,
# PP).

#' Paired estimator contrasts used by Tables S4, S8 and S10
#'
#' Each contrast is `c(a, b)` for the per-MA difference a - b, with Table 1
#' labels. The first three are the Section 6 paired comparisons (REz - RE,
#' UWLS - UWLSz, UWLSz - HS).
#'
#' @return [list] Character pairs.
#' @export
supplement_contrasts <- function() {
  list(
    c("REz", "RE1"),
    c("UWLS1", "UWLSz"),
    c("UWLSz", "HS"),
    c("RE1", "UWLS1"),
    c("UWLS2", "UWLS1")
  )
}

contrast_label <- function(contrast) paste(contrast[1], "-", contrast[2])

# Per-MA results reordered to follow `metas` (the meta-analysis names of
# split(pcc_df, pcc_df$meta)).
align_results <- function(results, metas) {
  idx <- match(metas, results$meta)
  if (anyNA(idx)) cli::cli_abort("The per-MA results do not cover every meta-analysis")
  results[idx, , drop = FALSE]
}

# Mean across meta-analyses of an alternative estimate, and its mean change
# from the baseline, over the meta-analyses where both are available.
compare_to_baseline <- function(alt, base) {
  ok <- !is.na(alt) & !is.na(base)
  if (!any(ok)) {
    return(list(n_metas = 0L, mean = NA_real_, change = NA_real_))
  }
  list(n_metas = sum(ok), mean = mean(alt[ok]), change = mean(alt[ok] - base[ok]))
}

# Mean, median and SD across meta-analyses of one estimator variant, plus its
# mean and mean absolute difference from the paper's version (Tables S6, S7).
summarise_variant <- function(values, baseline) {
  ok <- !is.na(values)
  both <- ok & !is.na(baseline)
  data.frame(
    n_metas = sum(ok),
    mean = if (any(ok)) mean(values[ok]) else NA_real_,
    median = if (any(ok)) stats::median(values[ok]) else NA_real_,
    sd = if (sum(ok) > 1) stats::sd(values[ok]) else NA_real_,
    mean_diff = if (any(both)) mean(values[both] - baseline[both]) else NA_real_,
    mean_abs_diff = if (any(both)) mean(abs(values[both] - baseline[both])) else NA_real_
  )
}

# Fit one estimator on one meta-analysis and return its estimate and
# per-estimate weights (NA weights, e.g. REz rows with df <= 1, count as zero).
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
#' three primary studies (top 3) with the largest share of that estimator's
#' weight (a study's weight is the sum of its estimates' weights, as in Table
#' S1), drops them and re-estimates the same estimator on the remaining
#' estimates. Meta-analyses with no more studies than are dropped are left out
#' of that column. PET-PEESE has no weight share and is not included (see
#' R/weight_concentration.R).
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param re_method [character] tau2 estimator for RE1, RE2 and REz.
#' @return [data.frame] One row per estimator: `full_mean` (mean across all
#'   meta-analyses, as in Table 1), and for the top-1 and top-3 drops the number
#'   of meta-analyses re-estimated (`drop1_n_metas`, `drop3_n_metas`), the mean
#'   re-estimated value (`drop1_mean`, `drop3_mean`) and the mean change from
#'   the full-sample estimate over those meta-analyses (`drop1_change`,
#'   `drop3_change`).
#' @export
calculate_drop_top_studies <- function(pcc_df, re_method = "ML") {
  keys <- setdiff(names(supplement_estimators()), "petpeese")
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
    d1 <- compare_to_baseline(vals[2, ], vals[1, ])
    d3 <- compare_to_baseline(vals[3, ], vals[1, ])
    data.frame(
      estimator = unname(supplement_estimators()[keys[i]]),
      full_mean = mean(vals[1, ], na.rm = TRUE),
      drop1_n_metas = d1$n_metas,
      drop1_mean = d1$mean,
      drop1_change = d1$change,
      drop3_n_metas = d3$n_metas,
      drop3_mean = d3$mean,
      drop3_change = d3$change
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

#' Table S3: one estimate per study and study-level weighting
#'
#' Addresses the dependence of estimates within primary studies four ways, each
#' reported as the mean across meta-analyses next to the Table 1 baseline:
#'
#' * `study_median`: each study collapsed to its median estimate
#'   ([study_medians()]), then every estimator applied as in Table 1.
#' * `random_draw`: one estimate drawn at random from each study, `n_draws`
#'   times; a meta-analysis's value is its average over the draws.
#' * `weights_by_m`: all estimates kept, each estimate's weight divided by m,
#'   its study's number of estimates, so that every study carries roughly its
#'   typical estimate's weight (RE's tau2 is estimated as usual; the simple mean
#'   becomes the mean of the study means).
#' * `three_level`: RE1 and REz from a three-level model (estimates nested in
#'   studies, REML), on the S1 variance and on Fisher's z respectively.
#'
#' The random draws and the m-weighted estimates use the column-wise
#' estimators of [estimators_matrix()].
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param results [data.frame|NULL] Per-MA results of [pcc_survey_analyse()]
#'   for the baseline column; recomputed with [meta_estimates()] when NULL.
#' @param n_draws [integer] Number of random draws.
#' @param seed [integer] Seed for the random draws.
#' @param re_method [character] tau2 estimator for the study-median fits.
#' @param three_level [logical] Whether to fit the three-level models (slow on
#'   the largest meta-analyses).
#' @return [data.frame] One row per estimator with columns `baseline`,
#'   `study_median`, `random_draw`, `weights_by_m`, `three_level` (RE1 and REz
#'   only) and `three_level_n_metas` (meta-analyses where the three-level model
#'   converged).
#' @export
calculate_study_dependence <- function(pcc_df, results = NULL, n_draws = 200L,
                                       seed = 2026L, re_method = "ML",
                                       three_level = TRUE) {
  labels <- unname(supplement_estimators())
  metas <- split(pcc_df, pcc_df$meta)

  baseline <- if (is.null(results)) {
    est <- t(vapply(metas, meta_estimates, numeric(length(labels)), re_method = re_method))
    colnames(est) <- labels
    as.data.frame(est)
  } else {
    results_estimates(align_results(results, names(metas)))
  }

  medians <- study_medians(pcc_df)
  median_est <- t(vapply(split(medians, medians$meta), meta_estimates,
                         numeric(length(labels)), re_method = re_method))

  set.seed(seed)
  draw_est <- t(vapply(metas, random_draw_estimates, numeric(length(labels)),
                       n_draws = n_draws))

  by_m_est <- t(vapply(metas, function(df) {
    m <- as.numeric(table(df$study)[as.character(df$study)])
    estimators_matrix(
      effect = df$effect, se_s1 = df$se_s1, se_s2 = df$se_s2, pcc3 = df$pcc3,
      fishers_z = df$fishers_z, fishers_z_se = df$fishers_z_se,
      sample_size = df$sample_size, mult = 1 / m
    )[1, ]
  }, numeric(length(labels))))

  three_level_est <- matrix(NA_real_, nrow = length(metas), ncol = 2,
                            dimnames = list(NULL, c("RE1", "REz")))
  if (three_level) {
    three_level_est <- t(vapply(metas, function(df) {
      c(
        RE1 = three_level_estimate(df$effect, df$se_s1^2, df$study),
        REz = tanh(three_level_estimate(df$fishers_z, df$fishers_z_se^2, df$study))
      )
    }, numeric(2)))
  }

  col_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  data.frame(
    estimator = labels,
    baseline = vapply(labels, function(l) col_mean(baseline[[l]]), numeric(1)),
    study_median = apply(median_est, 2, col_mean),
    random_draw = apply(draw_est, 2, col_mean),
    weights_by_m = apply(by_m_est, 2, col_mean),
    three_level = vapply(labels, function(l) {
      if (l %in% colnames(three_level_est)) col_mean(three_level_est[, l]) else NA_real_
    }, numeric(1)),
    three_level_n_metas = vapply(labels, function(l) {
      if (l %in% colnames(three_level_est)) sum(!is.na(three_level_est[, l])) else NA_integer_
    }, integer(1)),
    row.names = NULL
  )
}

#' Table S4: percentile bootstrap of paired estimator contrasts
#'
#' For each contrast a - b, the statistic is the mean across meta-analyses of
#' the per-MA difference. Meta-analyses are resampled with replacement
#' `n_boot` times (the same resamples for every contrast) and the confidence
#' interval is the percentile interval of the resampled means.
#'
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()].
#' @param contrasts [list] Pairs of Table 1 labels (see [supplement_contrasts()]).
#' @param n_boot [integer] Number of bootstrap resamples.
#' @param seed [integer] Seed for the resampling.
#' @param conf [numeric] Confidence level.
#' @return [data.frame] One row per contrast: `contrast`, `n_metas`,
#'   `mean_diff`, `boot_se`, `ci_lower`, `ci_upper` and `share_positive` (share
#'   of resampled means above zero).
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
    data.frame(
      contrast = contrast_label(cc),
      n_metas = sum(!is.na(d)),
      mean_diff = mean(d, na.rm = TRUE),
      boot_se = stats::sd(boot, na.rm = TRUE),
      ci_lower = q[1],
      ci_upper = q[2],
      share_positive = mean(boot > 0, na.rm = TRUE)
    )
  })
  do.call(rbind, rows)
}

#' Table S5: estimator means in subsamples of meta-analyses
#'
#' Table 1's mean and median of every estimator, recomputed in subsamples of
#' meta-analyses: all, at least 10, 20 and 50 primary studies, at least 30
#' estimates, and sign-flipped and not flipped (see
#' [convert_inverse_relationships()]).
#'
#' @param results [data.frame] Per-MA results with `n_studies`, `k_` and
#'   `flipped`.
#' @return [data.frame] Columns `subsample`, `n_metas`, `statistic` ("Mean" or
#'   "Median") and one column per estimator.
#' @export
calculate_subsample_means <- function(results) {
  results <- results[results$meta != "All meta-analyses", , drop = FALSE]
  est <- results_estimates(results)
  flipped <- as.logical(results$flipped)
  subsamples <- list(
    "All" = rep(TRUE, nrow(results)),
    ">= 10 studies" = results$n_studies >= 10,
    ">= 20 studies" = results$n_studies >= 20,
    ">= 50 studies" = results$n_studies >= 50,
    ">= 30 estimates" = results$k_ >= 30,
    "Flipped" = flipped,
    "Not flipped" = !flipped
  )

  rows <- list()
  for (name in names(subsamples)) {
    sel <- subsamples[[name]] & !is.na(subsamples[[name]])
    for (stat in c("Mean", "Median")) {
      f <- if (stat == "Mean") mean else stats::median
      vals <- vapply(est, function(x) {
        x <- x[sel]
        x <- x[!is.na(x)]
        if (length(x) == 0) NA_real_ else f(x)
      }, numeric(1))
      rows[[length(rows) + 1]] <- data.frame(
        subsample = name, n_metas = sum(sel), statistic = stat,
        t(vals), check.names = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Table S6: other tau2 estimators and HS with df weights
#'
#' Re-estimates RE1, RE2 and REz with the REML and DerSimonian-Laird tau2
#' estimators, and HS with degrees-of-freedom weights instead of sample-size
#' weights. Each variant is summarised across meta-analyses next to the
#' paper's version (ML tau2; sample-size weights).
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()] (the
#'   paper's versions).
#' @param re_methods [character] Alternative tau2 estimators.
#' @return [data.frame] One row per estimator and variant: `estimator`,
#'   `variant`, `n_metas`, `mean`, `median`, `sd`, `mean_diff` and
#'   `mean_abs_diff` (per-MA difference from the paper's version).
#' @export
calculate_tau2_hs_variants <- function(pcc_df, results, re_methods = c("REML", "DL")) {
  metas <- split(pcc_df, pcc_df$meta)
  base <- results_estimates(align_results(results, names(metas)))
  re_keys <- c("re1", "re2", "fishers_z")
  labels <- supplement_estimators()

  rows <- list()
  for (key in re_keys) {
    label <- labels[[key]]
    rows[[length(rows) + 1]] <- cbind(
      estimator = label, variant = "ML (paper)",
      summarise_variant(base[[label]], base[[label]])
    )
  }
  for (method in re_methods) {
    est <- t(vapply(metas, meta_estimates, numeric(length(re_keys)),
                    estimators = re_keys, re_method = method))
    for (key in re_keys) {
      label <- labels[[key]]
      rows[[length(rows) + 1]] <- cbind(
        estimator = label, variant = method,
        summarise_variant(est[, key], base[[label]])
      )
    }
  }
  hs_dof <- vapply(metas, function(df) meta_estimates(df, "hsma", hs_weights = "dof")[["hsma"]],
                   numeric(1))
  rows[[length(rows) + 1]] <- cbind(estimator = "HS", variant = "Sample-size weights (paper)",
                                    summarise_variant(base$HS, base$HS))
  rows[[length(rows) + 1]] <- cbind(estimator = "HS", variant = "df weights",
                                    summarise_variant(hs_dof, base$HS))

  out <- do.call(rbind, rows)
  out <- out[order(match(out$estimator, unname(labels))), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Table S7: Fisher's z variance 1 / (df - 1) versus 1 / (n - 3)
#'
#' REz and UWLSz under the paper's partial-correlation variance 1 / (df - 1)
#' (estimates with df <= 1 left out) and under the bivariate-correlation
#' variance 1 / (n - 3) used in the first revision (every estimate kept, since
#' validation requires n > 3), summarised across meta-analyses.
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()] (the
#'   paper's versions).
#' @return [data.frame] One row per estimator and variance: `estimator`,
#'   `variance`, `n_estimates` (estimates used), `n_metas`, `mean`, `median`,
#'   `sd`, `mean_diff` and `mean_abs_diff` (per-MA difference from the paper's
#'   1 / (df - 1) version).
#' @export
calculate_fishers_z_variants <- function(pcc_df, results) {
  metas <- split(pcc_df, pcc_df$meta)
  base <- results_estimates(align_results(results, names(metas)))
  keys <- c(fishers_z = "REz", uwlsz = "UWLSz")
  n3 <- t(vapply(metas, meta_estimates, numeric(2), estimators = names(keys),
                 fishers_z_variance = "n_minus_3"))
  n_df1 <- sum(!is.na(pcc_df$fishers_z_se))
  n_n3 <- sum(pcc_df$sample_size > 3)

  rows <- list()
  for (key in names(keys)) {
    label <- keys[[key]]
    rows[[length(rows) + 1]] <- cbind(
      estimator = label, variance = "1 / (df - 1) (paper)", n_estimates = n_df1,
      summarise_variant(base[[label]], base[[label]])
    )
    rows[[length(rows) + 1]] <- cbind(
      estimator = label, variance = "1 / (n - 3)", n_estimates = n_n3,
      summarise_variant(n3[, key], base[[label]])
    )
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

#' Table S8: estimator gaps and publication-selection indicators
#'
#' Regresses each per-MA estimator gap (a contrast a - b) on one
#' publication-selection indicator at a time plus controls, by OLS with HC3
#' standard errors, across meta-analyses. The indicators are the excess
#' statistical significance `ESS` (from the UWLS mean, as in
#' psb_results.csv), the proportion-of-significance test `PSST` = (Pss - Esig)
#' / sqrt(Esig (1 - Esig) / k) (as in secondary-stats/IndividualPSST.dta) and
#' the Egger test statistic `Egger_t` = FAT / SE(FAT) from the PET fit (the
#' `tFAT` of secondary-stats/pcc_survey_results.dta).
#'
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()]
#'   (needs `fat`, `fat_se`, `k_`, `median_n`, `i2_uwls2`, `flipped`).
#' @param psb_results [data.frame] Per-MA PSB results from
#'   [get_psb_metaflavours()] (needs `meta`, `ess_uwls`, `esig_uwls`, `pss`,
#'   `k`).
#' @param contrasts [list] Gaps as pairs of Table 1 labels.
#' @param controls [character] Control variables: any of `log_k` (log number of
#'   estimates), `log_studies` (log number of primary studies), `log_median_n`
#'   (log median sample size), `i2` (UWLS2 I-squared as a fraction) and
#'   `flipped` (sign-flip dummy).
#' @return [data.frame] One row per gap, indicator and regressor: `gap`,
#'   `indicator`, `term`, `estimate`, `se_hc3`, `t_value`, `p_value`, `n` and
#'   `r_squared`.
#' @export
calculate_gap_regressions <- function(results, psb_results,
                                      contrasts = supplement_contrasts(),
                                      controls = c("log_k", "log_median_n", "i2", "flipped")) {
  results <- results[results$meta != "All meta-analyses", , drop = FALSE]
  psb <- psb_results[match(results$meta, psb_results$meta), , drop = FALSE]
  if (anyNA(psb$meta)) cli::cli_abort("psb_results does not cover every meta-analysis")

  indicators <- data.frame(
    ESS = psb$ess_uwls,
    PSST = (psb$pss - psb$esig_uwls) / sqrt(psb$esig_uwls * (1 - psb$esig_uwls) / psb$k),
    Egger_t = results$fat / results$fat_se
  )
  control_data <- data.frame(
    log_k = log(results$k_),
    log_studies = log(results$n_studies),
    log_median_n = log(results$median_n),
    i2 = results$i2_uwls2 / 100,
    flipped = as.numeric(as.logical(results$flipped))
  )
  stopifnot(all(controls %in% colnames(control_data)))
  est <- results_estimates(results)

  rows <- list()
  for (cc in contrasts) {
    gap <- est[[cc[1]]] - est[[cc[2]]]
    for (ind in colnames(indicators)) {
      x <- cbind(`(Intercept)` = 1, indicators[ind], control_data[controls])
      x <- as.matrix(x)
      ok <- stats::complete.cases(x) & !is.na(gap)
      fit <- ols_hc3(gap[ok], x[ok, , drop = FALSE])
      rows[[length(rows) + 1]] <- cbind(
        gap = contrast_label(cc), indicator = ind, fit,
        n = attr(fit, "n"), r_squared = attr(fit, "r_squared")
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Cross-MA statistics tracked by Table S10: the mean of every estimator, the
# mean and paired t of every contrast, and the Section 6 correlations.
leave_one_out_statistics <- function(est, contrasts, correlations) {
  means <- vapply(est, function(x) mean(x, na.rm = TRUE), numeric(1))
  names(means) <- paste("Mean", names(est))
  diffs <- unlist(lapply(contrasts, function(cc) {
    d <- est[[cc[1]]] - est[[cc[2]]]
    d <- d[!is.na(d)]
    stats::setNames(
      c(mean(d), mean(d) / (stats::sd(d) / sqrt(length(d)))),
      paste(c("Mean diff", "Paired t"), contrast_label(cc))
    )
  }))
  cors <- vapply(correlations, function(cc) {
    stats::cor(est[[cc[1]]], est[[cc[2]]], use = "complete.obs")
  }, numeric(1))
  names(cors) <- vapply(correlations, function(cc) paste0("corr(", cc[1], ", ", cc[2], ")"),
                        character(1))
  c(means, diffs, cors)
}

#' Table S10: leave-one-meta-analysis-out ranges
#'
#' Recomputes the headline cross-MA statistics with each meta-analysis left
#' out in turn and reports their range: the mean of every estimator (Table 1),
#' the mean difference and paired t of every contrast, and the Section 6
#' correlations.
#'
#' @param results [data.frame] Per-MA results of [pcc_survey_analyse()].
#' @param contrasts [list] Pairs of Table 1 labels.
#' @param correlations [list] Pairs of Table 1 labels to correlate.
#' @return [data.frame] One row per statistic: `statistic`, `full_sample`,
#'   `loo_min`, `loo_max` and the meta-analysis whose omission gives the
#'   minimum and maximum (`dropped_at_min`, `dropped_at_max`).
#' @export
calculate_leave_one_out <- function(results, contrasts = supplement_contrasts(),
                                    correlations = list(c("HS", "UWLSz"), c("HS", "REz"))) {
  results <- results[results$meta != "All meta-analyses", , drop = FALSE]
  est <- results_estimates(results)
  full <- leave_one_out_statistics(est, contrasts, correlations)
  loo <- vapply(seq_len(nrow(est)), function(i) {
    leave_one_out_statistics(est[-i, , drop = FALSE], contrasts, correlations)
  }, numeric(length(full)))
  data.frame(
    statistic = names(full),
    full_sample = unname(full),
    loo_min = apply(loo, 1, min),
    loo_max = apply(loo, 1, max),
    dropped_at_min = results$meta[apply(loo, 1, which.min)],
    dropped_at_max = results$meta[apply(loo, 1, which.max)],
    row.names = NULL
  )
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
calculate_section6_tests <- function(results,
                                     pairs = supplement_contrasts()[1:3],
                                     correlations = list(c("HS", "UWLSz"), c("HS", "REz"))) {
  est <- results_estimates(results)
  t_rows <- lapply(pairs, function(cc) {
    ok <- !is.na(est[[cc[1]]]) & !is.na(est[[cc[2]]])
    tt <- stats::t.test(est[[cc[1]]][ok], est[[cc[2]]][ok], paired = TRUE)
    data.frame(
      test = "Paired t", estimator_a = cc[1], estimator_b = cc[2], n_metas = sum(ok),
      mean_diff = unname(tt$estimate), t_value = unname(tt$statistic),
      df = unname(tt$parameter), p_value = tt$p.value, correlation = NA_real_
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
