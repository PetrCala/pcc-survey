# Estimator helpers for the online supplement (Tables S2 to S10)
#
# Two kinds of helpers live here:
#
# * meta_estimates() runs the package's own estimator functions (R/pcc.R, so
#   metafor for RE) on one meta-analysis, with the supplement's variations:
#   another tau2 estimator (Table S6), HS weighted by df instead of sample size
#   (Table S6) and Fisher's z with variance 1 / (n - 3) instead of 1 / (df - 1)
#   (Table S7).
# * estimators_matrix() and the *_matrix() functions compute the ten Table 1
#   estimators for many samples at once, one sample per matrix column. The
#   supplement re-estimates every meta-analysis thousands of times (200
#   one-estimate-per-study draws in Table S3, 2,000 simulated replications under
#   three selection conditions in Table S9), which would take hours with one
#   metafor fit per sample. They follow R/pcc.R: RE with the ML tau2 found by
#   Fisher scoring (as metafor's rma(method = "ML")), UWLS as the 1 / se^2
#   weighted mean, HS weighted by sample size, REz / UWLSz on Fisher's z
#   leaving out rows whose variance is NA (df <= 1), and PET-PEESE as in
#   fat_pet_peese(). tests/testthat/test_supplement_estimators.R checks them
#   against the metafor-based functions.

# Estimator keys (the per-MA results columns without "_est") and their Table 1
# labels, in Table 1 order.
supplement_estimators <- function() {
  c(
    simple_mean = "Mean",
    re1 = "RE1",
    re2 = "RE2",
    uwls1 = "UWLS1",
    uwls2 = "UWLS2",
    uwls3 = "UWLS+3",
    hsma = "HS",
    fishers_z = "REz",
    uwlsz = "UWLSz",
    petpeese = "PP"
  )
}

#' Per-MA estimates by Table 1 label
#'
#' Pulls the ten Table 1 point estimates out of the per-MA results of
#' [pcc_survey_analyse()] and names them by their Table 1 labels (Mean, RE1,
#' ..., UWLSz, PP).
#'
#' @param results [data.frame] Per-MA results with the `<key>_est` columns and
#'   `petpeese`.
#' @return [data.frame] One column per estimator present, labelled as in Table 1.
#' @keywords internal
results_estimates <- function(results) {
  results <- results[results$meta != "All meta-analyses", , drop = FALSE]
  keys <- supplement_estimators()
  cols <- ifelse(names(keys) == "petpeese", "petpeese", paste0(names(keys), "_est"))
  present <- cols %in% colnames(results)
  out <- as.data.frame(lapply(cols[present], function(col) results[[col]]))
  colnames(out) <- unname(keys[present])
  out
}

#' All Table 1 estimators for one meta-analysis, with supplement variations
#'
#' Runs the estimator functions of R/pcc.R on one meta-analysis and returns
#' their point estimates. With the defaults the values equal the per-MA results
#' behind Table 1. When metafor's Fisher scoring does not converge for RE1, RE2
#' or REz, the fit is retried with its documented remedy, a halved step
#' (`control = list(stepadj = 0.5, maxiter = 1000)`). A fit that still fails
#' (or has too few estimates left) gives NA for that estimator.
#'
#' @param df [data.frame] One meta-analysis after [compute_derived_quantities()].
#' @param estimators [character] Estimator keys to compute (see
#'   `supplement_estimators()`); defaults to all ten.
#' @param re_method [character] tau2 estimator for RE1, RE2 and REz ("ML",
#'   "REML", "DL", ...; passed to metafor).
#' @param hs_weights [character] "sample_size" (the paper's HS) or "dof" (HS
#'   weighted by degrees of freedom, Table S6).
#' @param fishers_z_variance [character] "df_minus_1" (the paper's 1 / (df - 1),
#'   estimates with df <= 1 left out) or "n_minus_3" (1 / (n - 3), the first
#'   revision's variance, Table S7).
#' @return [numeric] Named by estimator key.
#' @export
meta_estimates <- function(df, estimators = names(supplement_estimators()),
                           re_method = "ML",
                           hs_weights = c("sample_size", "dof"),
                           fishers_z_variance = c("df_minus_1", "n_minus_3")) {
  hs_weights <- match.arg(hs_weights)
  fishers_z_variance <- match.arg(fishers_z_variance)
  stopifnot(all(estimators %in% names(supplement_estimators())))

  if (fishers_z_variance == "n_minus_3") {
    df$fishers_z_se <- ifelse(df$sample_size > 3, 1 / sqrt(df$sample_size - 3), NA_real_)
  }
  hs_df <- df
  if (hs_weights == "dof") hs_df$sample_size <- df$dof

  # Default metafor settings first, then a halved Fisher scoring step
  with_retry <- function(fit) {
    tryCatch(fit(list()), error = function(e) fit(list(stepadj = 0.5, maxiter = 1000)))
  }
  fits <- list(
    simple_mean = function() simple_mean(df)$est,
    re1 = function() with_retry(function(ctl) re(df, se = df$se_s1, method = re_method, control = ctl)$est),
    re2 = function() with_retry(function(ctl) re(df, se = df$se_s2, method = re_method, control = ctl)$est),
    uwls1 = function() uwls(df, se = df$se_s1)$est,
    uwls2 = function() uwls(df, se = df$se_s2)$est,
    uwls3 = function() uwls3(df)$est,
    hsma = function() hsma(hs_df)$est,
    fishers_z = function() with_retry(function(ctl) fishers_z(df, method = re_method, control = ctl)$est),
    uwlsz = function() uwls_fishers_z(df)$est,
    petpeese = function() fat_pet_peese(df, se = df$se_s1, alpha = 0.1)$est
  )

  vapply(estimators, function(key) {
    tryCatch(as.numeric(fits[[key]]()), error = function(e) NA_real_)
  }, numeric(1))
}

# Coerce x (a vector of length k or a k x b matrix) to a k x b matrix.
as_column_matrix <- function(x, k, b) {
  if (is.matrix(x)) {
    stopifnot(nrow(x) == k, ncol(x) == b)
    return(x)
  }
  stopifnot(length(x) == k)
  matrix(x, nrow = k, ncol = b)
}

#' Random-effects estimates for many samples at once
#'
#' Fits the random-effects model to each column of `y`, with tau2 estimated by
#' maximum likelihood as in metafor's `rma(method = "ML")`: Fisher scoring
#' started from the DerSimonian-Laird value and truncated at zero, with
#' step halving, and a direct one-dimensional maximisation of the likelihood
#' for any column not converged after `max_iter` steps. Rows with an NA effect
#' or variance are left out of their column.
#'
#' @param y [matrix|numeric] Effects, one sample per column.
#' @param v [matrix|numeric] Sampling variances, same shape as `y` (a vector of
#'   length `nrow(y)` is recycled across columns).
#' @param mult [matrix|numeric|NULL] Optional multiplier for each estimate's
#'   weight in the pooled estimate (Table S3 divides by the study's number of
#'   estimates). tau2 is estimated without it.
#' @param tol [numeric] Convergence tolerance on the change in tau2.
#' @param max_iter [integer] Fisher scoring steps before the direct
#'   maximisation takes over.
#' @return [list] `est` (pooled estimate per column) and `tau2`.
#' @keywords internal
re_matrix <- function(y, v, mult = NULL, tol = 1e-10, max_iter = 100L) {
  y <- as.matrix(y)
  k <- nrow(y)
  b <- ncol(y)
  v <- as_column_matrix(v, k, b)
  keep <- !is.na(y) & !is.na(v)
  complete <- all(keep)
  if (!complete) {
    y[!keep] <- 0
    v[!keep] <- 1
  }
  ind <- keep * 1
  n_obs <- colSums(ind)

  # For the columns `idx` at tau2 `t2`: the ML log-likelihood (up to a
  # constant) and the sums behind the Fisher scoring step
  # (sum w^2 e^2 - sum w) / sum w^2.
  fit_at <- function(idx, t2) {
    cols <- if (length(idx) == b) TRUE else idx
    vt <- v[, cols, drop = FALSE] + rep(t2, each = k)
    w <- 1 / vt
    log_vt <- log(vt)
    if (!complete) {
      w <- w * ind[, cols, drop = FALSE]
      log_vt <- log_vt * ind[, cols, drop = FALSE]
    }
    yy <- y[, cols, drop = FALSE]
    sw <- colSums(w)
    e2 <- (yy - rep(colSums(w * yy) / sw, each = k))^2
    w2 <- w * w
    list(
      ll = -0.5 * colSums(log_vt) - 0.5 * colSums(w * e2),
      step = (colSums(w2 * e2) - sw) / colSums(w2)
    )
  }

  # DerSimonian-Laird starting value
  w <- ind / v
  sw <- colSums(w)
  mu <- colSums(w * y) / sw
  q <- colSums(w * (y - rep(mu, each = k))^2)
  c_dl <- sw - colSums(w^2) / sw
  tau2 <- pmax(0, (q - (n_obs - 1)) / c_dl)
  tau2[n_obs <= 1 | !is.finite(tau2)] <- 0

  # Fisher scoring for the ML tau2, each step halved while it lowers the
  # likelihood (plain scoring can oscillate when the sampling variances differ
  # by orders of magnitude). `cur` holds the fit at the current tau2 of the
  # active columns.
  active <- which(n_obs > 1)
  cur <- if (length(active) > 0) fit_at(active, tau2[active]) else NULL
  iter <- 0L
  while (length(active) > 0 && iter < max_iter) {
    iter <- iter + 1L
    step <- cur$step
    new_tau2 <- pmax(0, tau2[active] + step)
    nxt <- fit_at(active, new_tau2)
    worse <- which(nxt$ll < cur$ll)
    halvings <- 0L
    while (length(worse) > 0 && halvings < 50L) {
      halvings <- halvings + 1L
      step[worse] <- step[worse] / 2
      new_tau2[worse] <- pmax(0, tau2[active[worse]] + step[worse])
      retry <- fit_at(active[worse], new_tau2[worse])
      nxt$ll[worse] <- retry$ll
      nxt$step[worse] <- retry$step
      worse <- worse[retry$ll < cur$ll[worse]]
    }
    done <- abs(new_tau2 - tau2[active]) < tol
    tau2[active] <- new_tau2
    active <- active[!done]
    cur <- list(ll = nxt$ll[!done], step = nxt$step[!done])
  }
  # Scoring crawls, or stalls on a flat shoulder of the likelihood, when the
  # expected information far exceeds the likelihood's curvature. As metafor
  # does, a positive tau2 that is less likely than tau2 = 0 is not accepted.
  # Those columns and any not converged are maximised directly: the ML tau2 is
  # a weighted average of (y_i - mu)^2 - v_i, so it lies in [0, range(y)^2].
  fit_cols <- which(n_obs > 1 & tau2 > 0)
  if (length(fit_cols) > 0) {
    shoulder <- fit_cols[fit_at(fit_cols, rep(0, length(fit_cols)))$ll > fit_at(fit_cols, tau2[fit_cols])$ll]
    active <- union(active, shoulder)
  }
  for (j in active) {
    kk <- keep[, j]
    yj <- y[kk, j]
    vj <- v[kk, j]
    ll_j <- function(t2) {
      w <- 1 / (vj + t2)
      -0.5 * sum(log(vj + t2)) - 0.5 * sum(w * (yj - sum(w * yj) / sum(w))^2)
    }
    opt <- stats::optimize(ll_j, c(0, max(diff(range(yj))^2, 1e-12)), maximum = TRUE, tol = 1e-14)
    candidates <- c(tau2[j], 0, opt$maximum)
    tau2[j] <- candidates[which.max(vapply(candidates, ll_j, numeric(1)))]
  }

  w <- ind / (v + rep(tau2, each = k))
  if (!is.null(mult)) w <- w * as_column_matrix(mult, k, b)
  est <- colSums(w * y) / colSums(w)
  est[n_obs == 0] <- NA_real_
  list(est = est, tau2 = tau2)
}

#' UWLS estimates for many samples at once
#'
#' The UWLS point estimate is the 1 / se^2 weighted mean (see [uwls()]),
#' computed for each column. Rows with an NA effect or SE are left out.
#'
#' @param y [matrix|numeric] Effects, one sample per column.
#' @param se [matrix|numeric] Standard errors, same shape as `y`.
#' @param mult [matrix|numeric|NULL] Optional weight multiplier (see [re_matrix()]).
#' @return [numeric] One estimate per column.
#' @keywords internal
uwls_matrix <- function(y, se, mult = NULL) {
  y <- as.matrix(y)
  k <- nrow(y)
  b <- ncol(y)
  se <- as_column_matrix(se, k, b)
  keep <- !is.na(y) & !is.na(se)
  y[!keep] <- 0
  w <- ifelse(keep, 1 / se^2, 0)
  if (!is.null(mult)) w <- w * as_column_matrix(mult, k, b)
  est <- colSums(w * y) / colSums(w)
  est[colSums(keep) == 0] <- NA_real_
  est
}

# Weighted least squares of y on x (with intercept) for each column. Returns
# the intercept, its standard error and the residual df.
wls_intercept_matrix <- function(y, x, w) {
  k <- nrow(y)
  sw <- colSums(w)
  x_bar <- colSums(w * x) / sw
  y_bar <- colSums(w * y) / sw
  xc <- x - rep(x_bar, each = k)
  yc <- y - rep(y_bar, each = k)
  sxx <- colSums(w * xc^2)
  slope <- colSums(w * xc * yc) / sxx
  intercept <- y_bar - slope * x_bar
  resid <- yc - xc * rep(slope, each = k)
  df_resid <- colSums(w > 0) - 2
  sigma2 <- colSums(w * resid^2) / df_resid
  se_intercept <- sqrt(sigma2 * (1 / sw + x_bar^2 / sxx))
  list(est = intercept, se = se_intercept, df = df_resid)
}

#' Conditional PET-PEESE for many samples at once
#'
#' Column-wise version of [fat_pet_peese()]: WLS of the effect on its SE (PET)
#' with weights 1 / se^2; if the PET intercept is significantly positive at the
#' one-sided `alpha`, the PEESE intercept (WLS on se^2) is reported instead.
#' NA for columns with fewer than three estimates.
#'
#' @param y [matrix|numeric] Effects, one sample per column.
#' @param se [matrix|numeric] Standard errors, same shape as `y`.
#' @param mult [matrix|numeric|NULL] Optional weight multiplier (see [re_matrix()]).
#' @param alpha [numeric] One-sided significance level for the PET decision.
#' @return [numeric] One estimate per column.
#' @keywords internal
pet_peese_matrix <- function(y, se, mult = NULL, alpha = 0.1) {
  y <- as.matrix(y)
  k <- nrow(y)
  b <- ncol(y)
  se <- as_column_matrix(se, k, b)
  w <- 1 / se^2
  if (!is.null(mult)) w <- w * as_column_matrix(mult, k, b)

  pet <- wls_intercept_matrix(y, se, w)
  peese <- wls_intercept_matrix(y, se^2, w)
  p_one_sided <- stats::pt(pet$est / pet$se, df = pet$df, lower.tail = FALSE)
  use_peese <- !is.na(p_one_sided) & p_one_sided < alpha
  est <- ifelse(use_peese, peese$est, pet$est)
  est[pet$df < 1] <- NA_real_
  est
}

#' The ten Table 1 estimators for many samples at once
#'
#' Each column of the inputs is one sample (one meta-analysis draw or one
#' simulated meta-analysis); vectors of length `nrow(effect)` are recycled
#' across columns. The inputs are the columns that [compute_derived_quantities()]
#' adds, so for one sample with `mult = NULL` the result matches the per-MA
#' results of [get_pcc_survey_metaflavours()] up to the ML tau2 tolerance.
#'
#' @param effect,se_s1,se_s2,pcc3,fishers_z,fishers_z_se,sample_size Per-estimate
#'   PCC, S1 and S2 standard errors, PCC3 (UWLS+3), Fisher's z and its SE (NA
#'   where df <= 1) and sample size.
#' @param mult [matrix|numeric|NULL] Optional multiplier for each estimate's
#'   weight in every estimator (1 / m in Table S3, m being the number of
#'   estimates of the estimate's study; the simple mean then averages the study
#'   means). RE's tau2 is estimated without it.
#' @return [matrix] One row per sample, one column per estimator, labelled as in
#'   Table 1 (Mean, RE1, ..., UWLSz, PP).
#' @keywords internal
estimators_matrix <- function(effect, se_s1, se_s2, pcc3, fishers_z, fishers_z_se,
                              sample_size, mult = NULL) {
  effect <- as.matrix(effect)
  k <- nrow(effect)
  b <- ncol(effect)
  se_s1 <- as_column_matrix(se_s1, k, b)
  se_s2 <- as_column_matrix(se_s2, k, b)
  m <- if (is.null(mult)) matrix(1, k, b) else as_column_matrix(mult, k, b)
  n <- as_column_matrix(sample_size, k, b) * m

  re1 <- re_matrix(effect, se_s1^2, mult = mult)
  re2 <- re_matrix(effect, se_s2^2, mult = mult)
  rez <- re_matrix(fishers_z, as_column_matrix(fishers_z_se, k, b)^2, mult = mult)

  out <- cbind(
    Mean = colSums(m * effect) / colSums(m),
    RE1 = re1$est,
    RE2 = re2$est,
    UWLS1 = uwls_matrix(effect, se_s1, mult = mult),
    UWLS2 = uwls_matrix(effect, se_s2, mult = mult),
    `UWLS+3` = uwls_matrix(pcc3, se_s1, mult = mult),
    HS = colSums(n * effect) / colSums(n),
    REz = tanh(rez$est),
    UWLSz = tanh(uwls_matrix(fishers_z, fishers_z_se, mult = mult)),
    PP = pet_peese_matrix(effect, se_s1, mult = mult)
  )
  rownames(out) <- NULL
  out
}
