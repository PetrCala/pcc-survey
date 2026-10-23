# Weight-concentration diagnostic (RSM revision, Reviewer 2 point 3)
#
# Reviewer 2 asked for "the percentage of total weight attributable to the most
# influential primary study and to the three most influential primary studies
# under the main weighting schemes". The decisions taken, so that the response
# letter can state them:
#
# * The paper reports the PRIMARY-STUDY (paper) level (co-authors, October 2026,
#   second-round revision; this replaces the estimate-level choice of
#   2 Sep 2026). A study's weight is the sum of the weights of all its
#   estimates within the meta-analysis, and the per-MA shares are the
#   "<method>_w_top1_study" / "<method>_w_top3_study" columns. Study identity is
#   the `study` column (the Excel "Title"; rows without a title get a synthetic
#   "Missing study N" label from fill_missing_values(), grouping by first
#   author / year runs, so grouping is approximate there).
# * The ESTIMATE level (the estimator's single most heavily weighted estimate,
#   and its top three) is still computed and kept in the per-MA results
#   ("<method>_w_top1_estimate" / "_w_top3_estimate") for reference; it feeds
#   no table. The data carry about 10 estimates per study, so the two levels
#   differ a lot.
# * Shares are fractions of the estimator's total weight within one
#   meta-analysis (0 to 1). The paper's Appendix Table 1 (the W_top1 / W_top3
#   rows of the estimator summary) reports their MEAN across the 172
#   meta-analyses. The online supplement's Table S1
#   (calculate_weight_shares()) adds the median and interquartile
#   range, plus an equal-study-weights benchmark: 1 / k for the top study and
#   min(3, k) / k for the top three, with k the number of studies in the MA.
# * "Top 3" is the top min(3, number of units); with fewer than 3 units it is 1
#   by construction.
# * REz and UWLSz leave out estimates with df <= 1 (their Fisher's z variance
#   1 / (df - 1) is undefined); those estimates carry no weight and are
#   ignored when the shares are formed.
# * PET-PEESE gets no share: its WLS weights (1 / se^2) equal UWLS1's, but its
#   intercept is not a weighted mean of the effects, so a "share of weight" is
#   not defined. It is left out of Table S1 and blank in Appendix Table 1, as
#   MSD-PP is for PP.

#' Share of total weight carried by the most heavily weighted units
#'
#' Returns the share of the total weight carried by the `top_n` units with the
#' largest weight. A unit is a single estimate when `study` is NULL, and a
#' primary study (paper) otherwise, in which case the weights of all estimates
#' of a study are summed first.
#'
#' @param weights [numeric] Per-estimate weights (any positive scale; normalised
#'   internally).
#' @param study [vector|NULL] Primary-study identifier for each estimate, same
#'   length as `weights`; NULL for the estimate level.
#' @param top_n [integer] Number of top units to sum over (default 1).
#' @return [numeric] Share in [0, 1] of the total weight carried by the `top_n`
#'   units with the largest weight. NA if all weights are NA.
#' @export
top_weight_share <- function(weights, study = NULL, top_n = 1L) {
  stopifnot(length(top_n) == 1, top_n >= 1)
  ok <- !is.na(weights) & is.finite(weights)
  if (!any(ok)) return(NA_real_)
  if (!is.null(study)) {
    stopifnot(length(weights) == length(study))
    study <- as.character(study)[ok]
  }
  weights <- weights[ok]
  stopifnot(all(weights >= 0))
  total <- sum(weights)
  if (total <= 0) return(NA_real_)

  by_unit <- if (is.null(study)) weights else as.numeric(tapply(weights, study, sum))
  by_unit <- sort(by_unit, decreasing = TRUE)
  n_take <- min(as.integer(top_n), length(by_unit))
  # The top units summed in sorted order and the total summed in data order
  # can differ in the last bit, so a share that covers every unit is set to
  # exactly 1 and the rest are kept within [0, 1].
  if (n_take == length(by_unit)) return(1)
  min(1, sum(by_unit[seq_len(n_take)]) / total)
}

#' Share of total weight carried by the most influential primary studies
#'
#' Paper-level convenience wrapper around [top_weight_share()] (the per-MA
#' "*_study" columns reported in the paper).
#'
#' @inheritParams top_weight_share
#' @param study [vector] Primary-study identifier for each estimate.
#' @return [numeric] Share in [0, 1].
#' @export
top_study_weight_share <- function(weights, study, top_n = 1L) {
  stopifnot(!is.null(study))
  top_weight_share(weights, study = study, top_n = top_n)
}

#' Top-1 and top-3 weight shares for one estimator, at both levels
#'
#' @param weights [numeric] Per-estimate weights.
#' @param study [vector] Primary-study identifier per estimate.
#' @return [list] `top1_study`, `top3_study` (primary-study level; these feed
#'   Appendix Table 1 and Table S1) and `top1_est`, `top3_est` (estimate level,
#'   kept for reference), all fractions in [0, 1].
#' @export
weight_concentration <- function(weights, study) {
  list(
    top1_est = top_weight_share(weights, study = NULL, top_n = 1L),
    top3_est = top_weight_share(weights, study = NULL, top_n = 3L),
    top1_study = top_weight_share(weights, study = study, top_n = 1L),
    top3_study = top_weight_share(weights, study = study, top_n = 3L)
  )
}

#' Weight-concentration table (online supplement Table S1)
#'
#' Summarises, for each estimator, the per-meta-analysis primary-study-level
#' shares of total weight carried by the highest-weight study (top 1) and by
#' the three highest-weight studies (top 3): their mean, median and 25th and
#' 75th percentiles (the interquartile range) across meta-analyses. The mean
#' columns equal the W_top1 / W_top3 rows of [calculate_estimator_summary()]
#' (the paper's Appendix Table 1). A last row, "Equal study weights", gives the
#' benchmark the shares would take if every study counted equally: 1 / k for
#' the top study and min(3, k) / k for the top three, with k the number of
#' studies in the meta-analysis. PET-PEESE has no weight share and is left out
#' (see the notes at the top of this file).
#'
#' @param results_df [data.frame] Results from pcc_survey_analyse(), with the
#'   `<method>_w_top1_study` / `<method>_w_top3_study` columns and, for the
#'   benchmark row, `n_studies`.
#' @return [data.frame] One row per estimator in Table 1 order, plus the
#'   benchmark row when `n_studies` is present. Columns: `estimator`,
#'   `top1_mean`, `top1_median`, `top1_q25`, `top1_q75`, `top3_mean`,
#'   `top3_median`, `top3_q25`, `top3_q75` (fractions in [0, 1]).
#' @export
calculate_weight_shares <- function(results_df) {
  individual_metas <- results_df[results_df$meta != "All meta-analyses", ]

  summarise_shares <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(rep(NA_real_, 4))
    q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
    c(mean(x), q[2], q[1], q[3])
  }

  # Point-estimate columns in Table 1 order; "petpeese" has no "_est" suffix
  # and so drops out here.
  estimator_names <- estimator_display_names()
  est_cols <- grep("_est$", names(estimator_names), value = TRUE)

  rows <- list()
  for (col in est_cols) {
    method <- sub("_est$", "", col)
    top1_col <- paste0(method, "_w_top1_study")
    top3_col <- paste0(method, "_w_top3_study")
    if (!all(c(top1_col, top3_col) %in% colnames(individual_metas))) next
    rows[[estimator_names[[col]]]] <- c(
      summarise_shares(individual_metas[[top1_col]]),
      summarise_shares(individual_metas[[top3_col]])
    )
  }
  if (length(rows) == 0) {
    cli::cli_abort("No primary-study weight-share columns ('<method>_w_top1_study') found")
  }

  if ("n_studies" %in% colnames(individual_metas)) {
    k <- individual_metas$n_studies
    rows[["Equal study weights"]] <- c(
      summarise_shares(1 / k),
      summarise_shares(pmin(3, k) / k)
    )
  }

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(out) <- c(
    "top1_mean", "top1_median", "top1_q25", "top1_q75",
    "top3_mean", "top3_median", "top3_q25", "top3_q75"
  )
  out <- cbind(estimator = names(rows), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}
