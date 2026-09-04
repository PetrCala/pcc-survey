# Weight-concentration diagnostic (RSM revision, Reviewer 2 point 3)
#
# Reviewer 2 asked for "the percentage of total weight attributable to the most
# influential primary study and to the three most influential primary studies
# under the main weighting schemes". The decisions taken, so that the response
# letter can state them:
#
# * Table S1 reports the ESTIMATE level (Tom Stanley, 2 Sep 2026): the share of
#   the estimator's total weight carried by its single most heavily weighted
#   estimate (the smallest SE on that estimator's own scale) and by its three
#   most heavily weighted estimates. Reviewer 2's "primary study" is read in the
#   medical sense, where each estimate is a study.
# * The PAPER level is computed alongside and kept in the per-MA results
#   (columns "*_study"): a paper's weight is the sum of the weights of all its
#   estimates within the meta-analysis. Paper identity is the `study` column
#   (the Excel "Title"; rows without a title get a synthetic "Missing study N"
#   label from fill_missing_values(), grouping by first author / year runs, so
#   grouping is approximate there). The data carry about 9 estimates per paper,
#   so the two levels differ a lot; the paper level is there in case the
#   reviewer meant papers.
# * Shares are fractions of the estimator's total weight within one
#   meta-analysis (0 to 1); Table S1 reports their MEDIAN across meta-analyses
#   (Tom Stanley, 2 Sep 2026: the mean is distorted by a few extreme MAs).
# * "Top 3" is the top min(3, number of units); with fewer than 3 units it is 1
#   by construction.
# * PET-PEESE gets no share: its WLS weights (1 / se^2) equal UWLS1's, but its
#   intercept is not a weighted mean of the effects, so a "share of weight" is
#   not defined. It is left blank in Table S1, as MSE-PP is for PP.

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
  sum(by_unit[seq_len(n_take)]) / total
}

#' Share of total weight carried by the most influential primary studies
#'
#' Paper-level convenience wrapper around [top_weight_share()] (kept for the
#' per-MA "*_study" columns).
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
#' @return [list] `top1_est`, `top3_est` (estimate level; these feed Table S1)
#'   and `top1_study`, `top3_study` (paper level), all fractions in [0, 1].
#' @export
weight_concentration <- function(weights, study) {
  list(
    top1_est = top_weight_share(weights, study = NULL, top_n = 1L),
    top3_est = top_weight_share(weights, study = NULL, top_n = 3L),
    top1_study = top_weight_share(weights, study = study, top_n = 1L),
    top3_study = top_weight_share(weights, study = study, top_n = 3L)
  )
}
