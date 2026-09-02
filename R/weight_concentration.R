# Weight-concentration diagnostic (RSM revision, Reviewer 2 point 3)
#
# Reviewer 2 asked for "the percentage of total weight attributable to the most
# influential primary study and to the three most influential primary studies
# under the main weighting schemes". The decisions taken here, so that the
# response letter can state them:
#
# * Primary-study level, not estimate level. A study's weight is the sum of the
#   weights of all its estimates within the meta-analysis (the data carry about
#   9 estimates per study, so the two levels differ a lot). Study identity is the
#   `study` column (the Excel "Title"; rows without a title get a synthetic
#   "Missing study N" label from fill_missing_values(), which groups by first
#   author / year runs, so grouping is approximate there).
# * Shares are fractions of the estimator's total weight within one
#   meta-analysis (0 to 1); Table S1 reports their mean across meta-analyses.
# * "Top 3" is the top min(3, number of studies); with fewer than 3 studies it
#   is 1 by construction.
# * PET-PEESE gets no share: its WLS weights (1 / se^2) equal UWLS1's, but its
#   intercept is not a weighted mean of the effects, so a "share of weight" is
#   not defined. It is left blank in Table S1, as MSE-PP is for PP.

#' Share of total weight carried by the most influential primary studies
#'
#' Aggregates per-estimate weights by primary study (summing the weights of all
#' estimates of a study) and returns the share of the total weight carried by
#' the `top_n` most heavily weighted studies.
#'
#' @param weights [numeric] Per-estimate weights (any positive scale; normalised
#'   internally).
#' @param study [vector] Primary-study identifier for each estimate, same length
#'   as `weights`.
#' @param top_n [integer] Number of top studies to sum over (default 1).
#' @return [numeric] Share in [0, 1] of the total weight carried by the `top_n`
#'   studies with the largest aggregate weight. NA if all weights are NA.
#' @export
top_study_weight_share <- function(weights, study, top_n = 1L) {
  stopifnot(length(weights) == length(study))
  stopifnot(length(top_n) == 1, top_n >= 1)
  ok <- !is.na(weights) & is.finite(weights)
  if (!any(ok)) return(NA_real_)
  weights <- weights[ok]
  study <- as.character(study)[ok]
  stopifnot(all(weights >= 0))
  total <- sum(weights)
  if (total <= 0) return(NA_real_)

  by_study <- sort(tapply(weights, study, sum), decreasing = TRUE)
  n_take <- min(as.integer(top_n), length(by_study))
  sum(by_study[seq_len(n_take)]) / total
}

#' Top-1 and top-3 study weight shares for one estimator
#'
#' Convenience wrapper around [top_study_weight_share()] returning both shares
#' requested by Reviewer 2.
#'
#' @param weights [numeric] Per-estimate weights.
#' @param study [vector] Primary-study identifier per estimate.
#' @return [list] `top1` and `top3` shares (fractions in [0, 1]).
#' @export
weight_concentration <- function(weights, study) {
  list(
    top1 = top_study_weight_share(weights, study, top_n = 1L),
    top3 = top_study_weight_share(weights, study, top_n = 3L)
  )
}
