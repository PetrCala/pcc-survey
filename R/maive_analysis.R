# MAIVE (Meta-Analysis Instrumental Variable Estimator) Analysis Functions

#' Calculate MAIVE estimate
#'
#' Wrapper function to call the MAIVE package for meta-analysis using
#' instrumental variables to address publication bias.
#'
#' @param df [data.frame] The data frame with 'effect', 'se', and 'sample_size' columns
#' @param method [integer] MAIVE method: 1=PET, 2=PEESE, 3=PET-PEESE, 4=Endogenous Kink (default: 3)
#' @param weight [integer] Weighting option: 0=no weights, 1=IV weights, 2=MAIVE-adjusted weights, 3=study weights (default: 0)
#' @param instrument [integer] Whether to use instrument: 0=no, 1=yes (default: 1)
#' @param studylevel [integer] Study-level correlation: 0=none, 1=fixed effects, 2=clustering, 3=both (default: 0)
#' @param SE [integer] Standard error type: 0=CR0, 1=CR1, 2=CR2, 3=wild bootstrap (default: 0)
#' @param AR [integer] Anderson-Rubin CI: 0=no, 1=yes (default: 0)
#' @param first_stage [integer] First-stage specification: 0=levels, 1=log (default: 0)
#' @return [list] A list with properties "est", "t_value", and other MAIVE statistics
#' @export
calculate_maive <- function(df,
                            method = 3,
                            weight = 0,
                            instrument = 1,
                            studylevel = 0,
                            SE = 0,
                            AR = 0,
                            first_stage = 0) {
  stopifnot(all(c("effect", "se", "sample_size") %in% colnames(df)))
  stopifnot(nrow(df) > 0)

  maive_data <- data.frame(
    bs = df$effect,
    sebs = df$se,
    Ns = df$sample_size
  )

  maive_result <- MAIVE::maive(
    maive_data,
    method = method,
    weight = weight,
    instrument = instrument,
    studylevel = studylevel,
    SE = SE,
    AR = AR,
    first_stage = first_stage
  )

  stopifnot(!is.null(maive_result) && length(maive_result) > 0)

  # Extract estimate and t-value
  est <- NA_real_
  t_value <- NA_real_

  if ("coefficients" %in% names(maive_result)) {
    if (length(maive_result$coefficients) > 0) {
      est <- as.numeric(maive_result$coefficients[1])
      if ("se" %in% names(maive_result) && length(maive_result$se) > 0) {
        se_est <- as.numeric(maive_result$se[1])
        if (!is.na(se_est) && se_est > 0) {
          t_value <- est / se_est
        }
      }
    }
  } else if ("beta" %in% names(maive_result)) {
    est <- as.numeric(maive_result$beta[1])
    if ("se" %in% names(maive_result) && length(maive_result$se) > 0) {
      se_est <- as.numeric(maive_result$se[1])
      if (!is.na(se_est) && se_est > 0) {
        t_value <- est / se_est
      }
    }
  } else if (is.numeric(maive_result) && length(maive_result) > 0) {
    est <- as.numeric(maive_result[1])
  }

  list(est = est, t_value = t_value)
}
