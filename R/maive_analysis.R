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
  meta <- unique(df$meta)

  # Validate required columns
  required_cols <- c("effect", "se")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    logger::log_warn(paste("Missing required columns for MAIVE:", paste(missing_cols, collapse = ", ")))
    return(list(est = NA, t_value = NA))
  }

  # Get sample sizes (use DOF as fallback if sample_size is missing)
  n_ <- get_dof_or_sample_size(df, target = "sample_size", use_dof_directly = TRUE)

  # Drop rows where sample size is missing or invalid
  valid_rows <- !is.na(n_) & n_ > 0 & !is.na(df$effect) & !is.na(df$se) & is.finite(df$effect) & is.finite(df$se)
  if (sum(valid_rows) == 0) {
    logger::log_warn(paste("No valid data to calculate MAIVE for meta-analysis", meta))
    return(list(est = NA, t_value = NA))
  }

  # Prepare data for MAIVE
  maive_data <- data.frame(
    bs = df$effect[valid_rows],
    sebs = df$se[valid_rows],
    Ns = n_[valid_rows]
  )

  result <- tryCatch(
    {
      # Call MAIVE function
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

      # Extract results
      # MAIVE returns a list with coefficients and other statistics
      # The structure may vary, so we need to extract appropriately
      if (is.null(maive_result) || length(maive_result) == 0) {
        logger::log_warn(paste("MAIVE returned empty result for meta-analysis", meta))
        return(list(est = NA, t_value = NA))
      }

      # Extract estimate and t-value
      # The exact structure depends on MAIVE package version
      # Common structure: maive_result$coefficients or maive_result$beta
      est <- NA_real_
      t_value <- NA_real_

      if ("coefficients" %in% names(maive_result)) {
        if (length(maive_result$coefficients) > 0) {
          est <- as.numeric(maive_result$coefficients[1])
          # Try to get standard error and calculate t-value
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

      list(est = est, t_value = t_value, maive_result = maive_result)
    },
    error = function(e) {
      logger::log_warn(paste("Could not fit the MAIVE model for meta-analysis", meta, ":", conditionMessage(e)))
      list(est = NA, t_value = NA)
    }
  )

  # Return only est and t_value (remove full maive_result for cleaner output)
  list(est = result$est, t_value = result$t_value)
}
