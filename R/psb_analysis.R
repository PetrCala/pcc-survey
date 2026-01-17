# Publication Selection Bias (PSB) Analysis Functions

#' Calculate RE1 model and extract tau^2 (heterogeneity variance)
#'
#' RE1 uses S1 standard errors and a random effects model to estimate tau^2.
#' This tau^2 is used in the E_sig calculations to account for heterogeneity.
#'
#' @param df [data.frame] The data frame with 'effect' column
#' @param re_method [character] Random effects method (default: "ML")
#' @return [numeric] tau^2 (heterogeneity variance), or NA if calculation fails
#' @export
get_re1_tau2 <- function(df, re_method = "ML") {
  meta <- unique(df$meta)
  if (length(meta) != 1) {
    logger::log_warn("Could not calculate RE1 tau^2 for multiple meta-analyses")
    return(NA_real_)
  }

  result <- tryCatch(
    {
      # Calculate S1 standard errors (as used by RE1)
      se_s1 <- pcc_se_s1(df)

      # Filter out NA values
      valid_rows <- !is.na(se_s1) & !is.na(df$effect)
      if (sum(valid_rows) == 0) {
        logger::log_warn(paste("No valid data to calculate RE1 tau^2 for meta-analysis", meta))
        return(NA_real_)
      }

      re_data_ <- data.frame(
        yi = df$effect[valid_rows],
        sei = se_s1[valid_rows],
        study = df$study[valid_rows]
      )

      suppressWarnings(
        re_ <- metafor::rma(
          yi = re_data_$yi,
          sei = re_data_$sei,
          method = re_method
        )
      )

      # Extract tau^2 from the rma result
      tau2 <- re_$tau2
      if (is.null(tau2) || is.na(tau2)) {
        return(NA_real_)
      }

      # Ensure tau^2 is non-negative
      if (tau2 < 0) {
        logger::log_warn(paste("Negative tau^2 from RE1 for meta-analysis", meta, ", setting to 0"))
        return(0.0)
      }

      return(tau2)
    },
    error = function(e) {
      logger::log_warn(paste("Could not calculate RE1 tau^2 for meta-analysis", meta, ":", conditionMessage(e)))
      return(NA_real_)
    }
  )

  result
}

#' Calculate E_sigi for each study using one-sided test with heterogeneity
#'
#' Calculates the expected probability of statistical significance for each study
#' using the formula: Z_i = (1.96 * SE_i - |UWLS|) / sqrt(SE_i^2 + tau^2)
#' and E_sigi = 1 - N(Z_i), where N is the cumulative normal distribution.
#'
#' This is a one-sided calculation that includes random heterogeneity from RE1's
#' estimate of tau^2.
#'
#' @param df [data.frame] The data frame with 'se' column
#' @param uwls_estimate [numeric] The UWLS estimate (mean effect)
#' @param tau2 [numeric] Heterogeneity variance (tau^2) from RE1
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [vector] Vector of E_sigi values for each study
#' @export
calculate_esigi <- function(df, uwls_estimate, tau2, se = NULL, alpha = 0.05) {
  if (is.null(se)) se <- df$se
  stopifnot(length(se) == nrow(df))
  stopifnot(is.numeric(uwls_estimate), length(uwls_estimate) == 1)
  stopifnot(is.numeric(tau2), length(tau2) == 1, tau2 >= 0)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)

  # Critical value for one-sided test (alpha = 0.05 -> 1.96)
  critical_value <- stats::qnorm(1 - alpha)

  # Calculate Z_i for each study
  # Z_i = (1.96 * SE_i - |UWLS|) / sqrt(SE_i^2 + tau2)
  numerator <- critical_value * se - abs(uwls_estimate)
  denominator <- sqrt(se^2 + tau2)

  # Handle division by zero or negative denominator
  valid <- !is.na(se) & !is.na(tau2) & denominator > 0
  z_i <- rep(NA_real_, length(se))
  z_i[valid] <- numerator[valid] / denominator[valid]

  # Calculate E_sigi = 1 - N(Z_i)
  # where N is the cumulative normal distribution function
  esigi <- 1 - stats::pnorm(z_i)

  # Handle edge cases
  esigi[is.na(esigi)] <- NA_real_
  esigi[is.infinite(esigi)] <- NA_real_

  esigi
}

#' Calculate statistical power for each study
#'
#' Calculates the statistical power for each study given a mean effect estimate.
#' Power is the probability of detecting a significant result (|t| > critical_value)
#' when the true effect equals the mean effect estimate.
#'
#' @param df [data.frame] The data frame with 'effect' and 'se' columns
#' @param mean_effect [numeric] The assumed true mean effect size
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05 for two-tailed test)
#' @return [vector] Vector of power values for each study
#' @export
calculate_study_power <- function(df, mean_effect, se = NULL, alpha = 0.05) {
  if (is.null(se)) se <- df$se
  stopifnot(length(se) == nrow(df))
  stopifnot(is.numeric(mean_effect), length(mean_effect) == 1)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)

  # Critical value for two-tailed test
  critical_value <- stats::qnorm(1 - alpha / 2)

  # Non-centrality parameter: lambda = mean_effect / se_i
  lambda <- mean_effect / se

  # Power = P(|Z| > critical_value | Z ~ N(lambda, 1))
  # For two-tailed test: 1 - P(-critical_value < Z < critical_value)
  power <- 1 - stats::pnorm(critical_value, mean = lambda, sd = 1) +
    stats::pnorm(-critical_value, mean = lambda, sd = 1)

  # Handle edge cases
  power[is.na(power)] <- NA_real_
  power[is.infinite(power)] <- NA_real_

  power
}

#' Calculate expected number of statistically significant studies
#'
#' Uses one-sided calculation with heterogeneity variance (tau^2) from RE1.
#' E_sig = (sum(E_sigi)) / k, where E_sigi = 1 - N(Z_i) and
#' Z_i = (1.96 * SE_i - |UWLS|) / sqrt(SE_i^2 + tau^2)
#'
#' @param df [data.frame] The data frame with 'effect' and 'se' columns
#' @param uwls_estimate [numeric] The UWLS estimate (mean effect)
#' @param tau2 [numeric] Heterogeneity variance (tau^2) from RE1
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [numeric] Expected number of significant studies
#' @export
calculate_expected_significant <- function(df, uwls_estimate, tau2, se = NULL, alpha = 0.05) {
  esigi <- calculate_esigi(df, uwls_estimate, tau2, se, alpha)
  # E_sig = (<U+03A3> E_sigi) / k, but we return the sum for expected count
  # The proportion is calculated separately by dividing by k
  sum(esigi, na.rm = TRUE)
}

#' Calculate observed number of statistically significant studies
#'
#' Counts only studies that are statistically and positively significant.
#' Uses t-distribution critical values adjusted for degrees of freedom.
#'
#' @param df [data.frame] The data frame with 't_value' and 'dof' columns
#' @param alpha [numeric] Significance level (default: 0.05 for one-sided test)
#' @return [numeric] Observed number of positive significant studies
#' @export
calculate_observed_significant <- function(df, alpha = 0.05) {
  stopifnot("t_value" %in% colnames(df))
  stopifnot("dof" %in% colnames(df))

  # Calculate critical value for each study using t-distribution with df
  # One-sided test: t_value > critical_value (positive significance only)
  valid_rows <- !is.na(df$t_value) & !is.na(df$dof) & df$dof > 0

  if (sum(valid_rows) == 0) {
    return(0)
  }

  # Critical value for one-sided test using t-distribution
  # qt(1 - alpha, df) gives the critical value for one-sided test
  critical_values <- stats::qt(1 - alpha, df = df$dof[valid_rows])

  # Count studies where t_value > critical_value (positive significance only)
  significant <- df$t_value[valid_rows] > critical_values
  sum(significant, na.rm = TRUE)
}

#' Calculate Excess Statistical Significance (ESS)
#'
#' ESS = observed_significant - expected_significant
#'
#' @param df [data.frame] The data frame with 'effect', 'se', and 't_value' columns
#' @param uwls_estimate [numeric] The UWLS estimate (mean effect)
#' @param tau2 [numeric] Heterogeneity variance (tau^2) from RE1
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [numeric] Excess Statistical Significance
#' @export
calculate_ess <- function(df, uwls_estimate, tau2, se = NULL, alpha = 0.05) {
  observed <- calculate_observed_significant(df, alpha)
  expected <- calculate_expected_significant(df, uwls_estimate, tau2, se, alpha)
  observed - expected
}


#' Calculate all PSB measures for a given method
#'
#' @param df [data.frame] The data frame with 'effect', 'se', 't_value', and 'dof' columns
#' @param method_name [character] Name of the method (e.g., "uwls", "uwls3", "hs")
#' @param uwls_estimate [numeric] The UWLS estimate (mean effect) from the method
#' @param tau2 [numeric] Heterogeneity variance (tau^2) from RE1
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [list] List containing PSB measures (ess_prop, esig, pss, k) all as proportions
#' @export
calculate_psb_measures <- function(df, method_name, uwls_estimate, tau2, alpha = 0.05) {
  if (is.na(uwls_estimate) || is.na(tau2)) {
    return(list(
      ess_prop = NA_real_,
      esig = NA_real_,
      pss = NA_real_,
      k = NA_real_
    ))
  }

  n_total <- nrow(df)
  observed_count <- calculate_observed_significant(df, alpha)
  expected_count <- calculate_expected_significant(df, uwls_estimate, tau2, se = NULL, alpha = alpha)

  # Calculate proportions: P_ss, E_sig, and ESS
  pss <- if (n_total > 0) observed_count / n_total else NA_real_
  esig <- if (n_total > 0) expected_count / n_total else NA_real_
  ess_count <- calculate_ess(df, uwls_estimate, tau2, se = NULL, alpha = alpha)
  ess_prop <- if (n_total > 0) ess_count / n_total else NA_real_

  list(
    ess_prop = ess_prop,
    esig = esig,
    pss = pss,
    k = n_total
  )
}

#' Calculate PSB measures for a single meta-analysis using all three methods
#'
#' Calculates PSB measures (ESS, E_sig, P_ss) all as proportions, plus k (number of estimates),
#' using three different mean effect estimates: UWLS, UWLS+3, and Hunter-Schmidt (HS).
#' Uses one-sided E_sig calculations with tau^2 heterogeneity variance from RE1.
#'
#' @param df [data.frame] The single meta-analysis data frame
#' @param re_method [character] Random effects method for RE1 (default: "ML")
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [data.frame] A data frame with one row containing all PSB measures
#' @export
get_psb_metaflavours <- function(df, re_method = "ML", alpha = 0.05) {
  # Get the name of the meta-analysis
  meta <- unique(df$meta)
  if (length(meta) != 1) {
    cli::cli_abort("Expected exactly one unique meta-analysis name")
  }

  logger::log_debug(paste("Calculating PSB statistics for", meta))

  results <- list(meta = as.character(meta))

  # Calculate RE1 to get tau^2 (heterogeneity variance)
  # RE1 uses S1 standard errors
  tau2 <- get_re1_tau2(df, re_method = re_method)

  if (is.na(tau2)) {
    logger::log_warn(paste("Could not calculate tau^2 for meta-analysis", meta, "- PSB measures will be NA"))
    # Set tau^2 to 0 as fallback (no heterogeneity)
    tau2 <- 0.0
  }

  # Get mean effect estimates from the three methods
  uwls_result <- uwls(df)
  uwls3_result <- uwls3(df)
  hs_result <- hsma(df)

  # Calculate PSB measures for each method
  # Note: All methods use the same tau^2 from RE1, but different UWLS estimates
  methods <- list(
    uwls = list(result = uwls_result, name = "uwls"),
    uwls3 = list(result = uwls3_result, name = "uwls3"),
    hs = list(result = hs_result, name = "hs")
  )

  for (method in methods) {
    method_name <- method$name
    method_result <- method$result
    uwls_estimate <- method_result$est

    # Use UWLS estimate for all methods (as specified in the formula)
    # The formula uses |UWLS| regardless of which method's estimate we're evaluating
    psb_measures <- calculate_psb_measures(df, method_name, uwls_estimate, tau2, alpha)

    # Add all measures with method prefix (all as proportions)
    results[[paste0("ess_", method_name)]] <- psb_measures$ess_prop
    results[[paste0("esig_", method_name)]] <- psb_measures$esig
  }

  # Add P_ss and k (same for all methods)
  n_total <- nrow(df)
  observed_count <- calculate_observed_significant(df, alpha)
  pss <- if (n_total > 0) observed_count / n_total else NA_real_

  results$pss <- pss
  results$k <- n_total

  # Some elements might be numeric(0) here - replace with NA to allow for data frame conversion
  results <- lapply(results, function(x) if (length(x) == 0) NA else x)

  as.data.frame(results)
}
