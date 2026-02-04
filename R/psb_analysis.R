# Publication Selection Bias (PSB) Analysis Functions

#' Calculate RE1 model and extract tau^2 (heterogeneity variance)
#'
#' RE1 uses S1 standard errors and a random effects model to estimate tau^2.
#' This tau^2 is used in the E_sig calculations to account for heterogeneity.
#'
#' Uses pre-computed se_s1 column from compute_derived_quantities().
#' Assumes all inputs are valid. Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame with 'effect' and 'se_s1' columns
#' @param re_method [character] Random effects method (default: "ML")
#' @return [numeric] tau^2 (heterogeneity variance)
#' @export
get_re1_tau2 <- function(df, re_method = "ML") {
  meta <- unique(df$meta)
  stopifnot(length(meta) == 1)
  stopifnot("se_s1" %in% colnames(df))

  re_ <- metafor::rma(
    yi = df$effect,
    sei = df$se_s1,
    method = re_method
  )

  tau2 <- re_$tau2
  stopifnot(!is.null(tau2) && !is.na(tau2))

  if (tau2 < 0) {
    logger::log_warn(paste("Negative tau^2 from RE1 for meta-analysis", meta, ", setting to 0"))
    tau2 <- 0.0
  }

  tau2
}

#' Calculate E_sigi for each study using one-sided test with heterogeneity
#'
#' Calculates the expected probability of statistical significance for each study
#' using the formula: Z_i = (1.96 * SE_i - UWLS) / sqrt(SE_i^2 + tau^2)
#' and E_sigi = 1 - N(Z_i), where N is the cumulative normal distribution.
#'
#' This is a one-sided calculation that includes random heterogeneity from RE1's
#' estimate of tau^2.
#'
#' Note: Inverse relationships have already been converted (multiplied by -1),
#' so the absolute value is not needed.
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
  stopifnot(is.numeric(uwls_estimate), length(uwls_estimate) == 1, !is.na(uwls_estimate))
  stopifnot(is.numeric(tau2), length(tau2) == 1, tau2 >= 0)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)
  stopifnot(all(!is.na(se) & is.finite(se) & se > 0))

  critical_value <- stats::qnorm(1 - alpha)

  numerator <- critical_value * se - uwls_estimate
  denominator <- sqrt(se^2 + tau2)

  z_i <- numerator / denominator

  # E_sigi = 1 - N(Z_i)
  1 - stats::pnorm(z_i)
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
  stopifnot(is.numeric(mean_effect), length(mean_effect) == 1, !is.na(mean_effect))
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)
  stopifnot(all(!is.na(se) & is.finite(se) & se > 0))

  critical_value <- stats::qnorm(1 - alpha / 2)
  lambda <- mean_effect / se

  1 - stats::pnorm(critical_value, mean = lambda, sd = 1) +
    stats::pnorm(-critical_value, mean = lambda, sd = 1)
}

#' Calculate expected number of statistically significant studies
#'
#' Uses one-sided calculation with heterogeneity variance (tau^2) from RE1.
#' E_sig = (sum(E_sigi)) / k, where E_sigi = 1 - N(Z_i) and
#' Z_i = (1.96 * SE_i - UWLS) / sqrt(SE_i^2 + tau^2)
#' Note: No absolute value needed since inverse relationships are already converted.
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
  sum(esigi)
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
  stopifnot(all(!is.na(df$t_value) & is.finite(df$t_value)))
  stopifnot(all(!is.na(df$dof) & df$dof > 0))

  critical_values <- stats::qt(1 - alpha, df = df$dof)
  significant <- df$t_value > critical_values
  sum(significant)
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
  stopifnot(!is.na(uwls_estimate), !is.na(tau2))

  n_total <- nrow(df)
  stopifnot(n_total > 0)

  observed_count <- calculate_observed_significant(df, alpha)
  expected_count <- calculate_expected_significant(df, uwls_estimate, tau2, se = NULL, alpha = alpha)
  ess_count <- calculate_ess(df, uwls_estimate, tau2, se = NULL, alpha = alpha)

  list(
    ess_prop = ess_count / n_total,
    esig = expected_count / n_total,
    pss = observed_count / n_total,
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
  meta <- unique(df$meta)
  if (length(meta) != 1) {
    cli::cli_abort("Expected exactly one unique meta-analysis name")
  }

  logger::log_debug(paste("Calculating PSB statistics for", meta))

  results <- list(meta = as.character(meta))

  # Calculate RE1 to get tau^2 (heterogeneity variance)
  tau2 <- get_re1_tau2(df, re_method = re_method)

  # Get mean effect estimates from the three methods
  uwls_result <- uwls(df)
  uwls3_result <- uwls3(df)
  hs_result <- hsma(df)

  # Calculate PSB measures for each method
  methods <- list(
    uwls = list(result = uwls_result, name = "uwls"),
    uwls3 = list(result = uwls3_result, name = "uwls3"),
    hs = list(result = hs_result, name = "hs")
  )

  for (method in methods) {
    method_name <- method$name
    uwls_estimate <- method$result$est

    psb_measures <- calculate_psb_measures(df, method_name, uwls_estimate, tau2, alpha)

    results[[paste0("ess_", method_name)]] <- psb_measures$ess_prop
    results[[paste0("esig_", method_name)]] <- psb_measures$esig
  }

  # Add P_ss and k (same for all methods)
  n_total <- nrow(df)
  observed_count <- calculate_observed_significant(df, alpha)
  results$pss <- observed_count / n_total
  results$k <- n_total

  as.data.frame(results)
}
