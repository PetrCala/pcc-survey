# Publication Selection Bias (PSB) Analysis Functions

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
#' @param df [data.frame] The data frame with 'effect' and 'se' columns
#' @param mean_effect [numeric] The assumed true mean effect size
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [numeric] Expected number of significant studies
#' @export
calculate_expected_significant <- function(df, mean_effect, se = NULL, alpha = 0.05) {
  power <- calculate_study_power(df, mean_effect, se, alpha)
  sum(power, na.rm = TRUE)
}

#' Calculate observed number of statistically significant studies
#'
#' @param df [data.frame] The data frame with 't_value' column
#' @param alpha [numeric] Significance level (default: 0.05 for two-tailed test)
#' @return [numeric] Observed number of significant studies
#' @export
calculate_observed_significant <- function(df, alpha = 0.05) {
  stopifnot("t_value" %in% colnames(df))

  # Critical value for two-tailed test
  critical_value <- stats::qnorm(1 - alpha / 2)

  # Count studies where |t_value| > critical_value
  significant <- abs(df$t_value) > critical_value
  sum(significant, na.rm = TRUE)
}

#' Calculate Excess Statistical Significance (ESS)
#'
#' ESS = observed_significant - expected_significant
#'
#' @param df [data.frame] The data frame with 'effect', 'se', and 't_value' columns
#' @param mean_effect [numeric] The assumed true mean effect size
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [numeric] Excess Statistical Significance
#' @export
calculate_ess <- function(df, mean_effect, se = NULL, alpha = 0.05) {
  observed <- calculate_observed_significant(df, alpha)
  expected <- calculate_expected_significant(df, mean_effect, se, alpha)
  observed - expected
}

#' Calculate Proportion of Statistically Significant Tests (PSST)
#'
#' PSST = (observed proportion) / (expected proportion)
#' Where proportions are: significant_studies / total_studies
#'
#' @param df [data.frame] The data frame with 'effect', 'se', and 't_value' columns
#' @param mean_effect [numeric] The assumed true mean effect size
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [numeric] PSST value
#' @export
calculate_psst <- function(df, mean_effect, se = NULL, alpha = 0.05) {
  n_total <- nrow(df)
  if (n_total == 0) {
    return(NA_real_)
  }

  observed_count <- calculate_observed_significant(df, alpha)
  expected_count <- calculate_expected_significant(df, mean_effect, se, alpha)

  observed_prop <- observed_count / n_total
  expected_prop <- expected_count / n_total

  # Avoid division by zero
  if (expected_prop == 0) {
    if (observed_prop == 0) {
      return(1.0) # Both zero, ratio is 1
    } else {
      return(NA_real_) # Expected is zero but observed is not
    }
  }

  observed_prop / expected_prop
}

#' Calculate Proportion Selected to be Statistically Significant (Psss)
#'
#' Psss = ESS / (1 - expected_proportion_of_significant)
#'
#' @param df [data.frame] The data frame with 'effect', 'se', and 't_value' columns
#' @param mean_effect [numeric] The assumed true mean effect size
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [numeric] Psss value
#' @export
calculate_psss <- function(df, mean_effect, se = NULL, alpha = 0.05) {
  n_total <- nrow(df)
  if (n_total == 0) {
    return(NA_real_)
  }

  ess <- calculate_ess(df, mean_effect, se, alpha)
  expected_count <- calculate_expected_significant(df, mean_effect, se, alpha)
  expected_prop <- expected_count / n_total

  denominator <- 1 - expected_prop

  # Avoid division by zero
  if (abs(denominator) < 1e-10) {
    return(NA_real_)
  }

  ess / denominator
}

#' Calculate falsely positive evidence
#'
#' Falsely positive evidence = ESS / total_studies
#'
#' @param df [data.frame] The data frame with 'effect', 'se', and 't_value' columns
#' @param mean_effect [numeric] The assumed true mean effect size
#' @param se [vector] Standard errors for each study. If NULL, uses df$se
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [numeric] Falsely positive evidence (proportion)
#' @export
calculate_falsely_positive <- function(df, mean_effect, se = NULL, alpha = 0.05) {
  n_total <- nrow(df)
  if (n_total == 0) {
    return(NA_real_)
  }

  ess <- calculate_ess(df, mean_effect, se, alpha)
  ess / n_total
}

#' Calculate all PSB measures for a given method
#'
#' @param df [data.frame] The data frame with 'effect', 'se', and 't_value' columns
#' @param method_name [character] Name of the method (e.g., "uwls", "uwls3", "hs")
#' @param mean_effect [numeric] The mean effect estimate from the method
#' @param alpha [numeric] Significance level (default: 0.05)
#' @return [list] List containing all PSB measures
#' @export
calculate_psb_measures <- function(df, method_name, mean_effect, alpha = 0.05) {
  if (is.na(mean_effect)) {
    return(list(
      ess = NA_real_,
      psst = NA_real_,
      psss = NA_real_,
      falsely_positive = NA_real_,
      observed_prop = NA_real_,
      expected_prop = NA_real_,
      observed_count = NA_real_,
      expected_count = NA_real_
    ))
  }

  n_total <- nrow(df)
  observed_count <- calculate_observed_significant(df, alpha)
  expected_count <- calculate_expected_significant(df, mean_effect, se = NULL, alpha = alpha)

  observed_prop <- if (n_total > 0) observed_count / n_total else NA_real_
  expected_prop <- if (n_total > 0) expected_count / n_total else NA_real_

  ess <- calculate_ess(df, mean_effect, se = NULL, alpha = alpha)
  psst <- calculate_psst(df, mean_effect, se = NULL, alpha = alpha)
  psss <- calculate_psss(df, mean_effect, se = NULL, alpha = alpha)
  falsely_positive <- calculate_falsely_positive(df, mean_effect, se = NULL, alpha = alpha)

  list(
    ess = ess,
    psst = psst,
    psss = psss,
    falsely_positive = falsely_positive,
    observed_prop = observed_prop,
    expected_prop = expected_prop,
    observed_count = observed_count,
    expected_count = expected_count
  )
}

#' Calculate PSB measures for a single meta-analysis using all three methods
#'
#' Calculates PSB measures (ESS, PSST, Psss, falsely positive evidence) using
#' three different mean effect estimates: UWLS, UWLS+3, and Hunter-Schmidt (HS).
#'
#' @param df [data.frame] The single meta-analysis data frame
#' @param re_method [character] Random effects method (not used for PSB, but kept for consistency)
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
    method_result <- method$result
    mean_effect <- method_result$est

    psb_measures <- calculate_psb_measures(df, method_name, mean_effect, alpha)

    # Add all measures with method prefix
    results[[paste0("ess_", method_name)]] <- psb_measures$ess
    results[[paste0("psst_", method_name)]] <- psb_measures$psst
    results[[paste0("psss_", method_name)]] <- psb_measures$psss
    results[[paste0("falsely_positive_", method_name)]] <- psb_measures$falsely_positive
    results[[paste0("expected_prop_ss_", method_name)]] <- psb_measures$expected_prop
    results[[paste0("expected_count_ss_", method_name)]] <- psb_measures$expected_count
  }

  # Add observed values (same for all methods)
  observed_count <- calculate_observed_significant(df, alpha)
  n_total <- nrow(df)
  observed_prop <- if (n_total > 0) observed_count / n_total else NA_real_

  results$observed_prop_ss <- observed_prop
  results$observed_count_ss <- observed_count

  # Some elements might be numeric(0) here - replace with NA to allow for data frame conversion
  results <- lapply(results, function(x) if (length(x) == 0) NA else x)

  as.data.frame(results)
}
