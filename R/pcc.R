#' Calculate PCC standard error using S1 formula
#'
#' S1 = sqrt((1 - r_p^2) / df)
#' where r_p = t / sqrt(t^2 + df)
#'
#' Assumes all inputs are valid (t_value finite, dof > 0). Use
#' validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame with columns 'effect', 'dof', and 't_value'
#' @return [vector] A vector of PCC standard errors using S1 formula
#' @export
pcc_se_s1 <- function(df) {
  stopifnot("t_value" %in% colnames(df))
  stopifnot("dof" %in% colnames(df))
  stopifnot(all(!is.na(df$t_value) & is.finite(df$t_value)))
  stopifnot(all(!is.na(df$dof) & is.finite(df$dof) & df$dof > 0))

  r_p <- df$t_value / sqrt(df$t_value^2 + df$dof)
  sqrt((1 - r_p^2) / df$dof)
}

#' Calculate PCC standard error using S2 formula
#'
#' S2 variance = (1 - r_p^2)^2 / dof; S2 SE = sqrt(variance).
#' Here r_p = t / sqrt(t^2 + dof).
#'
#' Assumes all inputs are valid (t_value finite, dof > 0). Use
#' validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame with columns 'effect', 'dof', and 't_value'
#' @return [vector] A vector of PCC standard errors using S2 formula
#' @export
pcc_se_s2 <- function(df) {
  stopifnot("t_value" %in% colnames(df))
  stopifnot("dof" %in% colnames(df))
  stopifnot(all(!is.na(df$t_value) & is.finite(df$t_value)))
  stopifnot(all(!is.na(df$dof) & is.finite(df$dof) & df$dof > 0))

  r_p <- df$t_value / sqrt(df$t_value^2 + df$dof)
  sqrt((1 - r_p^2)^2 / df$dof)
}

#' Calculate random effects
#'
#' Assumes all inputs are valid (no NA/infinite values, positive SE).
#' Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame to calculate the RE with
#' @param effect [vector] The vector of effects. If not provided, defaults to df$effect.
#' @param se [vector] The vector of SEs. If not provided, defaults to df$se.
#' @param method [character] The method to use for the RE calculation. Defaults to "ML".
#' @export
re <- function(df, effect = NULL, se = NULL, method = "ML") {
  if (is.null(effect)) effect <- df$effect
  if (is.null(se)) se <- df$se
  stopifnot(length(effect) == nrow(df), length(se) == nrow(df))

  meta <- unique(df$meta)
  stopifnot(length(meta) == 1)
  stopifnot(all(!is.na(effect) & is.finite(effect)))
  stopifnot(all(!is.na(se) & is.finite(se) & se > 0))

  re_data_ <- data.frame(
    yi = effect,
    sei = se,
    study = df$study
  )

  re_ <- metafor::rma(
    yi = re_data_$yi,
    sei = re_data_$sei,
    method = method
  )

  re_est <- re_$beta[1]
  re_se <- re_$se[1]
  re_t_value <- re_est / re_se

  list(est = re_est, t_value = re_t_value)
}

#' Calculate UWLS
#'
#' Assumes all inputs are valid (no NA/infinite values, positive SE).
#' Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame on which to run the calculation
#' @param effect [vector] The vector of effects. If not provided, defaults to df$effect.
#' @param se [vector] The vector of SEs. If not provided, defaults to df$se.
#' @return [list] A list with properties "est", "t_value".
#' @export
uwls <- function(df, effect = NULL, se = NULL) {
  if (is.null(effect)) effect <- df$effect
  if (is.null(se)) se <- df$se
  stopifnot(length(effect) == nrow(df), length(se) == nrow(df))
  stopifnot(all(!is.na(effect) & is.finite(effect)))
  stopifnot(all(!is.na(se) & is.finite(se) & se > 0))

  df_model <- data.frame(
    t = effect / se,
    precision = 1 / se
  )

  uwls_model <- stats::lm(t ~ precision - 1, data = df_model)
  summary_uwls <- summary(uwls_model)
  est <- summary_uwls$coefficients[1, "Estimate"]
  t_value <- summary_uwls$coefficients[1, "t value"]

  list(est = est, t_value = t_value)
}


#' Calculate UWLS+3
#'
#' Uses pre-computed pcc3 column from compute_derived_quantities().
#' Assumes all inputs are valid. Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame with pre-computed 'pcc3' and 'se' columns.
#' @return [list] A list with properties "est", "t_value".
#' @export
uwls3 <- function(df) {
  stopifnot("pcc3" %in% colnames(df))
  stopifnot("se" %in% colnames(df))

  uwls3_data <- data.frame(
    effect = df$pcc3,
    se = df$se,
    meta = df$meta,
    study = df$study
  )

  uwls(uwls3_data)
}

#' Calculate the Hunter-Schmidt estimate
#'
#' Uses pre-computed sample_size column. Assumes all inputs are valid.
#' Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame with 'effect' and 'sample_size' columns
#' @return [list] A list with properties "est", "t_value".
#' @export
hsma <- function(df) {
  stopifnot(all(!is.na(df$effect)))
  stopifnot("sample_size" %in% colnames(df))

  n_ <- df$sample_size
  stopifnot(all(!is.na(n_) & n_ > 0))

  effect <- df$effect
  r_avg <- sum(n_ * effect) / sum(n_)
  sd_sq <- sum(n_ * ((effect - r_avg)^2)) / sum(n_)
  stopifnot(sd_sq >= 0)
  se_r <- sqrt(sd_sq) / sqrt(nrow(df))
  t_value <- r_avg / se_r

  list(est = r_avg, t_value = t_value)
}

#' Calculate WAIV2 (Weighted Average IV version 2)
#'
#' WAIV2 is an instrumental variable version of UWLS that uses sqrt(n) as an IV
#' for 1/SE in the regression of t vs 1/SE, with no constant term.
#'
#' Uses pre-computed sample_size column. Assumes all inputs are valid.
#' Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame on which to run the calculation
#' @param effect [vector] The vector of effects. If not provided, defaults to df$effect.
#' @param se [vector] The vector of SEs. If not provided, defaults to df$se.
#' @return [list] A list with properties "est", "t_value".
#' @export
waiv2 <- function(df, effect = NULL, se = NULL) {
  if (is.null(effect)) effect <- df$effect
  if (is.null(se)) se <- df$se
  stopifnot(length(effect) == nrow(df), length(se) == nrow(df))
  stopifnot("sample_size" %in% colnames(df))

  n_ <- df$sample_size
  stopifnot(all(!is.na(n_) & n_ > 0))
  stopifnot(all(!is.na(effect) & is.finite(effect)))
  stopifnot(all(!is.na(se) & is.finite(se) & se > 0))

  df_model <- data.frame(
    t = effect / se,
    precision = 1 / se,
    sqrt_n = sqrt(n_)
  )

  waiv2_model <- AER::ivreg(t ~ precision - 1 | sqrt_n - 1, data = df_model)
  summary_waiv2 <- summary(waiv2_model)
  est <- summary_waiv2$coefficients[1, "Estimate"]
  se_est <- summary_waiv2$coefficients[1, "Std. Error"]
  t_value <- est / se_est

  list(est = est, t_value = t_value)
}

#' Calculate Fisher's z
#'
#' Uses pre-computed fishers_z and fishers_z_se columns from
#' compute_derived_quantities(). Assumes all inputs are valid.
#' Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The data frame with pre-computed 'fishers_z' and 'fishers_z_se' columns
#' @param method [character] Random effects method for Fisher's z calculation (default: "ML")
#' @return [list] A list with properties "est", "t_value"
#' @export
fishers_z <- function(df, method = "ML") {
  stopifnot("fishers_z" %in% colnames(df))
  stopifnot("fishers_z_se" %in% colnames(df))

  re_data <- data.frame(
    effect = df$fishers_z,
    se = df$fishers_z_se,
    meta = df$meta,
    study = df$study
  )

  re_list <- re(df = re_data, method = method)
  re_est <- re_list$est
  re_t_value <- re_list$t_value

  # Convert back from Fisher's Z to PCC
  re_z <- (exp(2 * re_est) - 1) / (exp(2 * re_est) + 1)

  list(est = re_z, t_value = re_t_value)
}

#' Calculate various summary statistics associated with the PCC data frame
#'
#' Uses pre-computed sample_size column. Assumes all inputs are valid.
#' Use validate_pcc_observations() to ensure this before calling.
#'
#' @param df [data.frame] The PCC data frame with 'sample_size' column
#' @param log_results [logical] Whether to log the results (default: TRUE)
#' @return [list] A list with summary statistics (k_, avg_n, median_n, quantiles, etc.)
#' @export
pcc_sum_stats <- function(df, log_results = TRUE) {
  k_ <- nrow(df)
  stopifnot("sample_size" %in% colnames(df))

  n_ <- df$sample_size
  stopifnot(all(!is.na(n_)))

  quantiles <- stats::quantile(n_, probs = c(0.25, 0.75))

  get_ss_lt <- function(lt) {
    sum(n_ < lt) / k_
  }

  res <- list(
    k_ = k_,
    avg_n = mean(n_),
    median_n = stats::median(n_),
    quantile_1_n = as.numeric(quantiles[1]),
    quantile_3_n = as.numeric(quantiles[2]),
    ss_lt_50 = get_ss_lt(50),
    ss_lt_100 = get_ss_lt(100),
    ss_lt_200 = get_ss_lt(200),
    ss_lt_400 = get_ss_lt(400),
    ss_lt_1600 = get_ss_lt(1600),
    ss_lt_3200 = get_ss_lt(3200)
  )

  if (log_results) {
    logger::log_info("PCC analysis summary statistics:")
    logger::log_info(paste("Number of PCC observations:", k_))
  }
  res
}
