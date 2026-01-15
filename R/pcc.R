#' Calculate the PCC variance.
#'
#' @param df [data.frame] The data frame upon which to calculate the PCC vairance. Should include the columns 'effect', 'sample_size', 'dof'
#' @param offset [int] An offset value to subtract from the degrees of freedom
#'  in case they are missing.
#' @return [vector] A vector of PCC variances.
#' @export
pcc_variance <- function(df, offset) {
  stopifnot(sum(is.na(df$dof)) == 0)

  pcc_ <- df$effect

  numerator <- (1 - pcc_^2)^2
  denominator <- df$dof - offset

  variance <- numerator / denominator
  variance
}

#' Calculate random effects
#'
#' @param df [data.frame] The data frame to calculate the RE with
#' @param effect [vector] The vector of effects. If not provided, defaults to df$effect.
#' @param se [vector] The vector of SEs. If not provided, defaults to df$se.
#' @param method [character] The method to use for the RE calculation. Defaults to "DL". Other common options include "ML" and "REML".
#' @export
re <- function(df, effect = NULL, se = NULL, method = "DL") {
  if (is.null(effect)) effect <- df$effect
  if (is.null(se)) se <- df$se
  stopifnot(length(effect) == nrow(df), length(se) == nrow(df))

  meta <- unique(df$meta)

  result <- tryCatch(
    {
      if (length(meta) != 1) {
        logger::log_warn("Could not calculate RE for multiple meta-analyses")
        return(list(est = NA, t_value = NA))
      }

      re_data_ <- data.frame(yi = effect, sei = se, study = df$study)

      suppressWarnings( # Sometimes the variances are large
        re_ <- metafor::rma(
          yi = re_data_$yi,
          sei = re_data_$sei,
          method = method
        )
      )
      re_est <- re_$beta[1]
      re_se <- re_$se[1]

      re_t_value <- re_est / re_se
      return(list(est = re_est, t_value = re_t_value))
    },
    error = function(e) {
      logger::log_warn(paste("Could not fit the RE model for meta-analysis", meta, ":", conditionMessage(e)))
      list(est = NA, t_value = NA)
    }
  )

  result
}

#' Calculate UWLS
#'
#' @note Here, the statistics upon which the UWLS is calculated are more variable, thus flexibility is provided when defining the input through the 'effect' and 'se' arguments. To see how this can be leveraged, refer, for example, to the 'uwls3' function, or the 'get_chris_meta_flavours' function.
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

  meta <- unique(df$meta)

  df_model <- data.frame(
    t = effect / se,
    precision = 1 / se
  )

  result <- tryCatch(
    {
      uwls <- stats::lm(t ~ precision - 1, data = df_model)
      summary_uwls <- summary(uwls)
      est <- summary_uwls$coefficients[1, "Estimate"]
      t_value <- summary_uwls$coefficients[1, "t value"]
      list(est = est, t_value = t_value)
    },
    error = function(e) {
      logger::log_warn(paste("Could not fit the UWLS model for meta-analysis", meta, ":", conditionMessage(e)))
      list(est = NA, t_value = NA)
    }
  )

  result
}


#' Calculate UWLS+3
#'
#' @param df [data.frame] The data frame to calculate the UWLS+3 with
#' @return [list] A list with properties "est", "t_value".
#' @export
uwls3 <- function(df) {
  meta <- unique(df$meta)

  # Prefer DoF when present; when missing, follow project convention and impute
  # as sample_size - 7.
  dof_ <- get_dof_or_sample_size(df, target = "dof", offset = 0)

  t_ <- df$effect / df$se

  pcc3 <- t_ / sqrt(t_^2 + dof_ + 3) # dof_ + 3 ~~ sample_size - 7 + 3

  # Use the standard error reported in the data frame (not calculated from variance)
  se3 <- df$se

  # Drop observations where either the PCC3 or SE3 are missing
  uwls3_data <- data.frame(effect = pcc3, se = se3, meta = df$meta, study = df$study)
  uwls3_data <- uwls3_data[!is.na(pcc3) & !is.na(se3), ] # Drop NA rows

  if (nrow(uwls3_data) == 0) {
    logger::log_warn(paste("No data to calculate UWLS+3 z for meta-analysis", meta))
    return(list(est = NA, t_value = NA))
  }

  uwls(uwls3_data)
}

#' Calculate the Hunter-Schmidt estimate
#'
#' @param df [data.frame] The data frame to calculate the UWLS+3 with
#' @return [list] A list with properties "est", "t_value".
#' @export
hsma <- function(df) {
  meta <- unique(df$meta)
  stopifnot(sum(is.na(df$effect)) == 0)

  # Use DOF as substitute for sample size when missing
  n_ <- get_dof_or_sample_size(df, target = "sample_size", use_dof_directly = TRUE)

  # Drop rows where both sample_size and dof are missing
  missing_n <- is.na(n_)
  if (any(missing_n)) {
    logger::log_info(paste("Dropping", sum(missing_n), "rows with missing sample size and DOF for meta-analysis", meta))
    df <- df[!missing_n, ]
    n_ <- n_[!missing_n]
  }

  if (nrow(df) == 0) {
    logger::log_warn(paste("No data to calculate HSMA for meta-analysis", meta))
    return(list(est = NA, t_value = NA))
  }

  effect <- df$effect
  r_avg <- sum(n_ * effect) / sum(n_)
  sd_sq <- sum(n_ * ((effect - r_avg)^2)) / sum(n_) # SD_r^2
  stopifnot(sd_sq >= 0) # Assert no negative SD_r^2
  se_r <- sqrt(sd_sq) / sqrt(nrow(df)) # SE_r
  t_value <- r_avg / se_r
  list(est = r_avg, t_value = t_value)
}

#' Calculate WAIV2 (Weighted Average IV version 2)
#'
#' @note WAIV2 is an instrumental variable version of UWLS that uses sqrt(n) as an IV
#' for 1/SE in the regression of t vs 1/SE. This is the simple regression of t vs 1/SE
#' (exactly as used by UWLS) but with sqrt(n) as the IV for 1/SE, with no constant term.
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

  meta <- unique(df$meta)

  # Use DOF as substitute for sample size when missing
  n_ <- get_dof_or_sample_size(df, target = "sample_size", use_dof_directly = TRUE)

  # Drop rows where both sample_size and dof are missing
  missing_n <- is.na(n_)
  if (any(missing_n)) {
    logger::log_info(paste("Dropping", sum(missing_n), "rows with missing sample size and DOF for WAIV2 calculation for meta-analysis", meta))
    df <- df[!missing_n, ]
    effect <- effect[!missing_n]
    se <- se[!missing_n]
    n_ <- n_[!missing_n]
  }

  if (nrow(df) == 0) {
    logger::log_warn(paste("No data to calculate WAIV2 for meta-analysis", meta))
    return(list(est = NA, t_value = NA))
  }

  df_model <- data.frame(
    t = effect / se,
    precision = 1 / se,
    sqrt_n = sqrt(n_)
  )

  result <- tryCatch(
    {
      waiv2_model <- AER::ivreg(t ~ precision - 1 | sqrt_n - 1, data = df_model)
      summary_waiv2 <- summary(waiv2_model)
      est <- summary_waiv2$coefficients[1, "Estimate"]
      se_est <- summary_waiv2$coefficients[1, "Std. Error"]
      t_value <- est / se_est
      list(est = est, t_value = t_value)
    },
    error = function(e) {
      logger::log_warn(paste("Could not fit the WAIV2 model for meta-analysis", meta, ":", conditionMessage(e)))
      list(est = NA, t_value = NA)
    }
  )

  result
}

#' Calculate Fisher's z
#'
#' @note For the calculation, all studies should be present in the dataset.
#' @export
fishers_z <- function(df, method = "ML") {
  meta <- unique(df$meta)

  # Use DOF as substitute for sample size when missing
  n_ <- get_dof_or_sample_size(df, target = "sample_size", use_dof_directly = TRUE)

  suppressWarnings( # Sometimes the log produces NaNs - handled below
    fishers_z_ <- 0.5 * log((1 + df$effect) / (1 - df$effect))
  )
  suppressWarnings( # Sometimes the negative sqrt produces NaNs - handled below
    se_ <- 1 / sqrt(n_ - 3)
  )

  re_data <- data.frame(effect = fishers_z_, se = se_, meta = df$meta, study = df$study)
  re_data <- re_data[!is.na(fishers_z_) & !is.na(se_), ] # Drop NA rows

  if (nrow(re_data) == 0) {
    logger::log_warn(paste("No data to calculate Fisher's z for meta-analysis", meta))
    return(list(est = NA, t_value = NA))
  }

  re_list <- re(df = re_data, method = method)
  re_est <- re_list$est
  re_t_value <- re_list$t_value

  re_z <- (exp(2 * re_est) - 1) / (exp(2 * re_est) + 1)

  list(est = re_z, t_value = re_t_value)
}

#' Calculate various summary statistics associated with the PCC data frame
#' @export
pcc_sum_stats <- function(df, log_results = TRUE) {
  k_ <- nrow(df)

  # Use DOF as n when sample_size is missing (Use df instead of sample size)
  # For summary statistics, estimate sample size as dof + 7
  n_ <- get_dof_or_sample_size(df, target = "sample_size", use_dof_directly = FALSE)

  # Handle case where all n values are missing
  if (all(is.na(n_))) {
    res <- list(
      k_ = k_,
      avg_n = NA_real_,
      median_n = NA_real_,
      quantile_1_n = NA_real_,
      quantile_3_n = NA_real_,
      ss_lt_50 = NA_real_,
      ss_lt_100 = NA_real_,
      ss_lt_200 = NA_real_,
      ss_lt_400 = NA_real_,
      ss_lt_1600 = NA_real_,
      ss_lt_3200 = NA_real_
    )

    if (log_results) {
      logger::log_info("PCC analysis summary statistics:")
      logger::log_info(paste("Number of PCC observations:", k_))
    }
    return(res)
  }

  quantiles <- stats::quantile(n_, probs = c(0.25, 0.75), na.rm = TRUE)

  # ss_lt ~ sample sizes less than
  get_ss_lt <- function(lt) {
    if (all(is.na(n_))) {
      return(NA)
    }
    sum(n_ < lt, na.rm = TRUE) / k_
  }

  res <- list(
    k_ = k_,
    avg_n = mean(n_, na.rm = TRUE),
    median_n = stats::median(n_, na.rm = TRUE),
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
