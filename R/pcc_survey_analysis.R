# Main PCC Survey analysis functions
# Extracted and simplified from meta-facilitator

#' Calculate the flavours (statistics) of a single meta-analysis data and return these as a data frame
#'
#' @param df [data.frame] The single meta-analysis data frame
#' @param re_method [character] Random effects method
#' @param re_method_fishers_z [character] Random effects method for Fisher's z
#' @return [data.frame] A data frame with the flavour results
#' @export
get_pcc_survey_metaflavours <- function(df, re_method = "ML", re_method_fishers_z = "ML") {
  # Get the name of the meta-analysis
  meta <- unique(df$meta)
  if (length(meta) != 1) {
    cli::cli_abort("Expected exactly one unique meta-analysis name")
  }

  logger::log_debug(paste("Calculating PCC statistics for", meta, "-", nrow(df), "observations"))

  results <- list(meta = as.character(meta))

  # Use pre-computed S1 and S2 standard errors from compute_derived_quantities()
  stopifnot("se_s1" %in% colnames(df))
  stopifnot("se_s2" %in% colnames(df))

  # Define the various methods to calculate the PCC
  methods <- list(
    re1 = re(df, se = df$se_s1, method = re_method),
    re2 = re(df, se = df$se_s2, method = re_method),
    uwls1 = uwls(df, se = df$se_s1),
    uwls2 = uwls(df, se = df$se_s2),
    uwls3 = uwls3(df),
    hsma = hsma(df),
    fishers_z = fishers_z(df, method = re_method_fishers_z),
    uwlsz = uwls_fishers_z(df),
    simple_mean = simple_mean(df)
  )

  for (method in names(methods)) {
    res <- methods[[method]]
    results[[paste0(method, "_est")]] <- res$est
    results[[paste0(method, "_t_value")]] <- res$t_value
  }

  # Weight concentration (RSM revision, R2 point 3): share of each estimator's
  # total weight carried by its most heavily weighted estimate(s). Estimate
  # level ("<method>_w_top1_estimate" / "_w_top3_estimate") feeds the W_top1 / W_top3 rows
  # of the estimator summary (Table S1); the paper level ("_study", weights
  # summed over a primary study's estimates) is kept alongside in case the
  # reviewer meant papers. See R/weight_concentration.R for the decisions.
  # PET-PEESE has no weight share by design.
  for (method in names(methods)) {
    wc <- weight_concentration(methods[[method]]$weights, df$study)
    results[[paste0(method, "_w_top1_estimate")]] <- wc$top1_est
    results[[paste0(method, "_w_top3_estimate")]] <- wc$top3_est
    results[[paste0(method, "_w_top1_study")]] <- wc$top1_study
    results[[paste0(method, "_w_top3_study")]] <- wc$top3_study
  }

  # Explicit SE of the simple mean (sd(effect)/sqrt(k)); recoverable from the
  # t-value but surfaced as its own column at the co-authors' request.
  results$simple_mean_se <- methods$simple_mean$se

  # Per-MA heterogeneity statistics, all on the S2 (Eq. 3) sampling-error
  # variance the co-authors consider correct: tau2 from the RE2 fit, and gamma
  # (multiplicative variance), Q and I2 from the UWLS2 fit -- which are mutually
  # consistent since UWLS Q and I2 are both derived from the same gamma. (RSM
  # revision; the S1 variants and the RE-based Q/I2 were dropped at their request.)
  results$tau2_re2 <- methods$re2$tau2
  results$gamma_uwls2 <- methods$uwls2$gamma
  results$q_uwls2 <- methods$uwls2$Q
  results$i2_uwls2 <- methods$uwls2$I2

  # Conditional FAT-PET-PEESE on the S1 SE: the publication-bias-corrected
  # benchmark plus the FAT (Egger) coefficient. Stored as plain columns (no
  # "_est" suffix) so they stay out of Table 1 and the smallest-estimate counts.
  fpp <- fat_pet_peese(df, se = df$se_s1, alpha = 0.1)
  results$petpeese <- fpp$est
  results$petpeese_se <- fpp$se
  results$petpeese_type <- fpp$type
  results$fat <- fpp$fat
  results$fat_se <- fpp$fat_se

  sum_stats <- pcc_sum_stats(df, log_results = FALSE)
  results <- c(results, sum_stats)

  as.data.frame(results)
}

#' Run the PCC Survey analysis
#'
#' @param config [list] Configuration list loaded from pcc_survey_config.yaml
#' @param data_dir [character] Directory containing the data file (default: "data")
#' @return [data.frame] The analysis results
#' @export
pcc_survey_analyse <- function(config, data_dir = "data") {
  logger::log_info("Running the PCC Survey analysis")

  # Read the data (with caching if enabled)
  df <- maybe_cached(
    config,
    read_pcc_survey_data,
    file_name = config$data$file_name,
    sheet_name = config$data$sheet_name,
    data_dir = data_dir
  )

  # Clean the data (with caching if enabled)
  df <- maybe_cached(
    config,
    clean_pcc_survey_data,
    df = df,
    cols = config$cols,
    clean_names = config$cleaning$clean_names,
    recalculate_t_value = config$cleaning$recalculate_t_value
  )

  # Optionally subset to single meta-analysis
  meta_substring <- config$filtering$use_single_meta_analysis
  if (!is.null(meta_substring) && is.character(meta_substring)) {
    meta_to_use <- find_string_using_substring(unique(df$meta), meta_substring)
    logger::log_info("Subsetting to data of only ", meta_to_use)
    df <- df[df$meta == meta_to_use, ]
  }

  # Run the PCC analysis - use pcc studies only (with caching if enabled)
  pcc_df <- maybe_cached(
    config,
    get_pcc_data,
    df = data.table::copy(df),
    pcc_identifier = config$analysis$pcc_identifier,
    fill_dof = config$cleaning$fill_dof,
    fill_dof_conditions = config$cleaning$fill_dof_conditions
  )

  log_dataframe_info(df = pcc_df, colnames_to_analyse = c("study", "meta"))

  # Convert inverse relationships for comparability (if enabled)
  flipped_metas <- character(0)
  if (is.null(config$cleaning$convert_inverse_relationships) || config$cleaning$convert_inverse_relationships) {
    pcc_df <- convert_inverse_relationships(pcc_df, log_results = TRUE)
    flipped_metas <- attr(pcc_df, "flipped_metas")
    if (is.null(flipped_metas)) flipped_metas <- character(0)
  }

  # Compute all derived quantities (S1/S2 SE, Fisher's Z, PCC3, etc.)
  pcc_df <- compute_derived_quantities(pcc_df)

  # Build the combined study-level dataset for the aggregate FAT-PET panel (item 3)
  combined_dataset <- build_combined_dataset(pcc_df)

  # Calculate flavours for each meta-analysis
  get_flavours <- function() {
    lapply(split(pcc_df, pcc_df$meta), function(meta_df) {
      get_pcc_survey_metaflavours(
        meta_df,
        re_method = config$methods$re_method,
        re_method_fishers_z = config$methods$re_method_fishers_z
      )
    })
  }

  # Note: Not caching flavours calculation as it was commented out in original
  pcc_list <- get_flavours()
  pcc_df_out <- do.call(rbind, pcc_list)

  # Add an index
  if (config$analysis$add_idx_column) {
    idx <- seq_len(nrow(pcc_df_out))
    pcc_df_out <- cbind(idx, pcc_df_out)
    colnames(pcc_df_out)[1] <- "idx"
  }

  # Sign-flip flag (item 5): mark MAs whose effects were flipped for alignment.
  pcc_df_out$flipped <- pcc_df_out$meta %in% flipped_metas
  logger::log_info(paste("Sign alignment: flagged", sum(pcc_df_out$flipped), "of",
                         nrow(pcc_df_out), "meta-analyses as flipped."))

  # Attach the combined study-level dataset (item 3) for the runner to save.
  attr(pcc_df_out, "combined_dataset") <- combined_dataset

  pcc_df_out
}

# Map estimator column names to human-readable labels, in the Table 1 column
# order requested by the co-authors. Shared by calculate_estimator_summary() and
# calculate_smallest_estimate_counts(). Note "petpeese" has no "_est" suffix, so
# the PP column appears in Table 1 but not in the smallest-estimate counts (which
# select columns via the "_est" suffix).
estimator_display_names <- function() {
  c(
    "simple_mean_est" = "Mean",
    "re1_est" = "RE1",
    "re2_est" = "RE2",
    "uwls1_est" = "UWLS1",
    "uwls2_est" = "UWLS2",
    "uwls3_est" = "UWLS+3",
    "hsma_est" = "HS",
    "fishers_z_est" = "REz",
    "uwlsz_est" = "UWLSz",
    "petpeese" = "PP"
  )
}

#' Calculate estimator summary statistics across meta-analyses
#'
#' Calculates comprehensive summary statistics for each estimator
#' across all individual meta-analyses (excludes "All meta-analyses" row).
#' This produces Table 1 from the analysis.
#' Statistics are returned with estimators as columns and statistics as rows.
#'
#' Besides the standard descriptives, two Table 1 rows depend on other columns:
#' `MSE_PP` is the mean squared difference of each estimator from PET-PEESE
#' (requires a `petpeese` column; NA otherwise and for PP itself), and `Flipped`
#' is the mean estimate over the sign-flipped meta-analyses (requires a `flipped`
#' flag; NA otherwise). Two further rows make up the supplementary Table S1
#' (Reviewer 2's weight-concentration diagnostic): `W_top1` and `W_top3` are the
#' median, across meta-analyses, of the share of the estimator's total weight
#' carried by its most heavily weighted estimate and by its three most heavily
#' weighted estimates (requires `<method>_w_top1_estimate` / `<method>_w_top3_estimate`
#' columns from [get_pcc_survey_metaflavours()]; NA otherwise and for PP, which
#' has no weight share by design; see R/weight_concentration.R).
#'
#' @param results_df [data.frame] Results from pcc_survey_analyse() containing
#'   estimator columns, plus optionally `petpeese`, `flipped` and the
#'   `<method>_w_top1_estimate` / `<method>_w_top3_estimate` columns for the `MSE_PP`,
#'   `Flipped`, `W_top1` and `W_top3` rows.
#' @return [data.frame] Summary table with statistics as rows and estimators as columns
#' @export
calculate_estimator_summary <- function(results_df) {
  # Filter out "All meta-analyses" row
  individual_metas <- results_df[results_df$meta != "All meta-analyses", ]

  # Map column names to readable estimator names. The map also fixes the Table 1
  # column set and order (Mean, RE1, RE2, ..., UWLSz, PP), restricted to columns
  # actually present. Unlike the smallest-estimate counts this includes the
  # PET-PEESE ("petpeese" -> "PP") column, which has no "_est" suffix.
  estimator_names <- estimator_display_names()
  estimator_cols <- names(estimator_names)[names(estimator_names) %in% colnames(individual_metas)]

  if (length(estimator_cols) == 0) {
    cli::cli_abort("No estimator columns found")
  }

  # Helper function to calculate skewness
  calculate_skewness <- function(x) {
    if (length(x) < 3) {
      return(NA_real_)
    }
    x_centered <- x - mean(x)
    n <- length(x)
    numerator <- sum(x_centered^3) / n
    denominator <- (sum(x_centered^2) / n)^(3 / 2)
    if (denominator == 0) {
      return(NA_real_)
    }
    numerator / denominator
  }

  # Calculate statistics for each estimator
  summary_stats <- lapply(estimator_cols, function(col) {
    values <- individual_metas[[col]]
    values_clean <- values[!is.na(values)]
    n_total <- length(values)
    n_missing <- sum(is.na(values))
    n_valid <- length(values_clean)

    # MSE relative to PET-PEESE ("MSE-PP" row of Table 1): the mean squared
    # difference between this estimator and PET-PEESE across meta-analyses.
    # PET-PEESE versus itself is left undefined (NA, shown blank in Table 1);
    # NA also when the "petpeese" column is absent.
    pp_vals <- individual_metas[["petpeese"]]
    if (col == "petpeese" || is.null(pp_vals)) {
      mse_pp <- NA_real_
    } else {
      sq_diff <- (values - pp_vals)^2
      mse_pp <- if (all(is.na(sq_diff))) NA_real_ else mean(sq_diff, na.rm = TRUE)
    }

    # Mean over the sign-flipped meta-analyses ("Flipped" row of Table 1): the
    # average estimate restricted to MAs whose effects were flipped for sign
    # alignment (see convert_inverse_relationships()). NA when the "flipped"
    # flag is absent or no meta-analysis was flipped.
    flipped_flag <- individual_metas[["flipped"]]
    if (is.null(flipped_flag)) {
      flipped_mean <- NA_real_
    } else {
      flipped_vals <- values[as.logical(flipped_flag)]
      flipped_vals <- flipped_vals[!is.na(flipped_vals)]
      flipped_mean <- if (length(flipped_vals) == 0) NA_real_ else mean(flipped_vals)
    }

    # Weight-concentration rows ("W_top1" / "W_top3" of Table S1): median across
    # meta-analyses of the share of total weight carried by the top 1 / top 3
    # estimates (the median, not the mean, because a few extreme MAs distort the
    # mean). The per-MA shares live in "<method>_w_top1_estimate" /
    # "<method>_w_top3_estimate", with <method> the estimator column minus "_est". NA
    # when those columns are absent and for PET-PEESE ("petpeese"), which has no
    # weight share (see R/weight_concentration.R).
    median_share <- function(suffix) {
      if (col == "petpeese") return(NA_real_)
      share_col <- paste0(sub("_est$", "", col), suffix)
      shares <- individual_metas[[share_col]]
      if (is.null(shares) || all(is.na(shares))) NA_real_ else stats::median(shares, na.rm = TRUE)
    }
    w_top1 <- median_share("_w_top1_estimate")
    w_top3 <- median_share("_w_top3_estimate")

    # Common values for all cases
    count_val <- as.integer(n_total)
    missing_val <- as.integer(n_missing)

    # Calculate statistics based on number of valid values
    if (n_valid == 0) {
      # All values are NA
      minimum <- max_val <- skewness <- median_val <- iqr <- trimmed_mean <- mean_val <- sd_val <- NA_real_
    } else if (n_valid == 1) {
      # Single value - some stats are undefined
      minimum <- max_val <- median_val <- trimmed_mean <- mean_val <- values_clean
      skewness <- iqr <- sd_val <- NA_real_
    } else {
      # Multiple values - calculate all statistics
      quantiles <- stats::quantile(values_clean, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      minimum <- min(values_clean)
      max_val <- max(values_clean)
      skewness <- calculate_skewness(values_clean)
      median_val <- quantiles[2] # 50th percentile
      iqr <- quantiles[3] - quantiles[1] # Q3 - Q1
      trimmed_mean <- mean(values_clean, trim = 0.1)
      mean_val <- mean(values_clean)
      sd_val <- sd(values_clean)
    }

    # Return as named list
    list(
      count = count_val,
      minimum = minimum,
      max = max_val,
      missing = missing_val,
      skewness = skewness,
      median = median_val,
      IQR = iqr,
      trimmed_mean_10 = trimmed_mean,
      Mean = mean_val,
      SD = sd_val,
      MSE_PP = mse_pp,
      Flipped = flipped_mean,
      W_top1 = w_top1,
      W_top3 = w_top3
    )
  })

  # Create data frame with statistics as rows and estimators as columns
  stat_names <- c(
    "Mean", "median", "SD", "count", "minimum", "max", "missing",
    "skewness", "IQR", "trimmed_mean_10", "MSE_PP", "Flipped", "W_top1", "W_top3"
  )
  summary_df <- data.frame(
    Statistic = stat_names,
    stringsAsFactors = FALSE
  )

  # Calculate indices for integer statistics (calculated once)
  count_idx <- which(stat_names == "count")
  missing_idx <- which(stat_names == "missing")

  # Add each estimator as a column
  for (i in seq_along(estimator_cols)) {
    col <- estimator_cols[i]
    estimator_name <- if (col %in% names(estimator_names)) estimator_names[[col]] else col
    stats_for_col <- summary_stats[[i]]

    # Build values vector, handling integers separately
    values <- numeric(length(stat_names))
    for (j in seq_along(stat_names)) {
      stat <- stat_names[j]
      if (j == count_idx || j == missing_idx) {
        values[j] <- as.integer(stats_for_col[[stat]])
      } else {
        values[j] <- as.numeric(stats_for_col[[stat]])
      }
    }

    summary_df[[estimator_name]] <- values
  }

  summary_df
}

#' Count how often each estimator gives the smallest (most conservative) estimate
#'
#' Across the individual meta-analyses (excludes any "All meta-analyses" row),
#' tallies for each estimator (a) how often it is the smallest *signed* estimate
#' within its meta-analysis and (b) how often it is negative. Effects are assumed
#' already sign-aligned (see [convert_inverse_relationships()]), so the smallest
#' signed estimate is the most conservative. Framed as "most conservative", not
#' "least biased". Ties (estimators equal to the row minimum) are counted for each
#' tied estimator, so `times_smallest` can sum to slightly more than `n_metas`.
#'
#' @param results_df [data.frame] Results from pcc_survey_analyse() with estimator
#'   columns ending in "_est".
#' @return [data.frame] Columns: estimator, times_smallest, times_negative, n_metas.
#' @export
calculate_smallest_estimate_counts <- function(results_df) {
  # Filter out "All meta-analyses" row
  individual_metas <- results_df[results_df$meta != "All meta-analyses", ]

  # Estimator columns: the known point-estimate columns (ending with "_est") in
  # Table 1 order, restricted to those present. Selecting from the display-name
  # map rather than by suffix keeps other "_est*"-named columns (e.g. the
  # "_w_top1_estimate" weight shares) out of the contest. PET-PEESE has no
  # "_est" suffix and is excluded by design.
  estimator_names <- estimator_display_names()
  known_est_cols <- grep("_est$", names(estimator_names), value = TRUE)
  estimator_cols <- known_est_cols[known_est_cols %in% colnames(individual_metas)]
  if (length(estimator_cols) == 0) {
    cli::cli_abort("No estimator columns found (columns ending with '_est')")
  }
  est_matrix <- as.matrix(individual_metas[, estimator_cols, drop = FALSE])
  n_metas <- nrow(est_matrix)

  times_smallest <- stats::setNames(integer(length(estimator_cols)), estimator_cols)
  for (i in seq_len(n_metas)) {
    row_vals <- est_matrix[i, ]
    if (all(is.na(row_vals))) next
    row_min <- min(row_vals, na.rm = TRUE)
    is_smallest <- !is.na(row_vals) & row_vals == row_min
    times_smallest[is_smallest] <- times_smallest[is_smallest] + 1L
  }

  times_negative <- vapply(estimator_cols, function(col) {
    vals <- individual_metas[[col]]
    sum(!is.na(vals) & vals < 0)
  }, integer(1))

  readable <- vapply(estimator_cols, function(col) {
    if (col %in% names(estimator_names)) estimator_names[[col]] else col
  }, character(1))

  data.frame(
    estimator = unname(readable),
    times_smallest = as.integer(times_smallest),
    times_negative = as.integer(times_negative),
    n_metas = as.integer(n_metas),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Build the combined study-level dataset across all meta-analyses
#'
#' Produces one row per PCC observation with an `idx` identifying its
#' meta-analysis (1..N in alphabetical meta order). The `idx` matches the `idx`
#' of the per-MA summary produced by [pcc_survey_analyse()], which orders
#' meta-analyses via `split(pcc_df, pcc_df$meta)` (alphabetical), so the two
#' files align row-for-MA. Intended for an aggregate FAT-PET panel model.
#'
#' @param pcc_df [data.frame] Per-observation PCC data after
#'   [compute_derived_quantities()]. Needs columns: meta, study, effect, se_s1,
#'   sample_size.
#' @return [data.frame] Columns: idx, meta, study, effect, se, sample_size.
#' @export
build_combined_dataset <- function(pcc_df) {
  required <- c("meta", "study", "effect", "se_s1", "sample_size")
  missing_cols <- setdiff(required, colnames(pcc_df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("build_combined_dataset is missing required columns: {.val {missing_cols}}")
  }

  meta_chr <- as.character(pcc_df$meta)
  meta_levels <- sort(unique(meta_chr))
  idx <- match(meta_chr, meta_levels)

  data.frame(
    idx = idx,
    meta = meta_chr,
    study = as.character(pcc_df$study),
    effect = pcc_df$effect,
    se = pcc_df$se_s1,
    sample_size = pcc_df$sample_size,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
