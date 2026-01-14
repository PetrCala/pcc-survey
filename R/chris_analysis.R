# Main Chris analysis functions
# Extracted and simplified from meta-facilitator

#' Calculate the flavours (statistics) of a single meta-analysis data and return these as a data frame
#'
#' @param df [data.frame] The single meta-analysis data frame
#' @param re_method [character] Random effects method
#' @param re_method_fishers_z [character] Random effects method for Fisher's z
#' @return [data.frame] A data frame with the flavour results
#' @export
get_chris_metaflavours <- function(df, re_method = "ML", re_method_fishers_z = "ML") {
  # Get the name of the meta-analysis
  meta <- unique(df$meta)
  if (length(meta) != 1) {
    cli::cli_abort("Expected exactly one unique meta-analysis name")
  }

  logger::log_debug(paste("Calculating PCC statistics for", meta))

  # Get the standard errors - silence NaNs
  suppressWarnings(se_ <- sqrt(df[["pcc_var"]]))

  results <- list(meta = as.character(meta))

  # Define the various methods to calculate the PCC
  methods <- list(
    re = re(df, method = re_method),
    uwls = uwls(df),
    uwls3 = uwls3(df),
    hsma = hsma(df),
    fishers_z = fishers_z(df, method = re_method_fishers_z)
  )

  for (method in names(methods)) {
    res <- methods[[method]]
    results[[paste0(method, "_est")]] <- res$est
    results[[paste0(method, "_t_value")]] <- res$t_value
  }

  sum_stats <- pcc_sum_stats(df, log_results = FALSE)

  results <- c(results, sum_stats)

  # Some elements might be numeric(0) here - replace with NA to allow for data frame conversion
  results <- lapply(results, function(x) if (length(x) == 0) NA else x)

  as.data.frame(results)
}

#' Run the Chris analysis
#'
#' @param config [list] Configuration list loaded from chris_config.yaml
#' @return [data.frame] The analysis results
#' @export
chris_analyse <- function(config) {
  logger::log_info("Running the chris analysis")

  # Read the data (with caching if enabled)
  df <- maybe_cached(
    config,
    read_chris_data,
    file_name = config$data$file_name,
    sheet_name = config$data$sheet_name
  )

  # Clean the data (with caching if enabled)
  df <- maybe_cached(
    config,
    clean_chris_data,
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
  if (is.null(config$cleaning$convert_inverse_relationships) || config$cleaning$convert_inverse_relationships) {
    pcc_df <- convert_inverse_relationships(pcc_df, log_results = TRUE)
  }

  # Calculate flavours for each meta-analysis
  get_flavours <- function() {
    lapply(split(pcc_df, pcc_df$meta), function(meta_df) {
      get_chris_metaflavours(
        meta_df,
        re_method = config$methods$re_method,
        re_method_fishers_z = config$methods$re_method_fishers_z
      )
    })
  }

  # Note: Not caching flavours calculation as it was commented out in original
  pcc_list <- get_flavours()
  pcc_df_out <- do.call(rbind, pcc_list)

  # Add a row for the full data frame
  pcc_df$meta <- "All meta-analyses"
  pcc_full_df <- get_chris_metaflavours(
    pcc_df,
    re_method = config$methods$re_method,
    re_method_fishers_z = config$methods$re_method_fishers_z
  )
  pcc_df_out <- rbind(pcc_df_out, pcc_full_df)

  # Add an index
  if (config$analysis$add_idx_column) {
    idx <- seq_len(nrow(pcc_df_out))
    pcc_df_out <- cbind(idx, pcc_df_out)
    colnames(pcc_df_out)[1] <- "idx"
  }

  pcc_df_out
}

#' Calculate estimator summary statistics across meta-analyses
#'
#' Calculates comprehensive summary statistics for each estimator
#' across all individual meta-analyses (excludes "All meta-analyses" row).
#' This produces Table 1 from the analysis.
#' Statistics are returned with estimators as columns and statistics as rows.
#'
#' @param results_df [data.frame] Results from chris_analyse() containing estimator columns
#' @return [data.frame] Summary table with statistics as rows and estimators as columns
#' @export
calculate_estimator_summary <- function(results_df) {
  # Filter out "All meta-analyses" row
  individual_metas <- results_df[results_df$meta != "All meta-analyses", ]

  # Get estimator columns (columns ending with _est)
  estimator_cols <- grep("_est$", colnames(individual_metas), value = TRUE)

  if (length(estimator_cols) == 0) {
    cli::cli_abort("No estimator columns found (columns ending with '_est')")
  }

  # Map column names to readable estimator names
  estimator_names <- c(
    "re_est" = "RE",
    "uwls_est" = "UWLS",
    "uwls3_est" = "UWLS3",
    "hsma_est" = "HSMA",
    "fishers_z_est" = "Fisher's z"
  )

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
      SD = sd_val
    )
  })

  # Create data frame with statistics as rows and estimators as columns
  stat_names <- c("Mean", "median", "SD", "count", "minimum", "max", "missing", "skewness", "IQR", "trimmed_mean_10")
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
