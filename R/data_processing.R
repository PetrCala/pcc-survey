# Data reading, cleaning, and preprocessing functions for Chris analysis
# Extracted and simplified from meta-facilitator

#' Read Chris analysis data from Excel file
#'
#' @param file_name [character] Name of the Excel file
#' @param sheet_name [character] Name of the sheet to read
#' @param data_dir [character] Directory containing the data file
#' @return [data.frame] The loaded data
#' @export
read_chris_data <- function(file_name = "chris_data.xlsx", sheet_name = "Main", data_dir = "data") {
  # Check data availability first (provides helpful error messages)
  check_data_availability(file_name = file_name, data_dir = data_dir, sheet_name = sheet_name)

  file_path <- file.path(data_dir, file_name)

  logger::log_debug("Reading data from ", file_path)
  df <- readxl::read_excel(path = file_path, sheet = sheet_name)

  # Convert tibble to data.frame
  df <- as.data.frame(df)

  logger::log_info("Rows in the raw data frame: ", nrow(df))
  df
}

#' Assign NA to a column in a data frame
#'
#' @param df [data.frame] The data frame
#' @param colname [character] The column name
#' @return [data.frame] The modified data frame
assign_na_col <- function(df, colname) {
  df[[colname]] <- rep(NA, nrow(df))
  df
}

#' Check that a data frame contains specific columns
#'
#' @param df [data.frame] The data frame to check
#' @param columns [character] A vector of column names to check
validate_columns <- function(df, columns) {
  if (!is.data.frame(df)) {
    cli::cli_abort("'df' must be a data frame.")
  }
  if (!is.character(columns)) {
    cli::cli_abort("'columns' must be a character vector")
  }

  missing_cols <- setdiff(columns, colnames(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing_cols}}")
  }
}

#' Fill missing values in a data frame based on changes in other columns
#'
#' @param df [data.frame] The input data frame
#' @param target_col [character] The column to modify if it is NA
#' @param columns [character] A vector of column names to detect changes in
#' @param missing_value_prefix [character] The prefix to use for missing values
#' @return [data.frame] The modified data frame
#' @export
fill_missing_values <- function(df, target_col, columns = c(), missing_value_prefix = "missing value") {
  logger::log_debug(paste0("Filling missing values for col ", target_col, "..."))
  if (!is.character(columns)) {
    cli::cli_abort("The columns parameter must be a character vector")
  }

  # No values to modify by
  if (length(columns) == 0) {
    df[[target_col]] <- missing_value_prefix
    return(df)
  }

  # Detect changes in the specified columns
  change <- rep(FALSE, nrow(df))
  change[1] <- TRUE # First row always counts as a change

  for (i in 2:nrow(df)) {
    for (col in columns) {
      val_current <- df[[col]][i]
      val_previous <- df[[col]][i - 1]
      # Handle NA values: if either is NA, consider it a change if they're not both NA
      if (is.na(val_current) || is.na(val_previous)) {
        if (!identical(is.na(val_current), is.na(val_previous))) {
          change[i] <- TRUE
          break
        }
      } else if (val_current != val_previous) {
        change[i] <- TRUE
        break
      }
    }
  }

  # Count cumulative changes
  change_count <- cumsum(change)

  # Fill missing values
  na_indices <- is.na(df[[target_col]])
  if (any(na_indices)) {
    df[[target_col]][na_indices] <- paste(missing_value_prefix, change_count[na_indices])
  }

  invalid_value <- paste(missing_value_prefix, "NA")
  if (invalid_value %in% df[[target_col]]) {
    cli::cli_abort("The target column contains invalid values. Check the fill function.")
  }

  df
}

#' Fill missing degrees of freedom based on t-values and PCCs
#'
#' Fills DOF values using the formula: dof = t^2 * (1/pcc^2 - 1).
#' Validation of the resulting values (dropping negative, zero, NA, etc.)
#' is handled separately by validate_pcc_observations().
#'
#' @note Only use this function for interpolating missing degrees of freedom of PCC-type effects.
#' @param df [data.frame] The input data frame
#' @param replace_existing [logical] Whether to replace existing degrees of freedom
#' @return [data.frame] The modified data frame
#' @export
fill_dof_using_pcc <- function(df, replace_existing = FALSE) {
  validate_columns(df, c("effect", "se", "t_value", "dof"))

  pcc <- df$effect
  t_values <- df$t_value
  dof <- df$dof

  fillable_rows <- !is.na(t_values) & !is.na(pcc)

  if (!replace_existing) {
    fillable_rows <- fillable_rows & is.na(dof)
  }

  if (sum(fillable_rows) == 0) {
    return(df)
  }

  df[fillable_rows, "dof"] <- calculate_dof(
    t_value = t_values[fillable_rows],
    pcc = pcc[fillable_rows]
  )
  logger::log_info(paste("Filled", sum(fillable_rows), "degrees of freedom."))

  df
}

#' Convert selected columns to numeric
#'
#' @param df [data.frame] The data frame
#' @param cols [character] Column names to convert
#' @return [data.frame] The modified data frame
convert_columns_to_numeric <- function(df, cols) {
  logger::log_info(paste("Converting the following columns to numeric values:", paste(cols, collapse = ", ")))
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    } else {
      logger::log_warn(paste("Column", col, "does not exist in the dataframe"))
    }
  }
  df
}

#' Drop observations with missing values in specified columns
#'
#' @param df [data.frame] The data frame
#' @param cols [character] Column names to check for missing values
#' @return [data.frame] The modified data frame
drop_rows_with_missing_values <- function(df, cols = c("effect")) {
  missing_rows <- rep(FALSE, nrow(df))
  for (col in cols) {
    if (!col %in% colnames(df)) {
      logger::log_warn(paste0("Unknown column name: ", col, ". Skipping NA values check..."))
    } else {
      missing_rows <- missing_rows | is.na(df[[col]])
    }
  }
  logger::log_info(paste("Dropping", sum(missing_rows), "rows where at least one of these columns is missing a value:", paste(cols, collapse = ", ")))

  df[!missing_rows, ]
}

#' Recalculate the t-value based on the effect and se columns
#'
#' @param df [data.frame] The data frame
#' @return [data.frame] The modified data frame
recalculate_t_value <- function(df) {
  logger::log_debug("Recalculating t-values...")
  validate_columns(df, c("effect", "se"))

  if (sum(is.na(df$effect)) > 0) {
    cli::cli_abort("The 'effect' column contains missing values")
  }
  if (sum(is.na(df$se)) > 0) {
    cli::cli_abort("The 'se' column contains missing values")
  }

  t_values <- df$effect / df$se
  t_values[is.infinite(t_values)] <- NA
  df$t_value <- t_values
  df
}

#' Convert string columns to valid R names (remove special characters)
#'
#' @param df [data.frame] The data frame
#' @return [data.frame] The modified data frame
clean_names <- function(df) {
  logger::log_debug("Cleaning names...")
  df$study <- make.names(df$study)
  df$meta <- make.names(df$meta)
  df
}

#' Clean a data frame for analysis
#'
#' @param df [data.frame] The data frame to clean
#' @param cols [list] Column name mappings (internal_name = "source_column_name")
#' @param clean_names [logical] Whether to clean the names
#' @param recalculate_t_value [logical] Whether to recalculate t-values
#' @return [data.frame] The cleaned data frame
#' @export
clean_chris_data <- function(df, cols, clean_names = FALSE, recalculate_t_value = TRUE) {
  logger::log_debug("Cleaning data...")

  # Replace missing columns with NAs
  for (colname in names(cols)) {
    df_colname <- cols[[colname]]
    if (is_empty(df_colname)) {
      df <- assign_na_col(df, colname)
    }
  }

  # Subset to relevant colnames
  get_colname <- function(col) {
    col_mapping <- cols[[col]]
    if (is_empty(col_mapping)) {
      return(col)
    }
    col_mapping
  }
  relevant_colnames <- unlist(lapply(names(cols), get_colname))
  validate_columns(df, relevant_colnames)
  df <- df[, relevant_colnames, drop = FALSE]

  # Rename the columns
  colnames(df) <- names(cols)

  # Drop NA values
  df <- drop_rows_with_missing_values(df, cols = c("effect", "se"))

  # Ensure numeric values
  df <- convert_columns_to_numeric(df, cols = c("effect", "se", "sample_size", "dof"))

  # Fill missing studies
  if ("author1" %in% colnames(df) && "year" %in% colnames(df)) {
    df <- fill_missing_values(
      df = df,
      target_col = "study",
      columns = c("author1", "year"),
      missing_value_prefix = "Missing study"
    )
  }

  if (clean_names) {
    df <- clean_names(df = df)
  }

  if (recalculate_t_value) {
    df <- recalculate_t_value(df = df)
  }

  logger::log_info(paste("Rows after data cleaning:", nrow(df)))

  df
}

#' Get PCC data by filtering and preprocessing
#'
#' @param df [data.frame] The cleaned data frame
#' @param pcc_identifier [character] The effect type identifier for PCC studies
#' @param fill_dof [logical] Whether to fill missing degrees of freedom
#' @param fill_dof_conditions [list] Conditions for DOF filling
#' @return [data.frame] The processed PCC data frame
#' @export
get_pcc_data <- function(df, pcc_identifier = "correlation", fill_dof = TRUE, fill_dof_conditions = NULL) {
  validate_columns(df, c("effect_type"))

  # Subset to PCC studies only
  nrow_full <- nrow(df)
  logger::log_info("Subsetting to PCC studies only...")
  pcc_rows <- df$effect_type == pcc_identifier
  pcc_rows[is.na(pcc_rows)] <- FALSE
  df <- df[pcc_rows, ]
  nrow_pcc <- nrow(df)
  logger::log_info("Loaded ", nrow_pcc, " PCC studies out of ", nrow_full, " rows. (", to_perc(nrow_pcc / nrow_full), " of the clean dataset)")

  if (fill_dof) {
    replace_existing <- FALSE
    if (!is.null(fill_dof_conditions)) {
      replace_existing <- fill_dof_conditions$replace_existing %||% FALSE
    }
    df <- fill_dof_using_pcc(
      df = df,
      replace_existing = replace_existing
    )
  }

  # Pre-compute sample_size where missing (using dof + 7)
  df <- compute_sample_size(df)

  # Apply universal validation filter
  df <- validate_pcc_observations(df)

  df
}

#' Convert inverse relationships to positive for comparability
#'
#' For each meta-analysis, if the median PCC is negative, multiply all PCCs by -1.
#' This ensures all meta-analyses are measured in the same direction (positive correlations).
#' Uses median (rather than mean) to determine if the relationship is predominately negative,
#' which is more robust to outliers. Also flips t-values to maintain the relationship t = effect/se.
#'
#' @param df [data.frame] The PCC data frame with 'meta' and 'effect' columns
#' @param log_results [logical] Whether to log conversion information. Default: TRUE
#' @return [data.frame] The data frame with converted effects (and t-values)
#' @export
convert_inverse_relationships <- function(df, log_results = TRUE) {
  validate_columns(df, c("meta", "effect"))

  # Split by meta-analysis
  meta_list <- split(df, df$meta)
  converted_metas <- character(0)
  n_converted <- 0

  # Process each meta-analysis
  for (meta_name in names(meta_list)) {
    meta_df <- meta_list[[meta_name]]

    # Calculate median PCC for this meta-analysis
    median_effect <- median(meta_df$effect, na.rm = TRUE)

    # Skip if median is NA (all effects are NA)
    if (is.na(median_effect)) {
      if (log_results) {
        logger::log_debug(paste("Skipping conversion for meta-analysis", meta_name, "- all effects are NA"))
      }
      next
    }

    # If median is negative, flip all effects and t-values
    if (median_effect < 0) {
      # Flip effects
      meta_df$effect <- meta_df$effect * -1

      # Flip t-values to maintain t = effect/se relationship
      if ("t_value" %in% colnames(meta_df)) {
        meta_df$t_value <- meta_df$t_value * -1
      }

      # Update the meta_df in the list
      meta_list[[meta_name]] <- meta_df
      converted_metas <- c(converted_metas, meta_name)
      n_converted <- n_converted + 1

      if (log_results) {
        logger::log_info(paste("Converted inverse relationships for meta-analysis:", meta_name, "(median PCC:", round(median_effect, 4), ")"))
      }
    }
  }

  # Recombine all meta-analyses
  result_df <- do.call(rbind, meta_list)
  rownames(result_df) <- NULL # Reset row names

  if (log_results && n_converted > 0) {
    logger::log_info(paste("Converted", n_converted, "meta-analysis(es) with negative median PCCs for comparability"))
  }

  result_df
}

#' Pre-compute sample_size from DOF where missing
#'
#' Sets sample_size = dof + 7 for any row where sample_size is NA.
#' This creates a single definitive sample_size column for all downstream methods.
#'
#' @param df [data.frame] Data frame with 'sample_size' and 'dof' columns
#' @return [data.frame] Data frame with filled sample_size column
#' @export
compute_sample_size <- function(df) {
  validate_columns(df, c("sample_size", "dof"))

  missing_ss <- is.na(df$sample_size)
  if (any(missing_ss)) {
    df$sample_size[missing_ss] <- df$dof[missing_ss] + 7
    logger::log_info(paste("Filled", sum(missing_ss), "missing sample_size values using dof + 7"))
  }

  df
}

#' Validate PCC observations for all downstream methods
#'
#' Applies the strictest union of validity conditions across all methods,
#' ensuring every retained observation can be used by every method.
#' Logs a detailed breakdown of how many rows fail each condition.
#'
#' @param df [data.frame] PCC data frame with columns: effect, se, t_value, dof, sample_size
#' @return [data.frame] Filtered data frame with only universally valid observations
#' @export
validate_pcc_observations <- function(df) {
  validate_columns(df, c("effect", "se", "t_value", "dof", "sample_size"))

  n_before <- nrow(df)

  # Individual condition checks (for logging)
  conditions <- list(
    "effect is NA"           = is.na(df$effect),
    "effect is not finite"   = !is.na(df$effect) & !is.finite(df$effect),
    "|effect| >= 1"          = !is.na(df$effect) & abs(df$effect) >= 1,
    "se is NA"               = is.na(df$se),
    "se is not finite"       = !is.na(df$se) & !is.finite(df$se),
    "se <= 0"                = !is.na(df$se) & is.finite(df$se) & df$se <= 0,
    "t_value is NA"          = is.na(df$t_value),
    "t_value is not finite"  = !is.na(df$t_value) & !is.finite(df$t_value),
    "dof is NA"              = is.na(df$dof),
    "dof is not finite"      = !is.na(df$dof) & !is.finite(df$dof),
    "dof <= 0"               = !is.na(df$dof) & is.finite(df$dof) & df$dof <= 0,
    "sample_size is NA"      = is.na(df$sample_size),
    "sample_size not finite" = !is.na(df$sample_size) & !is.finite(df$sample_size),
    "sample_size <= 3"       = !is.na(df$sample_size) & is.finite(df$sample_size) & df$sample_size <= 3
  )

  for (name in names(conditions)) {
    n_fail <- sum(conditions[[name]])
    if (n_fail > 0) {
      logger::log_info(paste("Validation:", n_fail, "rows fail condition:", name))
    }
  }

  # Combined universal validity mask
  valid <- !is.na(df$effect) & is.finite(df$effect) & abs(df$effect) < 1 &
           !is.na(df$se) & is.finite(df$se) & df$se > 0 &
           !is.na(df$t_value) & is.finite(df$t_value) &
           !is.na(df$dof) & is.finite(df$dof) & df$dof > 0 &
           !is.na(df$sample_size) & is.finite(df$sample_size) & df$sample_size > 3

  df <- df[valid, ]
  n_after <- nrow(df)
  n_dropped <- n_before - n_after

  logger::log_info(paste("Universal validation: dropped", n_dropped, "of", n_before,
                         "rows.", n_after, "rows remain."))

  if (n_after == 0) {
    cli::cli_abort("No valid observations remain after universal validation.")
  }

  df
}

#' Compute all derived quantities needed by the 8 methods
#'
#' Assumes the data has already passed validate_pcc_observations() and
#' convert_inverse_relationships(). Adds columns: se_s1, se_s2,
#' fishers_z, fishers_z_se, pcc3.
#'
#' All derived quantities are mathematically guaranteed to be finite when the
#' universal validity conditions hold (effect finite with |effect| < 1, se > 0,
#' dof > 0, sample_size > 3, t_value finite).
#'
#' @param df [data.frame] Validated PCC data frame
#' @return [data.frame] Data frame with derived quantity columns added
#' @export
compute_derived_quantities <- function(df) {
  validate_columns(df, c("effect", "se", "t_value", "dof", "sample_size"))

  # r_p = t / sqrt(t^2 + dof)
  r_p <- df$t_value / sqrt(df$t_value^2 + df$dof)

  # S1 SE = sqrt((1 - r_p^2) / dof)
  df$se_s1 <- sqrt((1 - r_p^2) / df$dof)

  # S2 variance = (1 - r_p^2)^2 / dof; S2 SE = sqrt(variance)
  df$se_s2 <- sqrt((1 - r_p^2)^2 / df$dof)

  # Fisher's Z = 0.5 * log((1 + r_p) / (1 - r_p))
  df$fishers_z <- 0.5 * log((1 + r_p) / (1 - r_p))

  # Fisher's Z SE = 1 / sqrt(n - 3)
  df$fishers_z_se <- 1 / sqrt(df$sample_size - 3)

  # PCC3 for UWLS+3: t / sqrt(t^2 + dof + 3)
  df$pcc3 <- df$t_value / sqrt(df$t_value^2 + df$dof + 3)

  # Assert all derived columns are finite and non-NA
  derived_cols <- c("se_s1", "se_s2", "fishers_z", "fishers_z_se", "pcc3")
  for (col in derived_cols) {
    if (any(is.na(df[[col]])) || any(!is.finite(df[[col]]))) {
      n_bad <- sum(is.na(df[[col]]) | !is.finite(df[[col]]))
      cli::cli_abort(paste0(
        "Bug: ", n_bad, " non-finite/NA values in derived column '", col,
        "' after validation. This should not happen."
      ))
    }
  }

  logger::log_info(paste("Computed derived quantities for", nrow(df), "observations.",
                         "All", length(derived_cols), "derived columns validated."))

  df
}
