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

#' Helper function to call a function with optional caching based on config
#'
#' @param config [list] Configuration list
#' @param f [function] The function to call
#' @param ... Arguments to pass to the function
#' @return The result of calling f(...)
maybe_cached <- function(config, f, ...) {
  if (config$caching$use_cache) {
    run_cached_function(
      f = f,
      ...,
      use_cache = TRUE,
      cache_dir = "_cache",
      cache_age = config$caching$cache_age
    )
  } else {
    f(...)
  }
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
