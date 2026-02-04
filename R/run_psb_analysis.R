# Entry point for running the PSB (Publication Selection Bias) analysis
# This function loads the package, sets up logging, and runs the PSB analysis

#' Run the PSB analysis workflow
#'
#' This is the main entrypoint for the PSB analysis. It loads configuration,
#' sets up logging, runs the analysis, and saves results to CSV.
#'
#' @param config_path Path to the chris config YAML file. Defaults to
#'   `inst/extdata/chris_config.yaml`.
#' @param alpha Significance level for statistical tests (default: 0.05)
#' @return Invisibly returns the PSB analysis results data frame.
#' @export
run_psb_analysis <- function(config_path = NULL, alpha = 0.05) {
  # Load config
  if (is.null(config_path)) {
    config_path <- pccsurvey_extdata("chris_config.yaml")
  }

  if (!file.exists(config_path)) {
    cli::cli_abort("Config file does not exist: {.file {config_path}}")
  }

  config <- yaml::read_yaml(config_path)

  # Setup logging
  setup_chris_logging(
    log_level = config$logging$log_level,
    log_to_console_only = config$logging$log_to_console_only,
    log_file_name = config$logging$log_file_name,
    log_flush_on_setup = config$logging$log_flush_on_setup
  )

  logger::log_info("Starting PSB analysis")

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

  # Compute all derived quantities (S1/S2 SE, Fisher's Z, PCC3, etc.)
  pcc_df <- compute_derived_quantities(pcc_df)

  # Calculate PSB measures for each meta-analysis
  logger::log_info("Calculating PSB measures for each meta-analysis...")
  psb_list <- lapply(split(pcc_df, pcc_df$meta), function(meta_df) {
    get_psb_metaflavours(meta_df, re_method = config$methods$re_method, alpha = alpha)
  })

  psb_df_out <- do.call(rbind, psb_list)

  # Add an index
  if (config$analysis$add_idx_column) {
    idx <- seq_len(nrow(psb_df_out))
    psb_df_out <- cbind(idx, psb_df_out)
    colnames(psb_df_out)[1] <- "idx"
  }

  # Save results
  output_file_name <- "psb_results.csv"
  output_dir <- config$output$output_dir
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_path <- file.path(output_dir, output_file_name)
  logger::log_debug("Saving PSB results to ", output_path)
  utils::write.csv(psb_df_out, file = output_path, row.names = FALSE)
  logger::log_info(paste("PSB results saved to", output_path))

  logger::log_info("PSB analysis completed successfully")
  invisible(psb_df_out)
}
