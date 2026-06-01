# Entry point for running the PCC Survey analysis
# This function loads the package, sets up logging, and runs the analysis

#' Run the PCC Survey analysis workflow
#'
#' This is the main entrypoint for the PCC Survey analysis. It loads configuration,
#' sets up logging, runs the analysis, and saves results to CSV.
#'
#' @param config_path Path to the PCC Survey config YAML file. Defaults to
#'   `inst/extdata/pcc_survey_config.yaml`.
#' @param use_sample [logical] If TRUE, use sample data from `inst/extdata/sample_data.xlsx`
#'   instead of the full dataset. Useful for testing. Default: FALSE.
#' @return Invisibly returns the analysis results data frame.
#' @export
run_pcc_survey_analysis <- function(config_path = NULL, use_sample = FALSE) {
  # Load config
  if (is.null(config_path)) {
    config_path <- pccsurvey_extdata("pcc_survey_config.yaml")
  }

  if (!file.exists(config_path)) {
    cli::cli_abort("Config file does not exist: {.file {config_path}}")
  }

  config <- yaml::read_yaml(config_path)

  # Override data file if using sample data
  data_dir <- "data"
  if (use_sample) {
    sample_file <- pccsurvey_extdata("sample_data.xlsx")
    if (!file.exists(sample_file)) {
      cli::cli_abort("Sample data file not found: {.file {sample_file}}")
    }
    config$data$file_name <- basename(sample_file)
    config$data$sheet_name <- "Main"
    data_dir <- dirname(sample_file) # Use inst/extdata as data directory for sample
    logger::log_info(paste("Using sample data from:", sample_file))
  }

  # Setup logging
  setup_pcc_survey_logging(
    log_level = config$logging$log_level,
    log_to_console_only = config$logging$log_to_console_only,
    log_file_name = config$logging$log_file_name,
    log_flush_on_setup = config$logging$log_flush_on_setup
  )

  logger::log_info("Starting PCC Survey analysis")

  # Run the analysis
  results <- pcc_survey_analyse(config, data_dir = data_dir)

  # Save results
  save_pcc_survey_results(
    df = results,
    file_name = config$output$file_name,
    output_dir = config$output$output_dir
  )

  # Calculate and save estimator summary (Table 1)
  estimator_summary <- calculate_estimator_summary(results)
  save_estimator_summary(
    summary_df = estimator_summary,
    file_name = config$output$estimator_summary_file_name,
    output_dir = config$output$output_dir
  )

  # Save the combined study-level dataset (item 3) for the aggregate FAT-PET panel
  combined_dataset <- attr(results, "combined_dataset")
  if (!is.null(combined_dataset)) {
    save_combined_dataset(
      combined_df = combined_dataset,
      file_name = config$output$combined_dataset_file_name %||% "pcc_combined_dataset.csv",
      output_dir = config$output$output_dir
    )
  }

  # Calculate and save the smallest-estimate ("most conservative") counts (item 4)
  smallest_counts <- calculate_smallest_estimate_counts(results)
  save_smallest_estimate_counts(
    counts_df = smallest_counts,
    file_name = config$output$smallest_counts_file_name %||% "smallest_estimate_counts.csv",
    output_dir = config$output$output_dir
  )

  # Save session info for reproducibility
  session_info_file <- file.path(config$output$output_dir, "session_info.txt")
  if (!dir.exists(config$output$output_dir)) {
    dir.create(config$output$output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  capture.output(print(sessionInfo(), locale = FALSE), file = session_info_file)
  logger::log_info(paste("Session info saved to:", session_info_file))

  logger::log_info("PCC Survey analysis completed successfully")
  invisible(results)
}
