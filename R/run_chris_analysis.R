# Entry point for nunning the Chris analysis
# This function loads the package, sets up logging, and runs the analysis

#' Run the Chris analysis workflow
#'
#' This is the main entrypoint for the chris analysis. It loads configuration,
#' sets up logging, runs the analysis, and saves results to CSV.
#'
#' @param config_path Path to the chris config YAML file. Defaults to
#'   `inst/extdata/chris_config.yaml`.
#' @return Invisibly returns the analysis results data frame.
#' @export
run_chris_analysis <- function(config_path = NULL) {
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

  logger::log_info("Starting Chris analysis")

  # Run the analysis
  results <- chris_analyse(config)

  # Save results
  save_chris_results(
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

  logger::log_info("Chris analysis completed successfully")
  invisible(results)
}
