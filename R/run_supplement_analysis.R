# Entry point for the online supplement's tables (S2 to S10) and the Section 6
# paired t-tests and correlations

#' Save one supplement table to CSV, rounded for reporting
#'
#' @param table_df [data.frame] The table.
#' @param file_name [character] Output file name.
#' @param output_dir [character] Output directory (created if needed).
#' @param digits [integer] Decimal places for numeric columns (the supplement
#'   reports 3).
#' @return [character] The path written, invisibly.
#' @export
save_supplement_table <- function(table_df, file_name, output_dir = "output/supplement", digits = 3L) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  num <- vapply(table_df, is.double, logical(1))
  table_df[num] <- lapply(table_df[num], round, digits = digits)
  output_path <- file.path(output_dir, file_name)
  utils::write.csv(table_df, file = output_path, row.names = FALSE)
  logger::log_info(paste("Supplement table saved to", output_path))
  invisible(output_path)
}

#' Output file name of each supplement table
#'
#' @return [character] Named by table id.
#' @keywords internal
supplement_table_files <- function() {
  c(
    S2 = "table_s2_drop_top_studies.csv",
    S3 = "table_s3_one_estimate_per_study.csv",
    S4 = "table_s4_bootstrap_contrasts.csv",
    S5 = "table_s5_subsamples.csv",
    S6 = "table_s6_tau2_hs_variants.csv",
    S7 = "table_s7_fishers_z_variance.csv",
    S8 = "table_s8_gap_regressions.csv",
    S9 = "table_s9_simulation.csv",
    S10 = "table_s10_leave_one_out.csv",
    section6 = "section6_tests.csv"
  )
}

#' Run the online-supplement analysis
#'
#' Prepares the data exactly as [run_pcc_survey_analysis()] does, recomputes the
#' per-MA results behind Table 1 and the publication-selection measures, and
#' writes one CSV per supplement table to `output_dir`: Tables S2 to S10 and
#' the Section 6 paired t-tests and correlations (Table S1 is
#' `output/weight_concentration.csv`, written by [run_pcc_survey_analysis()]).
#' Numbers are rounded to 3 decimals (6 for the Table S8 regressions, whose
#' slopes are often below 0.001). All random steps use fixed seeds derived
#' from `seed`, so a rerun gives the same files. The full run (2,000 simulation
#' replications, 10,000 bootstrap resamples, 200 random draws) is slow; the
#' simulation dominates.
#'
#' @param config_path [character|NULL] Path to the config YAML file. Defaults to
#'   `inst/extdata/pcc_survey_config.yaml`.
#' @param output_dir [character|NULL] Output directory; defaults to
#'   `<config output_dir>/supplement`.
#' @param tables [character] Which tables to produce (ids as in
#'   `supplement_table_files()`: "S2", ..., "S10", "section6").
#' @param replications [integer] Simulation replications per meta-analysis and
#'   selection condition (Table S9).
#' @param n_boot [integer] Bootstrap resamples (Table S4).
#' @param n_draws [integer] Random one-estimate-per-study draws (Table S3).
#' @param seed [integer] Base seed.
#' @param cores [integer] Worker processes for the simulation (the results do not
#'   depend on it).
#' @param data_dir [character] Directory containing the data file.
#' @return [list] The tables, invisibly, named by table id.
#' @export
run_supplement_analysis <- function(config_path = NULL, output_dir = NULL,
                                    tables = names(supplement_table_files()),
                                    replications = 2000L, n_boot = 10000L, n_draws = 200L,
                                    seed = 2026L, cores = 1L, data_dir = "data") {
  if (is.null(config_path)) {
    config_path <- pccsurvey_extdata("pcc_survey_config.yaml")
  }
  if (!file.exists(config_path)) {
    cli::cli_abort("Config file does not exist: {.file {config_path}}")
  }
  config <- yaml::read_yaml(config_path)
  files <- supplement_table_files()
  unknown <- setdiff(tables, names(files))
  if (length(unknown) > 0) cli::cli_abort("Unknown supplement tables: {.val {unknown}}")
  if (is.null(output_dir)) output_dir <- file.path(config$output$output_dir, "supplement")

  setup_pcc_survey_logging(
    log_level = config$logging$log_level,
    log_to_console_only = config$logging$log_to_console_only,
    log_file_name = "pcc_supplement_analysis.log",
    log_flush_on_setup = config$logging$log_flush_on_setup
  )
  logger::log_info("Starting the online-supplement analysis")

  pcc_df <- prepare_pcc_data(config, data_dir = data_dir)
  results <- pcc_survey_analyse(config, data_dir = data_dir, pcc_df = pcc_df)
  re_method <- config$methods$re_method %||% "ML"

  psb_results <- NULL
  if ("S8" %in% tables) {
    psb_results <- do.call(rbind, lapply(split(pcc_df, pcc_df$meta), get_psb_metaflavours,
                                         re_method = re_method))
  }

  builders <- list(
    S2 = function() calculate_drop_top_studies(pcc_df, re_method = re_method),
    S3 = function() {
      calculate_study_dependence(pcc_df, results, n_draws = n_draws, seed = seed + 3L,
                                 re_method = re_method)
    },
    S4 = function() calculate_bootstrap_contrasts(results, n_boot = n_boot, seed = seed + 4L),
    S5 = function() calculate_subsample_means(results),
    S6 = function() calculate_tau2_hs_variants(pcc_df, results),
    S7 = function() calculate_fishers_z_variants(pcc_df, results),
    S8 = function() calculate_gap_regressions(results, psb_results),
    S9 = function() {
      run_calibrated_simulation(pcc_df, replications = replications, seed = seed + 9000L,
                                cores = cores)
    },
    S10 = function() calculate_leave_one_out(results),
    section6 = function() calculate_section6_tests(results)
  )

  out <- list()
  for (id in tables) {
    gc(verbose = FALSE)
    t0 <- Sys.time()
    logger::log_info(paste("Supplement: computing", id))
    out[[id]] <- builders[[id]]()
    # Several Table S8 slopes are below 0.001 (the indicators are on t-statistic
    # scales), so its coefficients keep 6 decimals.
    save_supplement_table(out[[id]], files[[id]], output_dir = output_dir,
                          digits = if (id == "S8") 6L else 3L)
    logger::log_info(paste("Supplement:", id, "done in",
                           format(round(difftime(Sys.time(), t0, units = "mins"), 2))))
  }

  logger::log_info("Online-supplement analysis completed successfully")
  invisible(out)
}
