#' Run the PCC survey validation workflow.
#'
#' This is the main entrypoint for the repo. It loads configuration, reads the
#' default dataset, computes expected statistics, optionally validates custom
#' implementations, and writes `expected_stats.xlsx`.
#'
#' @param config_path Path to a YAML config file. Defaults to the shipped
#'   `inst/extdata/static.yaml`.
#' @param output_dir Directory where outputs should be written (default: `data/`
#'   under the current working directory).
#' @return Invisibly returns the expected results list.
#' @export
run_validation <- function(
    config_path = pccsurvey_extdata("static.yaml"),
    output_dir = "data") {
  compute_expected_results <- function(df, STATIC) { # nolint: object_name_linter.
    list(
      re = re(df, method = STATIC$re_method),
      uwls = uwls(df),
      uwls3 = uwls3(df),
      hsma = hsma(df),
      fishers_z = fishers_z(df, method = STATIC$re_method_fishers_z)
    )
  }

  maybe_validate_methods <- function(df, STATIC, expected_results) { # nolint: object_name_linter.
    validate_method_wrapper <- function(method_name, method_func, expected_result) {
      message(paste("Validating the", method_name, "method."))
      custom_result <- method_func(df)
      validate_method(custom_results = custom_result, expected_results = expected_result)
    }

    if (isTRUE(STATIC$validate$re)) {
      validate_method_wrapper("RE", custom_re, expected_results$re)
    }
    if (isTRUE(STATIC$validate$uwls)) {
      validate_method_wrapper("UWLS", custom_uwls, expected_results$uwls)
    }
    if (isTRUE(STATIC$validate$uwls3)) {
      validate_method_wrapper("UWLS+3", custom_uwls3, expected_results$uwls3)
    }
    if (isTRUE(STATIC$validate$hsma)) {
      validate_method_wrapper("HSMA", custom_hsma, expected_results$hsma)
    }
    if (isTRUE(STATIC$validate$fishers_z)) {
      validate_method_wrapper("Fisher's Z", custom_fishers_z, expected_results$fishers_z)
    }
  }

  STATIC <- read_static(config_path) # nolint: object_name_linter.
  df <- load_data(file_name = STATIC$file_name)

  # PCC validation
  if (isTRUE(STATIC$validate$pcc_var_1)) {
    message("Validating the PCC variance with an offset of 1.")
    custom_pcc_var_1 <- custom_pcc_variance(df, offset = 1)
    validate_vector(custom_vector = custom_pcc_var_1, expected_vector = df$pcc_var_1)
  }

  if (isTRUE(STATIC$validate$pcc_var_2)) {
    message("Validating the PCC variance with an offset of 2.")
    custom_pcc_var_2 <- custom_pcc_variance(df, offset = 2)
    validate_vector(custom_vector = custom_pcc_var_2, expected_vector = df$pcc_var_2)
  }

  # Methods validation
  message("Calculating the expected PCC statistics.")
  expected_results <- compute_expected_results(df, STATIC)
  maybe_validate_methods(df, STATIC, expected_results)

  # Write expected results
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  save_data(results = expected_results, file_name = STATIC$expected_stats_file_name, output_dir = output_dir)

  message("Done.")
  invisible(expected_results)
}
