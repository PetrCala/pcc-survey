# Internal helper: resolve a path under inst/extdata both for installed packages
# and when running from the repo with `devtools::load_all()`.
pccsurvey_extdata <- function(...) {
  p <- system.file("extdata", ..., package = "pccsurvey")
  if (nzchar(p)) {
    p
  } else {
    file.path("inst", "extdata", ...)
  }
}

#' Read and return the PCC Survey config YAML file as a list
#'
#' @param path [character] Path to the config YAML file. If NULL, uses the default
#'   config file from `inst/extdata/pcc_survey_config.yaml`.
#' @return [list] The configuration list loaded from the YAML file
read_pcc_survey_config <- function(path = NULL) {
  if (is.null(path)) {
    path <- pccsurvey_extdata("pcc_survey_config.yaml")
  }

  if (!file.exists(path)) {
    cli::cli_abort("PCC Survey config file does not exist: {.file {path}}")
  }

  yaml::read_yaml(path)
}
