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

#' Read and return the chris config YAML file as a list
read_chris_config <- function(path = NULL) {
  if (is.null(path)) {
    path <- pccsurvey_extdata("chris_config.yaml")
  }

  if (!file.exists(path)) {
    cli::cli_abort("Chris config file does not exist: {.file {path}}")
  }

  yaml::read_yaml(path)
}
