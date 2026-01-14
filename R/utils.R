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

#' Load a single data frame from the shipped extdata (or repo `data/` fallback)
load_data <- function(file_name) {
  # Prefer shipped package data when the name matches.
  shipped <- pccsurvey_extdata(file_name)
  if (file.exists(shipped)) {
    full_path <- shipped
  } else {
    full_path <- file.path("data", file_name)
  }

  if (!file.exists(full_path)) {
    cli::cli_abort("File does not exist: {.file {full_path}}")
  }

  message("Reading data from ", full_path)
  df <- readxl::read_excel(path = full_path)

  if (ncol(df) < 2) {
    cli::cli_abort("Data must have at least two columns.")
  }

  message("Data loaded successfully.")
  df
}

save_data <- function(results, file_name, output_dir = "data") {
  full_path <- file.path(output_dir, file_name)
  dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
  message(paste("Creating a data frame from the results list under path", full_path))

  out <- list()
  for (method in sort(names(results))) {
    res <- results[[method]]
    if (!is.list(res) || !all(c("est", "t_value") %in% names(res))) {
      next
    }
    out[[paste0(method, "_est")]] <- res$est
    out[[paste0(method, "_t_value")]] <- res$t_value
  }

  # Some elements might be numeric(0) here - replace with NA to allow for data frame conversion
  out <- lapply(out, function(x) if (length(x) == 0) NA else x)

  writexl::write_xlsx(as.data.frame(out), path = full_path)
}

#' Read and return the static YAML file as a list
read_static <- function(path = NULL) {
  if (is.null(path)) {
    path <- pccsurvey_extdata("static.yaml")
  }

  if (!file.exists(path)) {
    cli::cli_abort("Static config file does not exist: {.file {path}}")
  }

  yaml::read_yaml(path)
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
