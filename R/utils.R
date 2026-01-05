# Internal helper: resolve a path under inst/extdata both for installed packages
# and when running from the repo with `devtools::load_all()`.
pccsurvey_extdata <- function(...) {
  p <- system.file("extdata", ..., package = "pccsurvey")
  if (nzchar(p)) {
    return(p)
  }
  file.path("inst", "extdata", ...)
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
    stop(paste("File", full_path, "does not exist."))
  }

  message("Reading data from ", full_path)
  df <- readxl::read_excel(path = full_path)

  if (ncol(df) < 2) {
    stop("Data must have at least two columns.")
  }

  message("Data loaded successfully.")
  df
}

save_data <- function(results, file_name, output_dir = "data") {
  full_path <- file.path(output_dir, file_name)
  dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
  message(paste("Creating a data frame from the results list under path", full_path))

  out <- list()
  for (method in names(results)) {
    res <- results[[method]]
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
    stop(paste("Static config file does not exist:", path))
  }

  yaml::read_yaml(path)
}
