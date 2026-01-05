# Install required CRAN packages listed in ../packages.txt
# Intended usage (from repo root or via Makefile):
#   Rscript pcc-survey/scripts/install-packages.R

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0) {
    stop("Cannot determine script path (expected to be run via Rscript).")
  }
  script_path <- sub("^--file=", "", file_arg[1])
  dirname(normalizePath(script_path))
}

script_dir <- get_script_dir()
project_dir <- normalizePath(file.path(script_dir, ".."))
packages_file <- file.path(project_dir, "packages.txt")

if (!file.exists(packages_file)) {
  stop(paste0("packages.txt not found at: ", packages_file))
}

options(repos = c(CRAN = "https://cloud.r-project.org"))

raw_lines <- readLines(packages_file, warn = FALSE)
raw_lines <- trimws(raw_lines)
raw_lines <- raw_lines[nzchar(raw_lines)]
raw_lines <- raw_lines[!startsWith(raw_lines, "#")]

if (length(raw_lines) == 0) {
  message("No packages listed in packages.txt; nothing to install.")
  quit(save = "no", status = 0)
}

installed <- rownames(installed.packages())
missing <- raw_lines[!raw_lines %in% installed]

if (length(missing) == 0) {
  message("All required packages are already installed.")
  quit(save = "no", status = 0)
}

message("Installing missing packages:")
message(paste0("- ", missing, collapse = "\n"))

install.packages(missing, dependencies = TRUE, Ncpus = max(1L, parallel::detectCores() - 1L))

still_missing <- missing[!missing %in% rownames(installed.packages())]
if (length(still_missing) > 0) {
  stop(paste0("Some packages still appear missing after install: ", paste(still_missing, collapse = ", ")))
}

message("Done.")
