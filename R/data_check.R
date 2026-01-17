# Data validation and availability checking functions

#' Check if required data files are available
#'
#' Verifies that required data files exist in the expected locations.
#' Provides helpful error messages if files are missing.
#'
#' @param file_name [character] Name of the data file to check (default: "chris_data.xlsx")
#' @param data_dir [character] Directory containing the data file (default: "data")
#' @param sheet_name [character] Name of the sheet to check (default: "Main")
#' @return [logical] TRUE if file exists and is readable, FALSE otherwise
#' @export
check_data_availability <- function(file_name = "chris_data.xlsx", data_dir = "data", sheet_name = "Main") {
  file_path <- file.path(data_dir, file_name)

  if (!file.exists(file_path)) {
    cli::cli_abort(
      c(
        "Data file not found: {.file {file_path}}",
        "",
        "To obtain the data file:",
        "1. Download the source file (typically named something like 'Copy of all_datasets_combined End 2023.xlsx')",
        "2. Place it in the {.file {data_dir}/} folder",
        "3. Rename it to {.file {file_name}}",
        "4. Ensure it contains a sheet named {.val {sheet_name}}",
        "",
        "The file should contain over 200,000 rows of meta-analysis data."
      ),
      call = NULL
    )
  }

  # Check if file is readable
  if (!file.access(file_path, mode = 4) == 0) {
    cli::cli_abort(
      "Data file exists but is not readable: {.file {file_path}}",
      call = NULL
    )
  }

  # Try to verify the sheet exists
  if (requireNamespace("readxl", quietly = TRUE)) {
    tryCatch(
      {
        sheets <- readxl::excel_sheets(file_path)
        if (!sheet_name %in% sheets) {
          cli::cli_warn(
            c(
              "Sheet {.val {sheet_name}} not found in {.file {file_path}}",
              "Available sheets: {.val {sheets}}"
            )
          )
        }
      },
      error = function(e) {
        cli::cli_warn("Could not verify sheet existence: {.val {conditionMessage(e)}}")
      }
    )
  }

  logger::log_info(paste("Data file found:", file_path))
  invisible(TRUE)
}

#' Validate data structure
#'
#' Checks that a data frame has the expected columns and basic structure.
#'
#' @param df [data.frame] The data frame to validate
#' @param expected_cols [character] Vector of expected column names
#' @param min_rows [integer] Minimum number of rows expected (default: 1)
#' @return [logical] TRUE if validation passes
#' @export
validate_data_structure <- function(df, expected_cols, min_rows = 1) {
  if (!is.data.frame(df)) {
    cli::cli_abort("'df' must be a data frame.", call = NULL)
  }

  if (nrow(df) < min_rows) {
    cli::cli_abort(
      "Data frame has {nrow(df)} row(s), but at least {min_rows} row(s) are expected.",
      call = NULL
    )
  }

  missing_cols <- setdiff(expected_cols, colnames(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns: {.val {missing_cols}}",
        "Available columns: {.val {colnames(df)}}"
      ),
      call = NULL
    )
  }

  logger::log_debug(paste("Data structure validation passed:", nrow(df), "rows,", ncol(df), "columns"))
  invisible(TRUE)
}

#' Get file checksum for verification
#'
#' Calculates MD5 or SHA256 checksum of a file for verification purposes.
#'
#' @param file_path [character] Path to the file
#' @param algorithm [character] Hash algorithm: "md5" or "sha256" (default: "md5")
#' @return [character] Checksum string
#' @export
get_data_checksum <- function(file_path, algorithm = c("md5", "sha256")) {
  algorithm <- match.arg(algorithm)

  if (!file.exists(file_path)) {
    cli::cli_abort("File not found: {.file {file_path}}", call = NULL)
  }

  if (algorithm == "md5") {
    digest::digest(file_path, algo = "md5", file = TRUE)
  } else {
    digest::digest(file_path, algo = "sha256", file = TRUE)
  }
}

#' Check data file and provide detailed information
#'
#' Comprehensive check of data file availability, structure, and basic statistics.
#'
#' @param file_name [character] Name of the data file (default: "chris_data.xlsx")
#' @param data_dir [character] Directory containing the data file (default: "data")
#' @param sheet_name [character] Name of the sheet to check (default: "Main")
#' @param validate_structure [logical] Whether to validate structure after reading (default: FALSE)
#' @return [list] List with file information
#' @export
check_data_file <- function(file_name = "chris_data.xlsx", data_dir = "data", sheet_name = "Main", validate_structure = FALSE) {
  file_path <- file.path(data_dir, file_name)

  result <- list(
    file_path = file_path,
    exists = file.exists(file_path),
    readable = FALSE,
    file_size = NA,
    sheet_exists = NA,
    row_count = NA,
    col_count = NA
  )

  if (result$exists) {
    result$readable <- file.access(file_path, mode = 4) == 0
    result$file_size <- file.info(file_path)$size

    if (requireNamespace("readxl", quietly = TRUE)) {
      tryCatch(
        {
          sheets <- readxl::excel_sheets(file_path)
          result$sheet_exists <- sheet_name %in% sheets
          result$available_sheets <- sheets

          if (result$sheet_exists) {
            # Try to read a small sample to get dimensions
            tryCatch(
              {
                df_sample <- readxl::read_excel(file_path, sheet = sheet_name, n_max = 1000)
                result$col_count <- ncol(df_sample)
                # Note: row_count is approximate if file is large
                result$col_names <- colnames(df_sample)
              },
              error = function(e) {
                logger::log_warn(paste("Could not read sample data:", conditionMessage(e)))
              }
            )
          }
        },
        error = function(e) {
          logger::log_warn(paste("Could not read Excel file:", conditionMessage(e)))
        }
      )
    }
  }

  result
}
