# Utility functions for Chris analysis
# Extracted and simplified from meta-facilitator

#' Check whether an object is empty
#'
#' @param obj [any] The object to evaluate
#' @return [logical] TRUE if the object is empty, FALSE otherwise
#' @export
is_empty <- function(obj) {
  type_obj <- typeof(obj)

  switch(type_obj,
    logical = all(!obj),
    integer = length(obj) == 0 || all(is.na(obj)),
    double = length(obj) == 0 || all(is.na(obj)),
    character = length(obj) == 0 || any(obj == ""),
    list = length(obj) == 0,
    NULL = TRUE,
    data.frame = is.data.frame(obj) && nrow(obj) == 0,
    factor = length(obj) == 0 || all(is.na(obj)),
    {
      if (is.na(obj)) {
        TRUE
      } else {
        FALSE
      }
    }
  )
}

#' Convert a number to a percentage
#'
#' @param x [numeric] The number to convert
#' @return [character] The number as a percentage string
#' @export
to_perc <- function(x) {
  paste0(round(x * 100, 2), "%")
}

#' Find a string in a vector of strings using a substring
#'
#' @param vector_of_strings [character] The vector of strings to search
#' @param substring [character] The substring to search for
#' @return [character] The string that contains the substring
#' @export
find_string_using_substring <- function(vector_of_strings, substring) {
  if (!is.character(substring) || length(substring) != 1) {
    cli::cli_abort("The substring must be a single character string")
  }
  if (!is.vector(vector_of_strings) || !is.character(vector_of_strings)) {
    cli::cli_abort("The vector_of_strings must be a character vector")
  }
  match_bool <- grepl(substring, vector_of_strings)
  if (sum(match_bool) == 0) {
    cli::cli_abort("Could not find the substring {.val {substring}} in the vector of strings.")
  }
  if (sum(match_bool) > 1) {
    cli::cli_abort("Found multiple matches for the substring {.val {substring}} in the vector of strings.")
  }
  vector_of_strings[match_bool]
}

#' Calculate degrees of freedom using a t-value and a PCC
#'
#' @note The t_value and PCC can be provided either as a single numeric value, or as vectors of the same length.
#'
#' @param t_value [numeric] The t-value(s) to use for the calculation.
#' @param pcc [numeric] The partial correlation coefficient(s) to use for the calculation.
#' @return [numeric] The calculated degrees of freedom.
#' @export
calculate_dof <- function(t_value, pcc) {
  if (length(t_value) != length(pcc)) {
    cli::cli_abort("The length of 't_value' and 'pcc' must be the same.")
  }
  lhs <- t_value^2
  rhs <- (1 / (pcc^2)) - 1
  lhs * rhs
}

#' Log various information about the data frame
#'
#' @param df [data.frame] The data frame to log information about
#' @param colnames_to_analyse [character] The column names to analyse
#' @export
log_dataframe_info <- function(df, colnames_to_analyse = NULL) {
  logger::log_info(paste("The data frame has", nrow(df), "rows and", ncol(df), "columns"))

  if (!is.null(colnames_to_analyse)) {
    if (!is.character(colnames_to_analyse)) {
      cli::cli_abort("colnames_to_analyse must be a character vector")
    }
    for (colname in colnames_to_analyse) {
      plural_colname <- pluralize(colname)
      n_ <- length(unique(df[[colname]]))
      logger::log_info(paste0("Unique ", plural_colname, ": ", n_))
    }
  }
}

#' Pluralize a word
#'
#' @param word [character] The word to pluralize
#' @return [character] The pluralized word
pluralize <- function(word) {
  if (!is.character(word) || length(word) != 1) {
    cli::cli_abort("word must be a single character string")
  }
  if (grepl("[sxz]$", word) || grepl("[sc]h$", word)) {
    paste0(word, "es")
  } else if (grepl("[^aeiou]y$", word)) {
    sub("y$", "ies", word)
  } else {
    paste0(word, "s")
  }
}

#' Save the chris analysis results to CSV
#'
#' @param df [data.frame] The data frame to save
#' @param file_name [character] The output file name
#' @param output_dir [character] The output directory
#' @export
save_chris_results <- function(df, file_name = "chris_results.csv", output_dir = "data") {
  # Create the output folder
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Save the results
  output_path <- file.path(output_dir, file_name)
  logger::log_debug("Saving the results to ", output_path)
  utils::write.csv(df, file = output_path, row.names = FALSE)
  logger::log_info(paste("Results saved to", output_path))
}
