validate_vector <- function(custom_vector, expected_vector) {
  if (length(custom_vector) != length(expected_vector)) {
    cli::cli_abort("The custom vector and the expected vector are not of the same length.")
  }

  if (!identical(class(custom_vector), class(expected_vector))) {
    cli::cli_abort("The custom vector and the expected vector are not of the same type.")
  }

  cmp <- all.equal(custom_vector, expected_vector)
  if (!isTRUE(cmp)) {
    cli::cli_abort("The custom vector and the expected vector are not equal: {cmp}")
  }

  message("Validation successful.")
}

validate_method <- function(custom_results, expected_results) {
  if (!is.list(custom_results) || !all(c("est", "t_value") %in% names(custom_results))) {
    warning("Custom results must be a list with keys 'est' and 't_value'.", call. = FALSE)
    message("") # Newline
    return(invisible(NULL))
  }
  if (!is.list(expected_results) || !all(c("est", "t_value") %in% names(expected_results))) {
    warning("Expected results must be a list with keys 'est' and 't_value'.", call. = FALSE)
    message("") # Newline
    return(invisible(NULL))
  }

  cmp <- all.equal(custom_results, expected_results)
  if (!isTRUE(cmp)) {
    warning(paste0("Validation mismatch: ", cmp), call. = FALSE)

    message("Custom results:")
    message(paste("Estimate:", custom_results$est))
    message(paste("T-value:", custom_results$t_value))
    message("Expected results:")
    message(paste("Estimate:", expected_results$est))
    message(paste("T-value:", expected_results$t_value))

    if (is.numeric(custom_results$est) && is.numeric(expected_results$est)) {
      message(paste("Delta est:", custom_results$est - expected_results$est))
    }
    if (is.numeric(custom_results$t_value) && is.numeric(expected_results$t_value)) {
      message(paste("Delta t_value:", custom_results$t_value - expected_results$t_value))
    }

    message("") # Newline
  } else {
    message("Validation successful.\n")
  }
}
