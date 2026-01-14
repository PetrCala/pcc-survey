# Internal helper utilities (not exported)

assert_required_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing}}")
  }
  invisible(TRUE)
}

#' Get DOF or sample size, using the other as substitute when missing
#'
#' @param df [data.frame] The data frame with sample_size and dof columns
#' @param target [character] What to return: "dof" or "sample_size" (default: "dof")
#' @param offset [numeric] Offset to apply when returning DOF (default: 0)
#' @param use_dof_directly [logical] When target="sample_size" and sample_size is missing,
#'   use DOF directly as n (TRUE) or estimate n = dof + 7 (FALSE). Default: FALSE
#' @return [numeric] Vector of DOF or sample size values
#' @export
get_dof_or_sample_size <- function(df,
                                   target = c("dof", "sample_size"),
                                   offset = 0,
                                   use_dof_directly = FALSE) {
  stopifnot(is.numeric(offset), length(offset) == 1)
  target <- match.arg(target)
  assert_required_cols(df, c("dof", "sample_size"))

  if (target == "dof") {
    # Return DOF: use sample_size - 7 when DOF is missing
    dof <- df$dof
    missing_dof <- is.na(dof)
    if (any(missing_dof)) {
      dof[missing_dof] <- df$sample_size[missing_dof] - 7
    }
    return(dof - offset)
  } else {
    # Return sample_size: use DOF (directly or +7) when sample_size is missing
    n_ <- df$sample_size
    missing_n <- is.na(n_)
    if (any(missing_n)) {
      if (use_dof_directly) {
        # Use DOF directly as n (for HSMA, Fisher's z)
        n_[missing_n] <- df$dof[missing_n]
      } else {
        # Estimate sample size as dof + 7 (for summary statistics)
        n_[missing_n] <- df$dof[missing_n] + 7
      }
    }
    return(n_)
  }
}
