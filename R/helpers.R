# Internal helper utilities (not exported)

assert_required_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing}}")
  }
  invisible(TRUE)
}

dof_or_sample_size <- function(df, offset = 0) {
  stopifnot(is.numeric(offset), length(offset) == 1)
  assert_required_cols(df, c("dof", "sample_size"))

  dof <- df$dof
  dof[is.na(dof)] <- df$sample_size[is.na(dof)] - 7

  dof - offset
}
