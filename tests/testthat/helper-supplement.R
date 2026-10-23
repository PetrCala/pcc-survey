# Shared fixture for the online-supplement tests: a few small meta-analyses
# with several estimates per study, passed through compute_derived_quantities()
# like the real data.
make_supplement_data <- function(n_meta = 4, seed = 1, one_per_study = FALSE) {
  set.seed(seed)
  rows <- list()
  for (j in seq_len(n_meta)) {
    n_studies <- 5 + j
    for (s in seq_len(n_studies)) {
      m <- if (one_per_study) 1L else sample(1:4, 1)
      dof <- sample(20:400, m, replace = TRUE)
      z <- rnorm(m, atanh(0.1 + 0.03 * j), sqrt(0.01 + 1 / (dof - 1)))
      r <- tanh(z)
      rows[[length(rows) + 1]] <- data.frame(
        meta = paste0("MA", j),
        study = paste0("MA", j, "-S", s),
        effect = r,
        dof = dof,
        sample_size = dof + sample(3:8, m, replace = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, rows)
  df$se <- sqrt((1 - df$effect^2) / df$dof)
  df$t_value <- df$effect / df$se
  compute_derived_quantities(df)
}

# Per-MA results in the shape of pcc_survey_analyse()'s output for the fixture
make_supplement_results <- function(pcc_df) {
  res <- do.call(rbind, lapply(split(pcc_df, pcc_df$meta), get_pcc_survey_metaflavours))
  res$flipped <- res$meta %in% c("MA1", "MA3")
  rownames(res) <- NULL
  res
}
