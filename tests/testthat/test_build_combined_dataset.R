# Tests for build_combined_dataset

test_that("build_combined_dataset has the expected columns and SE source", {
  pcc_df <- data.frame(
    meta = c("A", "B"),
    study = c("s1", "s2"),
    effect = c(0.1, 0.2),
    se_s1 = c(0.01, 0.02),
    se_s2 = c(0.99, 0.98), # should be ignored - se comes from se_s1
    sample_size = c(50, 60)
  )

  result <- build_combined_dataset(pcc_df)

  expect_equal(colnames(result), c("idx", "meta", "study", "effect", "se", "sample_size"))
  expect_equal(result$se, c(0.01, 0.02))
  expect_equal(nrow(result), 2)
})

test_that("build_combined_dataset assigns idx in alphabetical meta order", {
  pcc_df <- data.frame(
    meta = c("Charlie", "Alpha", "Bravo", "Alpha"),
    study = paste0("s", 1:4),
    effect = c(0.3, 0.1, 0.2, 0.15),
    se_s1 = c(0.03, 0.01, 0.02, 0.015),
    sample_size = c(100, 50, 75, 60)
  )

  result <- build_combined_dataset(pcc_df)

  expect_equal(unique(result$idx[result$meta == "Alpha"]), 1L)
  expect_equal(unique(result$idx[result$meta == "Bravo"]), 2L)
  expect_equal(unique(result$idx[result$meta == "Charlie"]), 3L)
})

test_that("build_combined_dataset idx aligns with split()/summary ordering", {
  pcc_df <- data.frame(
    meta = c("zeta", "alpha", "mu", "alpha", "zeta"),
    study = paste0("s", 1:5),
    effect = c(0.2, 0.1, 0.3, 0.12, 0.22),
    se_s1 = c(0.02, 0.01, 0.03, 0.012, 0.022),
    sample_size = c(40, 50, 60, 70, 80)
  )

  result <- build_combined_dataset(pcc_df)

  # The per-MA summary orders metas via split(pcc_df, pcc_df$meta); idx must match
  summary_order <- names(split(pcc_df, pcc_df$meta))
  for (i in seq_along(summary_order)) {
    expect_equal(unique(result$idx[result$meta == summary_order[i]]), i)
  }
})

test_that("build_combined_dataset errors on missing columns", {
  expect_error(build_combined_dataset(data.frame(meta = "A", effect = 0.1)))
})
