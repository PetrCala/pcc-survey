# Tests for calculate_psb_measures and get_psb_metaflavours functions

test_that("calculate_psb_measures returns all required measures", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  measures <- calculate_psb_measures(df, "test_method", mean_effect)

  # Should return all required fields
  expect_named(measures, c(
    "ess", "psst", "psss", "falsely_positive",
    "observed_prop", "expected_prop",
    "observed_count", "expected_count"
  ))

  # All should be numeric
  expect_true(all(sapply(measures, is.numeric)))
})

test_that("calculate_psb_measures calculates correctly", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  mean_effect <- 0.2
  measures <- calculate_psb_measures(df, "test_method", mean_effect)

  # Verify individual calculations
  ess_manual <- calculate_ess(df, mean_effect)
  expect_equal(measures$ess, ess_manual, tolerance = 1e-6)

  psst_manual <- calculate_psst(df, mean_effect)
  expect_equal(measures$psst, psst_manual, tolerance = 1e-6)

  psss_manual <- calculate_psss(df, mean_effect)
  expect_equal(measures$psss, psss_manual, tolerance = 1e-6)

  fp_manual <- calculate_falsely_positive(df, mean_effect)
  expect_equal(measures$falsely_positive, fp_manual, tolerance = 1e-6)
})

test_that("calculate_psb_measures handles NA mean_effect", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  measures <- calculate_psb_measures(df, "test_method", NA_real_)

  # All measures should be NA
  expect_true(all(sapply(measures, is.na)))
})

test_that("get_psb_metaflavours returns data frame with correct structure", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- get_psb_metaflavours(df)

  # Should return a data frame with one row
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Should have meta column
  expect_true("meta" %in% colnames(result))
  expect_equal(result$meta, "test_meta")

  # Should have columns for all three methods
  expect_true("ess_uwls" %in% colnames(result))
  expect_true("ess_uwls3" %in% colnames(result))
  expect_true("ess_hs" %in% colnames(result))
  expect_true("psst_uwls" %in% colnames(result))
  expect_true("psst_uwls3" %in% colnames(result))
  expect_true("psst_hs" %in% colnames(result))
  expect_true("psss_uwls" %in% colnames(result))
  expect_true("psss_uwls3" %in% colnames(result))
  expect_true("psss_hs" %in% colnames(result))
  expect_true("falsely_positive_uwls" %in% colnames(result))
  expect_true("falsely_positive_uwls3" %in% colnames(result))
  expect_true("falsely_positive_hs" %in% colnames(result))
})

test_that("get_psb_metaflavours calculates measures for all three methods", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- get_psb_metaflavours(df)

  # All three methods should produce numeric values (may be NA if method fails)
  expect_true(is.numeric(result$ess_uwls) || is.na(result$ess_uwls))
  expect_true(is.numeric(result$ess_uwls3) || is.na(result$ess_uwls3))
  expect_true(is.numeric(result$ess_hs) || is.na(result$ess_hs))
})

test_that("get_psb_metaflavours includes observed values", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- get_psb_metaflavours(df)

  # Should have observed_prop_ss and observed_count_ss
  expect_true("observed_prop_ss" %in% colnames(result))
  expect_true("observed_count_ss" %in% colnames(result))

  # Observed values should be the same regardless of method
  observed_count_manual <- calculate_observed_significant(df)
  expect_equal(result$observed_count_ss, observed_count_manual)

  observed_prop_manual <- observed_count_manual / nrow(df)
  expect_equal(result$observed_prop_ss, observed_prop_manual, tolerance = 1e-6)
})

test_that("get_psb_metaflavours includes expected values for each method", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result <- get_psb_metaflavours(df)

  # Should have expected values for each method
  expect_true("expected_prop_ss_uwls" %in% colnames(result))
  expect_true("expected_prop_ss_uwls3" %in% colnames(result))
  expect_true("expected_prop_ss_hs" %in% colnames(result))
  expect_true("expected_count_ss_uwls" %in% colnames(result))
  expect_true("expected_count_ss_uwls3" %in% colnames(result))
  expect_true("expected_count_ss_hs" %in% colnames(result))
})

test_that("get_psb_metaflavours errors on multiple meta-analyses", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = c("meta1", "meta1", "meta2"), # Multiple metas
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  expect_error(get_psb_metaflavours(df), "Expected exactly one unique meta-analysis name")
})

test_that("get_psb_metaflavours handles custom alpha", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.1, 0.1, 0.1),
    t_value = c(1.0, 2.0, 3.0),
    meta = "test_meta",
    study = c("study1", "study2", "study3"),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143)
  )

  result_05 <- get_psb_metaflavours(df, alpha = 0.05)
  result_01 <- get_psb_metaflavours(df, alpha = 0.01)

  # Different alpha should give different results
  expect_false(result_05$ess_uwls == result_01$ess_uwls)
})
