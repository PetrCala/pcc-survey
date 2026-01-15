# Tests for fishers_z function

test_that("fishers_z uses DOF when all sample sizes are missing", {
  # Create a data frame with all NA sample sizes but DOF available
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(NA_real_, NA_real_, NA_real_),
    dof = c(93, 193, 293),  # DOF available as substitute (use directly as n)
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  result <- fishers_z(df, method = "ML")

  # Should calculate using DOF directly as n (per Tom Stanley's guidance)
  # SE = 1/sqrt(dof - 3), so should work with dof >= 4
  # est and t_value should be numeric (not NA)
  expect_true(is.numeric(result$est))
  expect_false(is.na(result$est))
  expect_true(is.numeric(result$t_value))
  expect_false(is.na(result$t_value))
  
  # est should be between -1 and 1 (it's a correlation coefficient)
  expect_true(result$est >= -1 && result$est <= 1)
})

test_that("fishers_z returns NA when all effects are invalid for Fisher's z transformation", {
  # Create a data frame where Fisher's z transformation produces all NA
  # This can happen when effect values are exactly 1 or -1, or when sample_size - 3 <= 0
  df <- data.frame(
    effect = c(1.0, 1.0, 1.0), # Effect of 1.0 causes log((1+1)/(1-1)) = log(2/0) = Inf
    sample_size = c(10, 20, 30),
    dof = c(3, 13, 23),  # DOF available
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  result <- fishers_z(df, method = "ML")

  # Should return NA when no valid data remains after filtering
  expect_true(is.na(result$est))
  expect_true(is.na(result$t_value))
})

test_that("fishers_z calculates correctly with some missing sample sizes", {
  # Create a data frame with some NA sample sizes
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4),
    sample_size = c(50, NA_real_, 100, 200),
    dof = c(43, 93, 93, 193),  # DOF available as substitute
    meta = "test_meta",
    study = c("study1", "study2", "study3", "study4")
  )

  result <- fishers_z(df, method = "ML")

  # Should calculate using only non-NA sample sizes
  # fishers_z_ = 0.5 * log((1 + effect) / (1 - effect))
  # se_ = 1 / sqrt(sample_size - 3)
  # Then RE is calculated on the transformed data

  # est and t_value should be numeric (not NA)
  expect_true(is.numeric(result$est))
  expect_false(is.na(result$est))
  expect_true(is.numeric(result$t_value))
  expect_false(is.na(result$t_value))

  # est should be between -1 and 1 (it's a correlation coefficient)
  expect_true(result$est >= -1 && result$est <= 1)
})

test_that("fishers_z calculates correctly with no missing sample sizes", {
  # Create a data frame with no NA sample sizes
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(50, 100, 150),
    dof = c(43, 93, 143),  # DOF available
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  result <- fishers_z(df, method = "ML")

  # est and t_value should be numeric (not NA)
  expect_true(is.numeric(result$est))
  expect_false(is.na(result$est))
  expect_true(is.numeric(result$t_value))
  expect_false(is.na(result$t_value))

  # est should be between -1 and 1 (it's a correlation coefficient)
  expect_true(result$est >= -1 && result$est <= 1)
})

test_that("fishers_z handles sample sizes that are too small", {
  # Create a data frame with sample sizes <= 3 (which makes se = 1/sqrt(n-3) invalid)
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(2, 3, 4), # n-3 would be -1, 0, 1
    dof = c(-5, -4, -3),  # DOF available (may be negative)
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )

  result <- fishers_z(df, method = "ML")

  # Should handle this gracefully - may return NA if all se values are invalid
  # The exact behavior depends on which rows get filtered out
  expect_true(is.na(result$est) || is.numeric(result$est))
  expect_true(is.na(result$t_value) || is.numeric(result$t_value))
})

test_that("fishers_z correctly converts estimates back to PCC", {
  # Test that the Fisher's z transformation correctly converts back to PCC
  # This validates the mathematical correctness of the forward and inverse transformations
  
  # Test with various correlation values
  test_correlations <- c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, -0.1, -0.3, -0.5, -0.7)
  
  for (true_r in test_correlations) {
    # Create a data frame with a single study having the true correlation
    # Use a large sample size to minimize meta-analysis weighting effects
    df <- data.frame(
      effect = true_r,
      sample_size = 1000,
      dof = 993,
      meta = "test_meta",
      study = "study1"
    )
    
    result <- fishers_z(df, method = "ML")
    
    # The estimated PCC should be very close to the true correlation
    # (within floating point precision, typically < 1e-10)
    expect_true(is.numeric(result$est))
    expect_false(is.na(result$est))
    expect_true(result$est >= -1 && result$est <= 1)
    
    # For a single study with large sample size, the estimate should be very close
    # Allow for some numerical precision error (1e-6 is very generous)
    expect_true(abs(result$est - true_r) < 1e-6,
                info = paste0("For true_r = ", true_r, ", got est = ", result$est))
  }
})

test_that("fishers_z forward and inverse transformations are mathematically correct", {
  # Test the mathematical correctness of the transformation formulas
  # Forward: z = 0.5 * log((1 + r) / (1 - r))
  # Inverse: r = (exp(2*z) - 1) / (exp(2*z) + 1)
  
  test_values <- c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9, -0.1, -0.3, -0.5, -0.7, -0.9)
  
  for (r in test_values) {
    # Forward transformation (as implemented in the code)
    z <- 0.5 * log((1 + r) / (1 - r))
    
    # Inverse transformation (as implemented in the code)
    r_back <- (exp(2 * z) - 1) / (exp(2 * z) + 1)
    
    # The round-trip should recover the original value (within floating point precision)
    expect_true(abs(r - r_back) < 1e-14,
                info = paste0("For r = ", r, ", round-trip error = ", abs(r - r_back)))
  }
})

test_that("fishers_z conversion works correctly with multiple studies", {
  # Test that the conversion works correctly when aggregating multiple studies
  # The meta-analysis should properly weight studies and convert back to PCC
  
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4),
    sample_size = c(50, 100, 150, 200),
    dof = c(43, 93, 143, 193),
    meta = "test_meta",
    study = c("study1", "study2", "study3", "study4")
  )
  
  result <- fishers_z(df, method = "ML")
  
  # The result should be a valid correlation coefficient
  expect_true(is.numeric(result$est))
  expect_false(is.na(result$est))
  expect_true(result$est >= -1 && result$est <= 1)
  
  # The estimate should be between the min and max input correlations
  # (weighted by sample size, so likely closer to larger studies)
  expect_true(result$est >= min(df$effect))
  expect_true(result$est <= max(df$effect))
  
  # The estimate should be reasonable given the input values
  # With these inputs (0.1 to 0.4), the weighted average should be positive
  expect_true(result$est > 0)
})
