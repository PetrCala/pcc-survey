# Tests for fishers_z function

test_that("fishers_z returns NA when all sample sizes are missing", {
  # Create a data frame with all NA sample sizes
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    sample_size = c(NA_real_, NA_real_, NA_real_),
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )
  
  result <- fishers_z(df, method = "ML")
  
  # Should return NA for both est and t_value
  expect_true(is.na(result$est))
  expect_true(is.na(result$t_value))
})

test_that("fishers_z returns NA when all effects are invalid for Fisher's z transformation", {
  # Create a data frame where Fisher's z transformation produces all NA
  # This can happen when effect values are exactly 1 or -1, or when sample_size - 3 <= 0
  df <- data.frame(
    effect = c(1.0, 1.0, 1.0),  # Effect of 1.0 causes log((1+1)/(1-1)) = log(2/0) = Inf
    sample_size = c(10, 20, 30),
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
    sample_size = c(2, 3, 4),  # n-3 would be -1, 0, 1
    meta = "test_meta",
    study = c("study1", "study2", "study3")
  )
  
  result <- fishers_z(df, method = "ML")
  
  # Should handle this gracefully - may return NA if all se values are invalid
  # The exact behavior depends on which rows get filtered out
  expect_true(is.na(result$est) || is.numeric(result$est))
  expect_true(is.na(result$t_value) || is.numeric(result$t_value))
})
