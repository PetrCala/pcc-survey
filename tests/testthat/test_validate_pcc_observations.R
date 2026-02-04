# Tests for validate_pcc_observations function

make_valid_df <- function(n = 5) {
  data.frame(
    effect = seq(0.1, 0.5, length.out = n),
    se = rep(0.1, n),
    t_value = seq(1.0, 3.0, length.out = n),
    dof = rep(50, n),
    sample_size = rep(57, n),
    meta = "test_meta",
    study = paste0("study", seq_len(n))
  )
}

test_that("validate_pcc_observations keeps all rows when all valid", {
  df <- make_valid_df(5)
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 5)
})

test_that("validate_pcc_observations drops rows with NA effect", {
  df <- make_valid_df(3)
  df$effect[2] <- NA_real_
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations drops rows with |effect| >= 1", {
  df <- make_valid_df(4)
  df$effect[1] <- 1.0
  df$effect[2] <- -1.0
  df$effect[3] <- 1.5
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 1)
})

test_that("validate_pcc_observations drops rows with NA se", {
  df <- make_valid_df(3)
  df$se[1] <- NA_real_
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations drops rows with se <= 0", {
  df <- make_valid_df(3)
  df$se[1] <- 0
  df$se[2] <- -0.1
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 1)
})

test_that("validate_pcc_observations drops rows with NA t_value", {
  df <- make_valid_df(3)
  df$t_value[1] <- NA_real_
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations drops rows with infinite t_value", {
  df <- make_valid_df(3)
  df$t_value[1] <- Inf
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations drops rows with dof <= 0", {
  df <- make_valid_df(3)
  df$dof[1] <- 0
  df$dof[2] <- -5
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 1)
})

test_that("validate_pcc_observations drops rows with sample_size <= 3", {
  df <- make_valid_df(3)
  df$sample_size[1] <- 3
  df$sample_size[2] <- 1
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 1)
})

test_that("validate_pcc_observations applies combined filter correctly", {
  df <- make_valid_df(5)
  df$effect[1] <- NA_real_      # row 1: bad effect
  df$se[2] <- -0.1              # row 2: bad se
  df$dof[3] <- 0                # row 3: bad dof
  # rows 4 and 5 remain valid
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations errors when all rows are invalid", {
  df <- make_valid_df(3)
  df$effect <- c(1.0, -1.0, NA_real_)
  expect_error(validate_pcc_observations(df), "No valid observations remain")
})

test_that("validate_pcc_observations errors on missing columns", {
  df <- data.frame(effect = c(0.1, 0.2))
  expect_error(validate_pcc_observations(df))
})

test_that("validate_pcc_observations drops rows with infinite effect", {
  df <- make_valid_df(3)
  df$effect[1] <- Inf
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations drops rows with infinite se", {
  df <- make_valid_df(3)
  df$se[1] <- Inf
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations drops rows with NA dof", {
  df <- make_valid_df(3)
  df$dof[1] <- NA_real_
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_pcc_observations drops rows with NA sample_size", {
  df <- make_valid_df(3)
  df$sample_size[1] <- NA_real_
  result <- validate_pcc_observations(df)
  expect_equal(nrow(result), 2)
})
