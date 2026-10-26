# Tests for the calibrated simulation (Table S9) and the supplement's CSV writer

test_that("simulation_design keeps df > 1 and calibrates to PET-PEESE and RE1's tau2", {
  df <- make_supplement_data(n_meta = 1, seed = 31)
  df$dof[2] <- 0.5
  df$fishers_z_se[2] <- NA
  d <- simulation_design(df)
  expect_equal(d$n_dropped, 1)
  expect_equal(length(d$dof), nrow(df) - 1)
  pp <- fat_pet_peese(df, se = df$se_s1)$est
  expect_equal(d$mu, min(0.5, max(0, pp)))
  mf <- metafor::rma(yi = df$effect, sei = df$se_s1, method = "ML",
                     control = list(threshold = 1e-12, maxiter = 10000))
  expect_equal(d$tau2, as.numeric(mf$tau2), tolerance = 1e-7)
  expect_equal(simulation_design(df, "uwls1")$mu, min(0.5, max(0, uwls(df, se = df$se_s1)$est)))
})

test_that("simulate_pccs reports insignificant or negative estimates with the stated probability", {
  design <- list(dof = c(20, 50, 100, 400), sample_size = c(25, 55, 105, 405), mu = 0.05,
                 tau2 = 0.004)
  set.seed(1)
  # Reporting probability close to 0: only positive significant estimates
  r <- simulate_pccs(design, 300, report_prob = 1e-9)
  t_value <- r * sqrt(design$dof) / sqrt(1 - r^2)
  expect_true(all(t_value > 1.96))
  # No selection: z = atanh(theta) + N(0, 1 / (df - 1)), theta = mu + N(0, tau2)
  r0 <- simulate_pccs(design, 20000, report_prob = 1)
  expect_equal(mean(r0), 0.05, tolerance = 0.02)
  expect_equal(apply(atanh(r0), 1, var), 0.004 + 1 / (design$dof - 1), tolerance = 0.06)
  # With selection, the share of insignificant estimates falls
  r_half <- simulate_pccs(design, 20000, report_prob = 0.5)
  sig <- function(r) mean(r * sqrt(design$dof) / sqrt(1 - r^2) > 1.96)
  p0 <- sig(r0)
  expect_equal(sig(r_half), p0 / (p0 + 0.5 * (1 - p0)), tolerance = 0.02)
})

test_that("run_calibrated_simulation is reproducible and shows selection bias", {
  df <- make_supplement_data(n_meta = 2, seed = 32)
  sim <- run_calibrated_simulation(df, replications = 30, seed = 5)
  expect_equal(nrow(sim), 3 * 8)
  expect_equal(unique(sim$condition), c("none", "moderate", "strong"))
  expect_equal(unique(sim$report_prob), c(1, 0.5, 0.25))
  expect_equal(unique(sim$estimator), supplement_table_estimators())
  expect_true(all(sim$n_metas == 2 & sim$replications == 30))
  expect_true(all(is.finite(sim$bias) & sim$rmse >= abs(sim$bias) & sim$mc_se > 0))
  expect_equal(sim$bias, sim$mean_estimate - sim$true_mean, tolerance = 1e-12)
  expect_identical(sim, run_calibrated_simulation(df, replications = 30, seed = 5))
  # Selection for significance pushes the simple mean up
  mean_bias <- sim$bias[sim$estimator == "Mean"]
  expect_lt(mean_bias[1], mean_bias[3])
})

test_that("run_calibrated_simulation results do not depend on the number of workers", {
  skip_on_cran()
  df <- make_supplement_data(n_meta = 2, seed = 33)
  probs <- c(none = 1, strong = 0.25)
  a <- run_calibrated_simulation(df, replications = 12, seed = 2, report_probs = probs)
  b <- run_calibrated_simulation(df, replications = 12, seed = 2, report_probs = probs, cores = 2L)
  expect_identical(a, b)
})

test_that("save_supplement_table rounds numeric columns to 3 decimals", {
  dir <- tempfile("supp")
  tab <- data.frame(estimator = c("RE1", "HS"), mean = c(0.123456, 0.2), n_metas = c(172L, 170L),
                    p_value = c(0.000641, 0.5))
  path <- save_supplement_table(tab, "t.csv", output_dir = dir)
  back <- utils::read.csv(path)
  expect_equal(back$mean, c(0.123, 0.2))
  expect_equal(back$p_value, c(0.000641, 0.5))
  expect_equal(back$n_metas, c(172L, 170L))
  expect_equal(back$estimator, c("RE1", "HS"))
  unlink(dir, recursive = TRUE)
})

test_that("run_supplement_analysis rejects unknown table ids", {
  expect_error(run_supplement_analysis(tables = "S99"), "Unknown supplement tables")
})
