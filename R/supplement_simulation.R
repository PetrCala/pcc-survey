# Online supplement Table S9: calibrated simulation
#
# Every one of the 172 meta-analyses is re-simulated `replications` times under
# each publication-selection condition, keeping its design (number of
# estimates, their degrees of freedom and sample sizes). The decisions, so that
# the supplement can state them:
#
# * True effect: the meta-analysis's PET-PEESE estimate (the paper's
#   publication-bias-corrected benchmark), or UWLS1 when PET-PEESE is undefined,
#   clipped to [-0.9, 0.9]. Heterogeneity: the ML tau2 of its REz fit, on the
#   Fisher's z scale.
# * Data: each estimate's Fisher's z is drawn from N(atanh(mu), tau2 +
#   1 / (df - 1)) and the reported PCC is tanh(z); its t-value, S1 / S2 SEs,
#   PCC3 and Fisher's z SE then follow as in compute_derived_quantities(). The
#   target is mu, the PCC at the centre of the distribution of true effects.
#   Estimates with df <= 1 (105 in two meta-analyses) are left out of the
#   design, since 1 / (df - 1) is not a variance there.
# * Selection: a share of the estimates (0, 50% and 75% for the "none",
#   "moderate" and "strong" conditions) is reported only if positive and
#   significant at the 5% level (t > 1.96); the rest are reported whatever
#   their sign or significance. A selected estimate that fails is redrawn until
#   it passes, which is a draw from the same normal distribution truncated at
#   the significance threshold.
# * Each estimator's bias and RMSE are computed per meta-analysis over the
#   replications and then averaged across meta-analyses (each counts equally).
# * Each meta-analysis has its own seed (`seed` + its position in alphabetical
#   order), so the results do not depend on `cores`. Parallel runs use fresh
#   worker processes (a socket cluster) rather than forks: a fork can end up
#   copying the main process's memory, which is several GB after reading the
#   data.

#' Simulation design of one meta-analysis
#'
#' @param df [data.frame] One meta-analysis after [compute_derived_quantities()].
#' @return [list] `meta`, `dof` and `sample_size` of the estimates kept (df > 1),
#'   `mu` (true effect on the PCC scale), `tau2_z` (heterogeneity on the Fisher's
#'   z scale) and `n_dropped` (estimates with df <= 1).
#' @keywords internal
simulation_design <- function(df) {
  keep <- df$dof > 1
  mu <- fat_pet_peese(df, se = df$se_s1, alpha = 0.1)$est
  if (is.na(mu)) mu <- uwls(df, se = df$se_s1)$est
  list(
    meta = as.character(df$meta[1]),
    dof = df$dof[keep],
    sample_size = df$sample_size[keep],
    mu = min(0.9, max(-0.9, mu)),
    tau2_z = re_matrix(df$fishers_z, df$fishers_z_se^2)$tau2,
    n_dropped = sum(!keep)
  )
}

#' Draw simulated PCCs for one meta-analysis design
#'
#' @param design [list] From [simulation_design()].
#' @param n_reps [integer] Number of replications (columns).
#' @param selection_share [numeric] Share of estimates subject to selection for
#'   positive significance.
#' @return [matrix] Simulated PCCs, one replication per column.
#' @keywords internal
simulate_pccs <- function(design, n_reps, selection_share) {
  k <- length(design$dof)
  theta <- atanh(design$mu)
  s_tot <- sqrt(design$tau2_z + 1 / (design$dof - 1))
  # t > 1.96 <=> r > 1.96 / sqrt(1.96^2 + df) <=> z > atanh(that)
  z_crit <- atanh(1.96 / sqrt(1.96^2 + design$dof))
  # Floored so that a vanishing passing probability cannot give an infinite z
  p_sig <- pmax(1e-300, stats::pnorm((z_crit - theta) / s_tot, lower.tail = FALSE))

  selected <- matrix(stats::runif(k * n_reps) < selection_share, k, n_reps)
  u <- matrix(stats::runif(k * n_reps), k, n_reps)
  z_free <- theta + s_tot * stats::qnorm(u)
  z_sel <- theta + s_tot * stats::qnorm(u * p_sig, lower.tail = FALSE)
  z <- ifelse(selected, z_sel, z_free)
  r <- tanh(z)
  bound <- 1 - 1e-12
  r[r > bound] <- bound
  r[r < -bound] <- -bound
  r
}

#' Simulate one meta-analysis under every selection condition
#'
#' @param design [list] From [simulation_design()].
#' @param replications [integer] Number of replications per condition.
#' @param selection [numeric] Named selection shares.
#' @param seed [integer] Seed for this meta-analysis.
#' @param max_cells [numeric] Largest number of simulated estimates held in
#'   memory at once (replications are processed in blocks).
#' @return [data.frame] One row per condition and estimator: `condition`,
#'   `estimator`, `mean_est`, `bias`, `rmse` and `n_valid` (replications where
#'   the estimator is defined).
#' @keywords internal
simulate_meta_analysis <- function(design, replications, selection, seed, max_cells = 1e6) {
  set.seed(seed)
  k <- length(design$dof)
  block <- max(1L, as.integer(floor(max_cells / k)))
  dof <- design$dof
  fz_se <- 1 / sqrt(dof - 1)
  labels <- unname(supplement_estimators())

  rows <- lapply(names(selection), function(cond) {
    sums <- sumsq <- sum_est <- n_valid <- stats::setNames(numeric(length(labels)), labels)
    done <- 0L
    while (done < replications) {
      b <- min(block, replications - done)
      r <- simulate_pccs(design, b, selection[[cond]])
      t_value <- r * sqrt(dof) / sqrt(1 - r^2)
      est <- estimators_matrix(
        effect = r, se_s1 = sqrt((1 - r^2) / dof), se_s2 = (1 - r^2) / sqrt(dof),
        pcc3 = t_value / sqrt(t_value^2 + dof + 3), fishers_z = atanh(r),
        fishers_z_se = fz_se, sample_size = design$sample_size
      )
      err <- est - design$mu
      sums <- sums + colSums(err, na.rm = TRUE)
      sumsq <- sumsq + colSums(err^2, na.rm = TRUE)
      sum_est <- sum_est + colSums(est, na.rm = TRUE)
      n_valid <- n_valid + colSums(!is.na(est))
      done <- done + b
      # Free the block before the next one, so that memory stays bounded by
      # one block whatever garbage-collection threshold the process has.
      rm(r, t_value, est, err)
      gc(verbose = FALSE)
    }
    data.frame(
      condition = cond,
      estimator = labels,
      mean_est = ifelse(n_valid > 0, sum_est / n_valid, NA_real_),
      bias = ifelse(n_valid > 0, sums / n_valid, NA_real_),
      rmse = ifelse(n_valid > 0, sqrt(sumsq / n_valid), NA_real_),
      n_valid = as.integer(n_valid),
      row.names = NULL
    )
  })
  do.call(rbind, rows)
}

#' Table S9: calibrated simulation
#'
#' Re-simulates every meta-analysis `replications` times under each selection
#' condition, keeping its design and calibrating its true effect and
#' heterogeneity to the data (see the notes at the top of
#' R/supplement_simulation.R), applies the ten Table 1 estimators with
#' [estimators_matrix()], and reports each estimator's bias and RMSE averaged
#' across meta-analyses.
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param replications [integer] Replications per meta-analysis and condition
#'   (2,000 in the supplement; use a small number for tests).
#' @param selection [numeric] Named shares of estimates subject to selection for
#'   positive significance.
#' @param seed [integer] Base seed; meta-analysis j uses `seed + j`.
#' @param cores [integer] Number of worker processes (1 runs in this process).
#'   The results do not depend on it.
#' @param max_cells [numeric] Memory cap, see [simulate_meta_analysis()].
#' @return [data.frame] One row per condition and estimator: `condition`,
#'   `selection_share`, `estimator`, `true_mean` (mean true effect across
#'   meta-analyses), `mean_estimate`, `bias`, `rmse` (each averaged across
#'   meta-analyses), `n_metas` and `replications`.
#' @export
run_calibrated_simulation <- function(pcc_df, replications = 2000L,
                                      selection = c(none = 0, moderate = 0.5, strong = 0.75),
                                      seed = 2026L, cores = 1L, max_cells = 1e6) {
  stopifnot(!is.null(names(selection)), all(selection >= 0 & selection <= 1))
  designs <- lapply(split(pcc_df, pcc_df$meta), simulation_design)
  n_dropped <- sum(vapply(designs, `[[`, numeric(1), "n_dropped"))
  logger::log_info(paste(
    "Table S9: simulating", length(designs), "meta-analyses x", replications,
    "replications x", length(selection), "selection conditions",
    paste0("(", n_dropped, " estimates with df <= 1 left out of the design)")
  ))

  per_meta <- simulate_designs(designs, replications, selection, seed + seq_along(designs),
                               max_cells, cores)

  mu <- vapply(designs, `[[`, numeric(1), "mu")
  all_rows <- do.call(rbind, Map(function(d, m) cbind(d, mu = m), per_meta, mu))
  labels <- unname(supplement_estimators())
  out <- do.call(rbind, lapply(names(selection), function(cond) {
    do.call(rbind, lapply(labels, function(lab) {
      sub <- all_rows[all_rows$condition == cond & all_rows$estimator == lab & all_rows$n_valid > 0, ]
      data.frame(
        condition = cond,
        selection_share = selection[[cond]],
        estimator = lab,
        true_mean = mean(sub$mu),
        mean_estimate = mean(sub$mean_est),
        bias = mean(sub$bias),
        rmse = mean(sub$rmse),
        n_metas = nrow(sub),
        replications = as.integer(replications)
      )
    }))
  }))
  rownames(out) <- NULL
  out
}

# Set up a fresh worker process: the main process's library paths, then this
# package (the installed copy, or the source checkout when it was loaded with
# pkgload::load_all()).
start_simulation_worker <- function(package_path, lib_paths) {
  .libPaths(lib_paths)
  if (dir.exists(file.path(package_path, "Meta"))) {
    loadNamespace("pccsurvey", lib.loc = dirname(package_path))
  } else {
    pkgload::load_all(package_path, quiet = TRUE)
  }
  invisible(TRUE)
}

# simulate_meta_analysis() for every design, in this process or in `cores`
# worker processes
simulate_designs <- function(designs, replications, selection, seeds, max_cells, cores) {
  more <- list(replications = replications, selection = selection, max_cells = max_cells)
  if (cores <= 1L) {
    return(Map(simulate_meta_analysis, design = designs, seed = seeds, MoreArgs = more))
  }
  cl <- parallel::makePSOCKcluster(cores, rscript_args = "--vanilla")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterCall(cl, start_simulation_worker,
                        normalizePath(getNamespaceInfo("pccsurvey", "path")), .libPaths())
  parallel::clusterMap(cl, simulate_meta_analysis, design = designs, seed = seeds,
                       MoreArgs = more, .scheduling = "dynamic")
}
