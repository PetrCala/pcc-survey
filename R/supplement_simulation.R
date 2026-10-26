# Online supplement Table S9: a simulation calibrated to the 172 meta-analyses
#
# As the supplement draft describes it: each meta-analysis keeps its design
# (the number of estimates and their degrees of freedom), its true mean effect
# is its PET-PEESE estimate truncated to [0, 0.5] and its heterogeneity its
# RE1 tau2; it is replicated `replications` times without selection and with
# two degrees of selection, in which statistically insignificant or negative
# estimates are reported with probability 0.5 or 0.25 while positive
# significant estimates are always reported. Bias and RMSE are computed per
# meta-analysis over the replications and averaged across meta-analyses (each
# counts equally).
#
# The draft does not state the following details, implemented as:
# * True effects: theta_i = mu + N(0, tau2) on the PCC scale (RE1's tau2 is on
#   that scale), kept inside (-0.999, 0.999).
# * Sampling: the reported PCC is tanh(z) with z ~ N(atanh(theta_i),
#   1 / (df - 1)), the paper's Fisher's z variance. Estimates with df <= 1 (105
#   in two meta-analyses) are left out of the design, as that variance is
#   undefined there.
# * Significance: "positive significant" means t > 1.96 (5% level, two-sided),
#   with t computed from the PCC and df as in Eq. 1.
# * An estimate that is not reported is replaced by a new draw (true effect and
#   sampling error), so every simulated meta-analysis keeps its number of
#   estimates.
# * HS weights are the meta-analysis's sample sizes (df + 7 where missing).
# * The Monte Carlo standard error of an averaged bias is sqrt(sum_j se_j^2) / J
#   over the J meta-analyses, se_j being meta-analysis j's own.
#
# Each meta-analysis has its own seed (`seed` + its position in alphabetical
# order), so the results do not depend on `cores`. Parallel runs use fresh
# worker processes (a socket cluster) rather than forks: a fork can end up
# copying the main process's memory, which is several GB after reading the
# data.

#' Simulation design of one meta-analysis
#'
#' @param df [data.frame] One meta-analysis after [compute_derived_quantities()].
#' @param true_effect [character] "petpeese" (the draft's calibration) or
#'   "uwls1" (the alternative its note mentions).
#' @return [list] `meta`, `dof` and `sample_size` of the estimates kept (df > 1),
#'   `mu` (true mean effect, truncated to [0, 0.5]), `tau2` (RE1's ML tau2) and
#'   `n_dropped` (estimates with df <= 1).
#' @keywords internal
simulation_design <- function(df, true_effect = c("petpeese", "uwls1")) {
  true_effect <- match.arg(true_effect)
  keep <- df$dof > 1
  mu <- if (true_effect == "petpeese") fat_pet_peese(df, se = df$se_s1, alpha = 0.1)$est else NA_real_
  if (is.na(mu)) mu <- uwls(df, se = df$se_s1)$est
  list(
    meta = as.character(df$meta[1]),
    dof = df$dof[keep],
    sample_size = df$sample_size[keep],
    mu = min(0.5, max(0, mu)),
    tau2 = re_matrix(df$effect, df$se_s1^2)$tau2,
    n_dropped = sum(!keep)
  )
}

#' Draw simulated PCCs for one meta-analysis design
#'
#' @param design [list] From [simulation_design()].
#' @param n_reps [integer] Number of replications (columns).
#' @param report_prob [numeric] Probability that a statistically insignificant
#'   or negative estimate is reported (1 means no selection); positive
#'   significant estimates (t > 1.96) are always reported, and an unreported
#'   estimate is replaced by a new draw.
#' @return [matrix] Simulated PCCs, one replication per column.
#' @keywords internal
simulate_pccs <- function(design, n_reps, report_prob) {
  k <- length(design$dof)
  out <- matrix(NA_real_, k, n_reps)
  todo <- seq_len(k * n_reps)
  tau <- sqrt(design$tau2)
  bound <- 1 - 1e-12
  while (length(todo) > 0) {
    dof <- design$dof[(todo - 1L) %% k + 1L]
    theta <- design$mu + tau * stats::rnorm(length(todo))
    theta <- pmin(0.999, pmax(-0.999, theta))
    r <- tanh(atanh(theta) + stats::rnorm(length(todo)) / sqrt(dof - 1))
    r <- pmin(bound, pmax(-bound, r))
    t_value <- r * sqrt(dof) / sqrt(1 - r^2)
    reported <- t_value > 1.96 | stats::runif(length(todo)) < report_prob
    out[todo[reported]] <- r[reported]
    todo <- todo[!reported]
  }
  out
}

#' Simulate one meta-analysis under every selection condition
#'
#' @param design [list] From [simulation_design()].
#' @param replications [integer] Number of replications per condition.
#' @param report_probs [numeric] Named reporting probabilities, one per
#'   condition (see [simulate_pccs()]).
#' @param seed [integer] Seed for this meta-analysis.
#' @param max_cells [numeric] Largest number of simulated estimates held in
#'   memory at once (replications are processed in blocks).
#' @return [data.frame] One row per condition and estimator: `condition`,
#'   `estimator`, `mean_est`, `bias`, `rmse`, `var_err` (variance of the error
#'   across replications) and `n_valid` (replications where the estimator is
#'   defined).
#' @keywords internal
simulate_meta_analysis <- function(design, replications, report_probs, seed, max_cells = 1e6) {
  set.seed(seed)
  k <- length(design$dof)
  block <- max(1L, as.integer(floor(max_cells / k)))
  dof <- design$dof
  fz_se <- 1 / sqrt(dof - 1)
  labels <- unname(supplement_estimators())

  rows <- lapply(names(report_probs), function(cond) {
    sums <- sumsq <- sum_est <- n_valid <- stats::setNames(numeric(length(labels)), labels)
    done <- 0L
    while (done < replications) {
      b <- min(block, replications - done)
      r <- simulate_pccs(design, b, report_probs[[cond]])
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
    bias <- ifelse(n_valid > 0, sums / n_valid, NA_real_)
    data.frame(
      condition = cond,
      estimator = labels,
      mean_est = ifelse(n_valid > 0, sum_est / n_valid, NA_real_),
      bias = bias,
      rmse = ifelse(n_valid > 0, sqrt(sumsq / n_valid), NA_real_),
      var_err = ifelse(n_valid > 1, (sumsq - n_valid * bias^2) / (n_valid - 1), NA_real_),
      n_valid = as.integer(n_valid),
      row.names = NULL
    )
  })
  do.call(rbind, rows)
}

#' Table S9: bias and RMSE in the calibrated simulation
#'
#' Re-simulates every meta-analysis `replications` times under each selection
#' condition, keeping its design and calibrating its true effect and
#' heterogeneity to the data (see the notes at the top of
#' R/supplement_simulation.R), applies the Table 1 estimators with
#' [estimators_matrix()], and reports each estimator's bias and RMSE averaged
#' across meta-analyses.
#'
#' @param pcc_df [data.frame] Estimate-level data after
#'   [compute_derived_quantities()].
#' @param replications [integer] Replications per meta-analysis and condition
#'   (2,000 in the supplement; use a small number for tests).
#' @param report_probs [numeric] Named probabilities with which statistically
#'   insignificant or negative estimates are reported, one per condition.
#' @param true_effect [character] Calibration of the true mean effect,
#'   "petpeese" or "uwls1" (see [simulation_design()]).
#' @param seed [integer] Base seed; meta-analysis j uses `seed + j`.
#' @param cores [integer] Number of worker processes (1 runs in this process).
#'   The results do not depend on it.
#' @param max_cells [numeric] Memory cap, see [simulate_meta_analysis()].
#' @return [data.frame] One row per condition and estimator (Mean, RE1, UWLS1,
#'   UWLS+3, HS, REz, UWLSz, PP): `condition`, `report_prob`, `estimator`,
#'   `bias`, `rmse` (each averaged across meta-analyses), `mc_se` (Monte Carlo
#'   standard error of the bias), `true_mean` and `mean_estimate` (averaged
#'   across meta-analyses), `n_metas` and `replications`.
#' @export
run_calibrated_simulation <- function(pcc_df, replications = 2000L,
                                      report_probs = c(none = 1, moderate = 0.5, strong = 0.25),
                                      true_effect = c("petpeese", "uwls1"),
                                      seed = 2026L, cores = 1L, max_cells = 1e6) {
  true_effect <- match.arg(true_effect)
  stopifnot(!is.null(names(report_probs)), all(report_probs > 0 & report_probs <= 1))
  designs <- lapply(split(pcc_df, pcc_df$meta), simulation_design, true_effect = true_effect)
  n_dropped <- sum(vapply(designs, `[[`, numeric(1), "n_dropped"))
  logger::log_info(paste(
    "Table S9: simulating", length(designs), "meta-analyses x", replications,
    "replications x", length(report_probs), "selection conditions",
    paste0("(", n_dropped, " estimates with df <= 1 left out of the design)")
  ))

  per_meta <- simulate_designs(designs, replications, report_probs, seed + seq_along(designs),
                               max_cells, cores)

  mu <- vapply(designs, `[[`, numeric(1), "mu")
  all_rows <- do.call(rbind, Map(function(d, m) cbind(d, mu = m), per_meta, mu))
  out <- do.call(rbind, lapply(names(report_probs), function(cond) {
    do.call(rbind, lapply(supplement_table_estimators(), function(lab) {
      sub <- all_rows[all_rows$condition == cond & all_rows$estimator == lab & all_rows$n_valid > 0, ]
      data.frame(
        condition = cond,
        report_prob = report_probs[[cond]],
        estimator = lab,
        bias = mean(sub$bias),
        rmse = mean(sub$rmse),
        mc_se = sqrt(sum(sub$var_err / sub$n_valid)) / nrow(sub),
        true_mean = mean(sub$mu),
        mean_estimate = mean(sub$mean_est),
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
simulate_designs <- function(designs, replications, report_probs, seeds, max_cells, cores) {
  more <- list(replications = replications, report_probs = report_probs, max_cells = max_cells)
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
