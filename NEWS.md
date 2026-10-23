# pccsurvey NEWS

## Version 0.0.0.9000 (Development)

### Added (RSM revision, round 2: online supplement)
- `run_supplement_analysis()` and `make supplement` produce the online
  supplement's Tables S2 to S10 and the Section 6 paired t-tests and
  correlations, one CSV per table in `output/supplement/` (3 decimals, fixed
  seeds): dropping the top-1 / top-3 highest-weight studies
  (`calculate_drop_top_studies()`, S2); study medians, random one-per-study
  draws, weights divided by m and three-level REML
  (`calculate_study_dependence()`, S3); a percentile bootstrap of paired
  contrasts (`calculate_bootstrap_contrasts()`, S4); subsamples
  (`calculate_subsample_means()`, S5); REML / DL tau2 and df-weighted HS
  (`calculate_tau2_hs_variants()`, S6); Fisher's z under 1 / (df - 1) versus
  1 / (n - 3) (`calculate_fishers_z_variants()`, S7); regressions of estimator
  gaps on ESS, PSST and the Egger t with HC3 standard errors
  (`calculate_gap_regressions()`, S8); a calibrated simulation with a
  `replications` argument (`run_calibrated_simulation()`, S9); leave-one-MA-out
  ranges (`calculate_leave_one_out()`, S10); and `calculate_section6_tests()`.
- `meta_estimates()` runs all Table 1 estimators on one meta-analysis with the
  supplement's variations (tau2 estimator, HS weights, Fisher's z variance), and
  internal column-wise estimators (`estimators_matrix()`) re-estimate many
  samples at once for the random draws and the simulation.
- `prepare_pcc_data()` holds the data preparation that `pcc_survey_analyse()`
  did inline, and `pcc_survey_analyse()` accepts an already prepared `pcc_df`.
  Table 1 and Table S1 are unchanged.

### Changed (RSM revision, round 2: co-author edits of October 2026)
- Fisher's z now uses the partial-correlation variance 1 / (df - 1) (Fisher's
  degrees-of-freedom adjustment, as in Stanley, Doucouliagos & Havranek 2024)
  instead of 1 / (n - 3). It is undefined for df <= 1, so REz and UWLSz leave out
  those estimates (all other estimators keep them); `compute_derived_quantities()`
  sets `fishers_z_se` to NA there and the run log reports how many estimates
  were left out. `fishers_z()` and `uwls_fishers_z()` skip those rows and return
  NA weights for them.
- Weight concentration is now reported at the primary-study level, as in the
  revised paper: the `W_top1` / `W_top3` rows of the estimator summary (the
  paper's Appendix Table 1) are the means across meta-analyses of the
  `<method>_w_top1_study` / `<method>_w_top3_study` shares, where a study's
  weight is the sum of its estimates' weights. This replaces the estimate-level
  medians of the previous round; the estimate-level columns stay in the per-MA
  results for reference.
- The `MSE_PP` row of the estimator summary is renamed `MSD_PP` (mean squared
  difference from PET-PEESE), matching the relabelled Table 1 row.

### Added (RSM revision, round 2: co-author edits of October 2026)
- `calculate_weight_shares()` and `save_weight_shares()`
  write `output/weight_concentration.csv`, the online supplement's Table S1: for
  each estimator, the mean, median and 25th / 75th percentiles across
  meta-analyses of the study-level top-1 and top-3 weight shares, plus an
  "Equal study weights" benchmark row (1 / k and min(3, k) / k).
- `n_studies` (number of distinct primary studies) in the per-MA results.

### Added (RSM revision)
- UWLSz estimator: UWLS on the Fisher's z scale (`uwls_fishers_z()`), included in
  the per-MA results and the estimator summary (Table 1).
- Per-MA heterogeneity statistics in the main results, all on the S2 (Eq. 3)
  sampling-error variance: `tau2_re2` (from RE2) and `gamma_uwls2`, `q_uwls2`,
  `i2_uwls2` (from UWLS2).
- `build_combined_dataset()` writes a combined study-level dataset
  (`pcc_combined_dataset.csv`) with an `idx` aligned to the per-MA summary, for an
  aggregate FAT-PET panel model.
- `calculate_smallest_estimate_counts()` writes per-estimator "most conservative"
  (smallest signed) and negative counts (`smallest_estimate_counts.csv`).
- `flipped` column in the per-MA results flags meta-analyses whose effects were
  sign-aligned (median PCC negative), with the count logged.
- Two Table 1 rows added to the estimator summary so it fully reproduces the
  manuscript's Table 1: `MSE_PP` (mean squared difference of each estimator from
  PET-PEESE; blank for PP itself) and `Flipped` (mean estimate over the
  sign-flipped meta-analyses).

### Added (RSM revision, round 2)
- Weight-concentration diagnostic requested by Reviewer 2 (point 3): every
  estimator now returns its normalised per-estimate `weights`, and
  `weight_concentration()` / `top_study_weight_share()` aggregate them by
  primary study (the `study` column) and report the share of total weight carried
  by the most heavily weighted estimate(s) and, alternatively, by the most
  influential primary study (paper). Per-MA columns `<method>_w_top1_estimate` /
  `<method>_w_top3_estimate` (estimate level) and `<method>_w_top1_study` /
  `<method>_w_top3_study` (paper level) are added to the results, and two Table
  S1 rows `W_top1` / `W_top3` (medians across the 172 MAs of the estimate-level
  shares) to the estimator summary. Decisions recorded in
  `R/weight_concentration.R`: estimate level in Table S1 (paper level kept in
  the results), fractions of total weight, median across MAs, PET-PEESE blank.

### Changed (RSM revision follow-up)
- Added the simple unweighted mean (`simple_mean()`, labelled "Mean") as the
  OLS comparison estimator requested by the reviewers; SE = sd(effect)/sqrt(k),
  reported explicitly as the `simple_mean_se` column. It appears in the per-MA
  results, the estimator summary (Table 1), and the smallest-estimate counts.
- Added per-MA conditional FAT-PET-PEESE (`fat_pet_peese()`) on the S1 SE: columns
  `petpeese`, `petpeese_se`, `petpeese_type` ("PET"/"PEESE") plus the FAT (Egger)
  coefficient `fat`/`fat_se`. One-sided PET decision at alpha = 0.1. Reported as a
  "publication-bias-corrected benchmark".
- Removed WAIV2 from the survey outputs (results, Table 1, smallest-estimate
  counts); it was exploratory. The `waiv2()` function is retained for future use.
- Removed the `row_mean` column (it averaged the estimators rather than being a
  simple unweighted mean); the simple mean above replaces it.
- Dropped the exploratory sample-size descriptives `quantile_1_n`, `quantile_3_n`
  and `ss_lt_50/100/200/400/1600/3200` from `pcc_sum_stats()`; `k_`, `avg_n` and
  `median_n` are kept for the simulation-vs-survey comparison.

### Changed (RSM revision follow-up 2)
- Reduced the per-MA heterogeneity statistics to the S2 (Eq. 3) "correct"
  sampling-error variants: `tau2_re2` from RE2 and `gamma_uwls2`, `q_uwls2`,
  `i2_uwls2` from UWLS2. The S1 variants and the RE-based `Q`/`I2` were dropped
  at the co-authors' request.
- Included PET-PEESE ("PP") as a column in the estimator summary (Table 1); it
  remains excluded from the smallest-estimate counts.
- Relabelled and reordered the Table 1 estimators to match the co-authors' table:
  Mean, RE1, RE2, UWLS1, UWLS2, UWLS+3, HS, REz, UWLSz, PP ("Simple mean" ->
  "Mean", "UWLS3" -> "UWLS+3", "HSMA" -> "HS", "Fisher's z" -> "REz").
- `make replicate` now also runs the PSB/ESS analysis, and `make zip` bundles
  `psb_results.csv` (ESS), `smallest_estimate_counts.csv` and
  `pcc_combined_dataset.csv` alongside the results, Table 1, and the PET-PEESE /
  FAT columns.

### Added
- Initial release of pccsurvey package
- Support for PCC (Partial Correlation Coefficient) meta-analysis
- PCC Survey analysis workflow for processing large datasets
- PSB (Publication Selection Bias) analysis
- Multiple meta-analysis estimators: simple mean, RE, UWLS, UWLS+3, HS, and Fisher's z (RE and UWLS)
- Data validation and availability checking functions
- Session info capture for reproducibility
- renv support for dependency management

### Infrastructure
- renv lockfile for reproducible environments
- Comprehensive data validation with helpful error messages
- Enhanced documentation and replication guide
- Cross-platform compatibility testing
