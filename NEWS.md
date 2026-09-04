# pccsurvey NEWS

## Version 0.0.0.9000 (Development)

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
