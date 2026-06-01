# pccsurvey NEWS

## Version 0.0.0.9000 (Development)

### Added (RSM revision)
- UWLSz estimator: UWLS on the Fisher's z scale (`uwls_fishers_z()`), included in
  the per-MA results and the estimator summary (Table 1).
- Per-MA heterogeneity statistics in the main results: `tau2` (from RE1/RE2),
  `gamma` (UWLS multiplicative variance, UWLS1/UWLS2), and `Q`/`I2` reported both
  from the RE fits and as derived from UWLS gamma, on both the S1 and S2 SEs.
- `build_combined_dataset()` writes a combined study-level dataset
  (`pcc_combined_dataset.csv`) with an `idx` aligned to the per-MA summary, for an
  aggregate FAT-PET panel model.
- `calculate_smallest_estimate_counts()` writes per-estimator "most conservative"
  (smallest signed) and negative counts (`smallest_estimate_counts.csv`).
- `flipped` column in the per-MA results flags meta-analyses whose effects were
  sign-aligned (median PCC negative), with the count logged.

### Added
- Initial release of pccsurvey package
- Support for PCC (Partial Correlation Coefficient) meta-analysis
- PCC Survey analysis workflow for processing large datasets
- PSB (Publication Selection Bias) analysis
- Multiple meta-analysis methods: RE, UWLS, UWLS+3, HSMA, Fisher's Z, WAIV2, MAIVE
- Data validation and availability checking functions
- Sample data for quick testing
- Session info capture for reproducibility
- renv support for dependency management
- GitHub Actions CI workflow for cross-platform testing

### Infrastructure
- renv lockfile for reproducible environments
- Comprehensive data validation with helpful error messages
- Enhanced documentation and replication guide
- Cross-platform compatibility testing
