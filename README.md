# PCC Survey

[![R version](https://img.shields.io/badge/R%20version-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/)

A reproducible R package for a PCC (Partial Correlation Coefficient) methods survey. It processes a large meta-analytic dataset and produces summary statistics across multiple estimation methods.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Quick Start](#quick-start)
- [Outputs](#outputs)
- [Usage Reference](#usage-reference)
- [Configuration](#configuration)
- [Data](#data)
- [Data and code availability](#data-and-code-availability)
- [Reproducibility](#reproducibility)
- [Troubleshooting](#troubleshooting)
- [Development](#development)
- [Citation](#citation)

## Prerequisites

- **R** >= 4.1.0
- **A few GB of free RAM** — `data/pcc_survey_data.xlsx` is ~100MB but reading it needs headroom well beyond the file size; on a run with ~6GB free the read failed, and it succeeded with ~8GB free, so close other memory-heavy applications if the read errors out
- **make** is optional — all workflows can be run directly from R (see [Quick Start](#quick-start))

### System libraries (for compiling R packages from source)

#### macOS

```bash
xcode-select --install
```

#### Linux (Debian/Ubuntu)

```bash
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev
```

#### Linux (Fedora)

```bash
sudo dnf install -y libcurl-devel openssl-devel libxml2-devel
```

#### Windows

Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for your R version. Ensure `Rscript` is in your `PATH`.

## Quick Start

Open R in the project root directory and run:

```r
# 1. Install renv if needed, then restore all dependencies
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}
renv::restore()

# 2. Load the package (pkgload is a lightweight loader; install if needed)
if (!requireNamespace("pkgload", quietly = TRUE)) renv::install("pkgload")
pkgload::load_all(".")

# 3. Run the analysis
run_pcc_survey_analysis()
```

> `renv::restore()` installs the locked **runtime** dependencies. The developer
> tools (`devtools`, `roxygen2`, `testthat`, `lintr`) are `Suggests` and are not
> in the lockfile; the analysis only needs `pkgload` to load the package, so the
> snippet above is all you need to reproduce the results. Maintainers can install
> the dev tools with `make setup-dev`.

Results are written to `output/` (see [Outputs](#outputs)).

### Using make (optional)

If you have `make` installed, the equivalent one-liner is:

```bash
make replicate
```

This runs setup, data checks, and the analysis in sequence. Run `make help` to see all available targets.

## Outputs

| File | Description |
| --- | --- |
| `output/pcc_survey_results.csv` | Main results — statistics for each meta-analysis |
| `output/estimator_summary.csv` | Summary statistics across all estimators (Table 1, plus the Appendix Table 1 weight-concentration rows `W_top1` / `W_top3`) |
| `output/weight_concentration.csv` | Supplement Table S1: study-level top-1 / top-3 weight shares per estimator (mean, median, interquartile range) and the equal-study-weights benchmark |
| `output/smallest_estimate_counts.csv` | Per-estimator "most conservative" (smallest) and negative counts |
| `output/pcc_combined_dataset.csv` | Combined study-level dataset for the aggregate FAT-PET panel |
| `output/psb_results.csv` | Publication Selection Bias / ESS analysis results |
| `output/session_info.txt` | R session info for reproducibility |
| `logs/pcc_survey_analysis_YYYYMMDD_HHMMSS.log` | Timestamped execution log |

### Online supplement (`make supplement`)

`run_supplement_analysis()` (or `make supplement`) writes one CSV per online-supplement table to `output/supplement/`, rounded to 3 decimals (6 for Table S8, whose slopes are often below 0.001). It prepares the data exactly as the main analysis does (172 meta-analyses, 63,730 PCCs) and uses the same conventions; "mean" always means the mean across meta-analyses, each counting equally. Random steps use fixed seeds. The full run is slow (the Table S9 simulation dominates); `make supplement SUPPLEMENT_CORES=4` runs the simulation in 4 worker processes without changing the results, and `SUPPLEMENT_REPS` sets its number of replications.

| File | Description |
| --- | --- |
| `table_s2_drop_top_studies.csv` | Table S2: each estimator re-estimated after dropping its highest-weight primary study (top 1) and three highest-weight studies (top 3) |
| `table_s3_one_estimate_per_study.csv` | Table S3: one estimate per study (study medians; 200 random draws), weights divided by the study's number of estimates m, and three-level REML for RE1 and REz |
| `table_s4_bootstrap_contrasts.csv` | Table S4: percentile bootstrap (10,000 resamples of meta-analyses) of five paired estimator contrasts |
| `table_s5_subsamples.csv` | Table S5: estimator means and medians in subsamples (at least 10, 20 or 50 studies; at least 30 estimates; flipped / not flipped) |
| `table_s6_tau2_hs_variants.csv` | Table S6: RE1, RE2 and REz with REML and DerSimonian-Laird tau2; HS with df weights |
| `table_s7_fishers_z_variance.csv` | Table S7: REz and UWLSz with Fisher's z variance 1 / (df - 1) versus 1 / (n - 3) |
| `table_s8_gap_regressions.csv` | Table S8: estimator gaps regressed on ESS, PSST and the Egger t, with controls and HC3 standard errors |
| `table_s9_simulation.csv` | Table S9: calibrated simulation (172 meta-analyses x 2,000 replications x 3 selection conditions), bias and RMSE per estimator |
| `table_s10_leave_one_out.csv` | Table S10: leave-one-meta-analysis-out ranges of the headline statistics |
| `section6_tests.csv` | Section 6: paired t-tests (REz - RE1, UWLS1 - UWLSz, UWLSz - HS) and the HS / UWLSz and HS / REz correlations |

The definitions behind each table (which contrasts, controls, selection conditions and calibration) are documented in `R/supplement_tables.R` and `R/supplement_simulation.R`.

## Usage Reference

### R functions

```r
# Load the package
pkgload::load_all(".")

# Run PCC Survey analysis
results <- run_pcc_survey_analysis()

# Run PSB analysis
psb_results <- run_psb_analysis()

# Run the online-supplement tables (slow; writes output/supplement/)
supplement <- run_supplement_analysis()

# Check data availability
check_data_availability()
```

### make targets

```bash
make help       # Show all available targets
make setup      # Restore runtime dependencies via renv (+ pkgload loader)
make replicate  # Full workflow: setup + data check + analysis
make run        # Run PCC Survey analysis only
make run-psb    # Run PSB analysis only
make supplement # Online-supplement tables S2-S10 and Section 6 tests (slow)
make zip        # Bundle output CSVs, session_info, and latest log into a zip
make clean      # Remove generated output files
make doctor     # Print R and package version info

# Maintainer targets (require `make setup-dev` first):
make setup-dev  # Install devtools, roxygen2, testthat, lintr
make test       # Run testthat tests
make check      # Run R CMD check
make document   # Regenerate roxygen docs
make lint       # Run lintr (exits non-zero on lint errors)
```

## Configuration

Settings are in `inst/extdata/pcc_survey_config.yaml`. Key sections:

| Section | Controls |
| --- | --- |
| `data` | Input file name and sheet name |
| `cols` | Column mappings (Excel column names → internal names) |
| `analysis` | PCC identifier, index column |
| `methods` | Random-effects estimators (ML, REML, DL) |
| `cleaning` | DOF filling, t-value recalculation, inverse relationship conversion |
| `filtering` | Optionally restrict to a single meta-analysis |
| `logging` | Log level, file output settings |
| `caching` | Enable/disable result caching, cache age |
| `output` | Output file names and directories |

## Data

The analysis dataset is included in the repository at `data/pcc_survey_data.xlsx`. No external download is required.

The file must contain a sheet named `"Main"` with the following columns:

| Column | Description |
| --- | --- |
| `Title` | Study identifier |
| `Effect type` | Should include `"correlation"` for PCC studies |
| `Effect size` | Observed effect size |
| `Standard error` | Standard error of the effect |
| `t-stat` | t-statistic |
| `Sample size` | Sample size |
| `Filename` | Meta-analysis identifier |
| `Author 1` | First author name |
| `Year published` | Publication year |

If you need to substitute a different dataset, place it at `data/pcc_survey_data.xlsx` (or update the path in `pcc_survey_config.yaml`) and ensure it matches the schema above.

## Data and code availability

Everything needed to reproduce the paper's empirical results lives in this repository, in two parts:

- **Main analysis (this R package).** `data/pcc_survey_data.xlsx` together with the code in `R/` reproduces the per-meta-analysis results and Table 1 across the 172 PCC meta-analyses. Run it as in [Quick Start](#quick-start); results are written to `output/` (see [Outputs](#outputs)). `output/estimator_summary.csv` and `output/smallest_estimate_counts.csv` together contain every row of the manuscript's Table 1 (Mean, Median, Std Dev, Smallest, MSD-PP, and Flipped) plus the two Appendix Table 1 rows `W_top1` and `W_top3`: the mean, across the 172 meta-analyses, of the share of each estimator's total weight carried by its highest-weight primary study and by its three highest-weight primary studies, where a study's weight is the sum of the weights of its estimates. `output/weight_concentration.csv` is the online supplement's Table S1: the same shares with their median and interquartile range, plus the benchmark the shares would take if every study counted equally (see `R/weight_concentration.R` for the conventions). The per-meta-analysis shares are the `<method>_w_top1_study` / `<method>_w_top3_study` columns of `output/pcc_survey_results.csv`, and `n_studies` gives the number of primary studies; the estimate-level counterparts (`_estimate` columns) are kept for reference. Fisher's z uses the partial-correlation variance 1 / (df - 1), so REz and UWLSz leave out the few estimates with df <= 1; the run log reports how many. The remaining online-supplement tables (S2 to S10) and the Section 6 paired t-tests and correlations come from `run_supplement_analysis()` (`make supplement`), which writes them to `output/supplement/` (see [Online supplement](#online-supplement-make-supplement)).
- **Secondary statistics (`secondary-stats/`).** Companion analyses provided by co-author Tom Stanley and calculated outside the R pipeline (Stata / Statview / spreadsheets): the survey descriptive statistics, the PSST and aggregate Egger/FAT-PET publication-selection tests, the ICT illustration, and the combined study-level dataset (`PCCcombined.dta`). See [`secondary-stats/README.md`](secondary-stats/README.md) for a file-by-file guide. These are reference and provenance artifacts; they are **not** inputs to the R analysis above.

## Reproducibility

All package versions are locked via `renv`. The `renv.lock` file guarantees the same package versions are installed on any machine via `renv::restore()`.

Each analysis run also writes `output/session_info.txt` recording the R version, package versions, and platform.

## Troubleshooting

### Missing dependencies

```r
renv::restore()
# or, if renv itself fails to initialize:
renv::init()
renv::restore(prompt = FALSE)
```

### Data file not found

Verify the file is present:

```bash
ls -lh data/pcc_survey_data.xlsx
```

The file is committed directly to the repository (no Git LFS). If it is missing from your clone, the clone was likely incomplete — re-pull or re-clone, or copy the file back to `data/pcc_survey_data.xlsx`.

### Excel file reading errors

1. Verify the file is not corrupted
2. Confirm the sheet is named `"Main"` (matches `data.sheet` in `pcc_survey_config.yaml`)
3. Ensure `readxl` is installed: `install.packages("readxl")`

### Linux: missing system libraries

```bash
# Debian/Ubuntu
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

# Fedora
sudo dnf install -y libcurl-devel openssl-devel libxml2-devel
```

### Windows: Rscript not found

Add the R `bin` directory to your `PATH`, or install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) which includes it.

## Development

Developer tooling (`devtools`, `roxygen2`, `testthat`, `lintr`) is declared under
`Suggests` and is **not** part of the locked runtime environment. Install it once
with:

```bash
make setup-dev
```

### Running tests

```bash
make test
# or
Rscript -e "devtools::test()"
```

### Generating documentation

```bash
make document
# or
Rscript -e "devtools::document()"
```

### Code quality

```bash
make lint   # lintr
make check  # R CMD check
```

### Contributing

1. Fork the repository
2. Create a feature branch
3. Make changes and run `make test` and `make check`
4. Submit a pull request

## Citation

```r
citation("pccsurvey")
```

## License

MIT License — see [LICENSE](LICENSE) for details.
