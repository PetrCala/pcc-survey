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
- [Reproducibility](#reproducibility)
- [Troubleshooting](#troubleshooting)
- [Development](#development)
- [Citation](#citation)

## Prerequisites

- **R** >= 4.1.0
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

# 2. Load the package
devtools::load_all(".")

# 3. Run the analysis
run_pcc_survey_analysis()
```

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
| `output/estimator_summary.csv` | Summary statistics across all estimators (Table 1) |
| `output/psb_results.csv` | Publication Selection Bias analysis results |
| `output/session_info.txt` | R session info for reproducibility |
| `logs/pcc_survey_analysis_YYYYMMDD_HHMMSS.log` | Timestamped execution log |

## Usage Reference

### R functions

```r
# Load the package
devtools::load_all(".")

# Run PCC Survey analysis
results <- run_pcc_survey_analysis()

# Run PSB analysis
psb_results <- run_psb_analysis()

# Check data availability
check_data_availability()
```

### make targets

```bash
make help       # Show all available targets
make setup      # Restore dependencies via renv
make replicate  # Full workflow: setup + data check + analysis
make run        # Run PCC Survey analysis only
make run-psb    # Run PSB analysis only
make test       # Run testthat tests
make check      # Run R CMD check
make document   # Regenerate roxygen docs
make lint       # Run lintr (exits non-zero on lint errors)
make clean      # Remove generated output files
make doctor     # Print R and package version info
make zip        # Bundle output CSVs, session_info, and latest log into a zip
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

If the file is missing from your clone, check that Git LFS (if used) is properly initialized, or re-copy the file to `data/pcc_survey_data.xlsx`.

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

## Author

Petr Čala — <cala.p@seznam.cz>
