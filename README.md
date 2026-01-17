# PCC Survey (`pccsurvey`)

[![R-CMD-check](https://github.com/USERNAME/pcc-survey/workflows/R-CMD-check/badge.svg)](https://github.com/USERNAME/pcc-survey/actions)
[![R version](https://img.shields.io/badge/R%20version-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository provides a **reproducible R package** for meta-analysis of PCC (Partial Correlation Coefficient) studies. It includes the Chris analysis workflow for processing large datasets and calculating various meta-analysis statistics using multiple methods.

## Table of Contents

- [Quick Start](#quick-start)
- [Installation](#installation)
- [Prerequisites](#prerequisites)
- [Usage](#usage)
- [Data Requirements](#data-requirements)
- [Configuration](#configuration)
- [Outputs](#outputs)
- [Reproducibility](#reproducibility)
- [Troubleshooting](#troubleshooting)
- [Development](#development)

## Quick Start

### One-Command Replication

For a complete replication workflow:

```bash
make replicate
```

This will:
1. Restore all dependencies using `renv`
2. Check data availability
3. Run the complete analysis
4. Generate all outputs

### Step-by-Step

```bash
# 1. Restore dependencies
make setup

# 2. Run analysis
make run
```

**Note**: You need the `chris_data.xlsx` file in the `data/` folder (see [Data Requirements](#data-requirements)).

### Using Sample Data

For quick testing without the full dataset:

```r
devtools::load_all()
run_chris_analysis(use_sample = TRUE)
```

## Installation

### From Source

```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the package
devtools::install_github("USERNAME/pcc-survey")
```

### Using renv (Recommended for Reproducibility)

The package uses `renv` for dependency management. After cloning:

```bash
# Restore the exact environment
Rscript -e "renv::restore()"
```

## Prerequisites

### System Requirements

- **R**: Version >= 4.1.0
- **make**: For using Makefile commands (optional but recommended)

### R Package Dependencies

All dependencies are managed via `renv` and listed in `renv.lock`. Key packages:
- `MAIVE` (from CRAN)
- `metafor`
- `data.table`
- `readxl`
- And others (see `DESCRIPTION`)

### OS-Specific Setup

#### macOS

```bash
# Install R from CRAN
# Install Command Line Tools (provides make)
xcode-select --install
```

#### Linux (Debian/Ubuntu)

```bash
sudo apt-get update
sudo apt-get install -y r-base make libcurl4-openssl-dev libssl-dev libxml2-dev
```

#### Linux (Fedora)

```bash
sudo dnf install -y R make libcurl-devel openssl-devel libxml2-devel
```

#### Windows

Three options (pick one):

1. **WSL2 (Recommended)**: Install Ubuntu in WSL2, then install R + make
2. **Git Bash**: Install Git for Windows and MSYS2 for make
3. **Rtools**: Install Rtools for your R version (includes make)

Ensure `Rscript` is in your `PATH`.

## Usage

### Makefile Commands

```bash
make help          # Show all available commands
make setup         # Restore dependencies via renv
make replicate     # Complete replication workflow
make run           # Run Chris analysis
make run-psb       # Run PSB analysis
make test          # Run package tests
make check         # Run R CMD check
make document      # Generate roxygen documentation
make clean         # Remove generated outputs
make doctor        # Print environment information
```

### R Functions

```r
# Load the package
library(pccsurvey)

# Run Chris analysis
results <- run_chris_analysis()

# Run with sample data
results <- run_chris_analysis(use_sample = TRUE)

# Run PSB analysis
psb_results <- run_psb_analysis()

# Check data availability
check_data_availability()
```

## Data Requirements

### Full Dataset

The analysis requires `chris_data.xlsx` in the `data/` folder:

1. **Obtain the source file**: Typically named `Copy of all_datasets_combined End 2023.xlsx` (200,000+ rows)

2. **Place the file**:
   ```bash
   # Copy to data/ folder
   cp /path/to/source/file.xlsx data/chris_data.xlsx
   ```

3. **Verify**:
   ```bash
   ls -lh data/chris_data.xlsx
   ```

The file must contain a sheet named `"Main"` with the following columns:
- `Title` (study identifier)
- `Effect type` (should include "correlation" for PCC studies)
- `Effect size`
- `Standard error`
- `t-stat`
- `Sample size`
- `Filename` (meta-analysis identifier)
- `Author 1`
- `Year published`

### Sample Data

A minimal sample dataset is included at `inst/extdata/sample_data.xlsx` for testing. Use `run_chris_analysis(use_sample = TRUE)` to test without the full dataset.

## Configuration

Configuration is in `inst/extdata/chris_config.yaml`. Key settings:

- **Data**: File name, sheet name, column mappings
- **Analysis**: PCC identifier, index column
- **Methods**: Random-effects methods (RE, Fisher's Z)
- **Cleaning**: Data cleaning options (DOF filling, inverse relationships)
- **Logging**: Log level, file settings
- **Caching**: Enable/disable, cache age
- **Output**: Output file names and directories

## Outputs

### Analysis Results

- `output/chris_results.csv` - Main analysis results with statistics for each meta-analysis
- `output/estimator_summary.csv` - Summary statistics across all estimators (Table 1)
- `output/session_info.txt` - Session information for reproducibility

### Logs

- `logs/chris_analysis_YYYYMMDD_HHMMSS.log` - Timestamped detailed execution log

### PSB Analysis

- `output/psb_results.csv` - Publication Selection Bias analysis results

## Reproducibility

### Environment Locking

The package uses `renv` to lock all dependency versions. The `renv.lock` file ensures:

- Exact package versions across installations
- Reproducible results across platforms
- Easy environment restoration

### Session Information

Each analysis run saves `session_info.txt` with:
- R version
- Package versions
- Platform information

### Expected Results

For verification, compare outputs to reference results (if provided in `inst/expected/`).

## Troubleshooting

### Data File Not Found

**Error**: `Data file not found: data/chris_data.xlsx`

**Solution**:
1. Verify the file exists: `ls -lh data/chris_data.xlsx`
2. Check file permissions: `chmod 644 data/chris_data.xlsx`
3. Use sample data for testing: `run_chris_analysis(use_sample = TRUE)`

### Missing Dependencies

**Error**: Package installation fails

**Solution**:
```bash
# Restore via renv
make setup

# Or manually
Rscript -e "renv::restore()"
```

### Platform-Specific Issues

#### macOS: Missing Command Line Tools

```bash
xcode-select --install
```

#### Linux: Missing System Libraries

```bash
# Debian/Ubuntu
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

# Fedora
sudo dnf install -y libcurl-devel openssl-devel libxml2-devel
```

#### Windows: Rscript Not Found

Add R to your PATH:
- R installation directory → `bin` folder
- Or use Rtools which includes R in PATH

### Excel File Reading Issues

**Error**: Cannot read Excel file

**Solutions**:
1. Verify file is not corrupted
2. Check sheet name matches config (`"Main"` by default)
3. Ensure `readxl` package is installed: `install.packages("readxl")`

### renv Issues

**Error**: renv restore fails

**Solutions**:
```r
# Reinitialize renv
renv::init()

# Or restore without prompt
renv::restore(prompt = FALSE)
```

## Development

### Running Tests

```bash
make test
```

### Building Documentation

```bash
make document
```

### Code Quality

```bash
make lint    # Run lintr
make check   # Run R CMD check
```

### Contributing

1. Fork the repository
2. Create a feature branch
3. Make changes
4. Run tests: `make test`
5. Run checks: `make check`
6. Submit a pull request

## Citation

To cite this package:

```r
citation("pccsurvey")
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Author

Petr Čala - cala.p@seznam.cz

## Acknowledgments

This package uses the following key dependencies:
- `MAIVE` - Meta-Analysis Instrumental Variable Estimator
- `metafor` - Meta-Analysis Package for R
- And others (see `DESCRIPTION`)
