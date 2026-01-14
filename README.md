<h1 align="center">PCC Survey (`pccsurvey`)</h1>

This repo is now a **proper R package** named `pccsurvey`. It validates outcomes of custom methods on a controlled dataset (shipped in `inst/extdata/base.xlsx`). It’s mainly meant for iterating on implementations in `R/custom.R` and comparing them against the reference methods in `R/pcc.R`.

## Table of contents

- [Table of contents](#table-of-contents)
- [Quickstart (Makefile)](#quickstart-makefile)
  - [Run in RStudio (optional)](#run-in-rstudio-optional)
- [Prerequisites](#prerequisites)
- [OS setup](#os-setup)
  - [macOS](#macos)
  - [Linux](#linux)
  - [Windows](#windows)
- [Makefile commands](#makefile-commands)
- [Configuration](#configuration)
- [What the script does](#what-the-script-does)
- [Inputs / outputs](#inputs--outputs)
- [Chris Analysis Data](#chris-analysis-data)
- [Notes](#notes)

## Quickstart (Makefile)

From the repo root:

```bash
make setup
make run
```

**Note**: The `make run` command runs the Chris analysis workflow. For this to work, you need to have the `chris_data.xlsx` file in the `data/` folder (see [Chris Analysis Data](#chris-analysis-data) below).

Outputs:

- `data/chris_results.csv` (overwritten on each run)

### Run in RStudio (optional)

If you prefer RStudio, open this folder as a project, then run:

```r
devtools::load_all()
run_chris_analysis()
```

## Prerequisites

- R (so `Rscript` works from your terminal).
- A working `make` installation.

If you don’t have `make`, follow the OS section below.

## OS setup

### macOS

- Install R from CRAN.
- Install Command Line Tools (provides `make`):

```bash
xcode-select --install
```

### Linux

- Install R + make using your distro’s package manager (examples):

```bash
# Debian/Ubuntu
sudo apt-get update && sudo apt-get install -y r-base make
```

```bash
# Fedora
sudo dnf install -y R make
```

### Windows

You have three good options (pick one):

- **WSL2 (recommended)**: install Ubuntu, then install R + make inside WSL, and run `make ...` from the WSL shell.
- **Git Bash**: install Git for Windows (includes Git Bash) and a `make` provider (e.g. MSYS2), then run `make ...` from Git Bash.
- **Rtools**: install Rtools for your R version (includes `make` inside its MSYS2 environment).

Regardless of option, ensure `Rscript` is available in your shell `PATH`.

## Makefile commands

From the repo root:

```bash
make help
make doctor
make setup
make document
make check
make run
make clean
```

## Configuration

Configuration is shipped in `inst/extdata/static.yaml` and is read by `read_static()` at runtime. Common keys:

- `file_name`: Input Excel file name under `data/` (default: `base.xlsx`).
- `expected_stats_file_name`: Output Excel file name under `data/` (default: `expected_stats.xlsx`).
- `re_method`: Random-effects “flavor” (e.g. `"DL"`, `"ML"`, `"REML"`, ...).
- `re_method_fishers_z`: Same idea as `re_method`, but for Fisher’s Z.
- `validate`: Toggles for individual validation steps (`pcc_var_1`, `pcc_var_2`, `re`, `uwls`, `uwls3`, `hsma`, `fishers_z`).
- `separators`: Present in the YAML, but currently not used when reading Excel inputs.

## What the script does

Running `make run` (or `run_chris_analysis()`):

- Loads the package code from `R/`.
- Reads config from `inst/extdata/chris_config.yaml`.
- Loads data from `data/chris_data.xlsx` (see [Chris Analysis Data](#chris-analysis-data) section).
- Cleans and preprocesses the data (filters to PCC studies, fills missing DOF, calculates PCC variance).
- Calculates meta-analysis statistics for each individual meta-analysis using multiple methods (RE, UWLS, UWLS+3, HSMA, Fisher's Z).
- Calculates statistics for all meta-analyses combined.
- Writes results to `data/chris_results.csv`.

## Inputs / outputs

- **Inputs**:
  - `inst/extdata/chris_config.yaml` - Configuration for the Chris analysis
  - `data/chris_data.xlsx` - The analysis data file (must be provided by the user, see [Chris Analysis Data](#chris-analysis-data))
- **Outputs**:
  - `data/chris_results.csv` - Analysis results with statistics for each meta-analysis (overwritten on each run)
  - `logs/chris_analysis_YYYYMMDD_HHMMSS.log` - Timestamped log file with detailed execution information

## Chris Analysis Data

The Chris analysis requires an external data file that is not included in the repository:

1. **Download the source file**: The original file is typically named something like `Copy of all_datasets_combined End 2023.xlsx` and contains over 200,000 rows of meta-analysis data.

2. **Place and rename the file**:
   - Copy the file to the `data/` folder in the repository root
   - Rename it to `chris_data.xlsx`
   - The file should have a sheet named `"Main"` containing the data

3. **Verify the file**: After placing the file, you can verify it's in the correct location:

   ```bash
   ls -lh data/chris_data.xlsx
   ```

The analysis will read from `data/chris_data.xlsx` when you run `make run`. If the file is missing, you'll get an error indicating the data file was not found.

## Notes

- The analysis results are saved to `data/chris_results.csv` after each run.
- Log files are created in the `logs/` directory with timestamps for each run.
