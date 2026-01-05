<h1 align="center">PCC Survey</h1>

This folder validates the outcomes of custom methods on a controlled dataset (default: `data/base.xlsx`). It’s mainly meant for iterating on implementations in `src/custom.R` and comparing them against the reference methods in `src/pcc.R`.

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
- [Modifying custom methods](#modifying-custom-methods)
- [Source data description](#source-data-description)
- [Notes](#notes)

## Quickstart (Makefile)

From the repo root:

```bash
make setup
make run
```

Outputs:

- `pcc-survey/data/expected_stats.xlsx` (overwritten on each run)

### Run in RStudio (optional)

If you prefer RStudio, open this folder as a project (or set the working directory to `pcc-survey/`), then run `index.R`.

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
make run
make clean
```

## Configuration

Configuration lives in `static.yaml` and is read by `read_static()` at runtime. Common keys:

- `file_name`: Input Excel file name under `data/` (default: `base.xlsx`).
- `expected_stats_file_name`: Output Excel file name under `data/` (default: `expected_stats.xlsx`).
- `re_method`: Random-effects “flavor” (e.g. `"DL"`, `"ML"`, `"REML"`, ...).
- `re_method_fishers_z`: Same idea as `re_method`, but for Fisher’s Z.
- `validate`: Toggles for individual validation steps (`pcc_var_1`, `pcc_var_2`, `re`, `uwls`, `uwls3`, `hsma`, `fishers_z`).
- `separators`: Present in the YAML, but currently not used when reading Excel inputs.

## What the script does

Running `index.R`:

- Sources all `*.R` files from `src/`.
- Reads `static.yaml`.
- Loads data from `data/<file_name>`.
- Optionally validates PCC variance (offset 1/2) against `df$pcc_var_1` / `df$pcc_var_2`.
- Computes the expected method results (reference implementations).
- Optionally validates your custom implementations (`src/custom.R`) against the expected results.
- Writes a summary of expected results to `data/<expected_stats_file_name>`.

## Inputs / outputs

- **Inputs**:
  - `static.yaml`
  - `data/<file_name>` (default: `data/base.xlsx`)
- **Outputs**:
  - `data/<expected_stats_file_name>` (default: `data/expected_stats.xlsx`, overwritten)
  - Console messages; validation failures `stop()` the run

## Modifying custom methods

By default, all methods have an expected result for the predefined set of data (`base.xlsx`). These are listed in `src/pcc.R`.

If you wish to compare these against your custom method, **go to `src/custom.R`**. There are placeholder functions you can replace with your own code; their results will be compared against the expected results produced by the reference methods in `src/pcc.R`.

When defining the custom methods, keep in mind the following:

- For `custom_pcc_variance`, the function **must return a numeric vector**. Otherwise, there are no restrictions.
- For the other custom methods, **they must all return a list with two keys - `est` and `t_value`**. For example:

  ```r
  # THIS IS A VALID RETURN
  my_list <- list(est = 1, t_value = 2)
  return(my_list)

  # WHILE THIS IS NOT
  return(list(custom_est_key = 1, some_t_value_key = 3))
  return(list(est = 1)) # Missing t_value
  return(list(t_value = 2)) # Missing est
  ```

- Each method accepts (among others) a `df` argument, as in _data frame_. This is the data frame, as you can see it in the `base.xlsx` file. Consequently, this allows you to easily access the effect, standard error, and other variables using the following code:

  ```r
  some_custom_function <- function(df) {
    effect <- df$effect
    se <- df$se

    my_ols <- lm(effect ~ se, data = df)
    ...
  }
  ```

## Source data description

The main data frame to test against, `base.xlsx`, is a single meta-analysis from the file `1. Aid and Growth journals.xlsx`. To make the computations smoother, the following modifications have been done to obtain this data frame:

- Renamed columns
- Dropped rows which either do not report Partial Correlation Coefficient, effect size, or standard error.
- T-value was imputed where missing in the following manner - `t_value = effect / se`.
- Degrees of freedom were missing for all observations, and were thus imputed using the following function.

  ```r
  fill_dof_using_pcc <- function(df) {
    pcc <- df$effect
    se <- df$se
    t_values <- df$t_value
    dof <- df$dof

    calculated_t_values <- pcc / se # Might create inf

    t_values[is.na(t_values)] <- calculated_t_values[is.na(t_values)]
    t_values[is.infinite(t_values)] <- NA

    fillable_rows <- is.na(dof) & !is.na(t_values) & !is.na(pcc)
    if (sum(fillable_rows) == 0) {
      return(df)
    }
    df[fillable_rows, "dof"] <- ((1 / pcc[fillable_rows]^2) - 1) / t_values[fillable_rows]^2

    return(df)
  }
  ```

- PCC variance was calculated for two offsets (1 and 2) using `pcc_variance` (see `src/pcc.R`).

## Notes

- The expected results are also, by default, stored in the `expected_stats.xlsx` file for immediate access.
