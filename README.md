# Methods validation

This folder serves to validate the outcomes of several custom methods upon a set of controlled data - `base.xlsx`.

## Table of contents

- [Methods validation](#methods-validation)
  - [Table of contents](#table-of-contents)
  - [How to use](#how-to-use)
    - [Run in RStudio](#run-in-rstudio)
    - [Run in pure R (CLI)](#run-in-pure-r-cli)
  - [Modifying custom methods](#modifying-custom-methods)
  - [Source data description](#source-data-description)
  - [Notes](#notes)

## How to use

Before running, you may want to review `static.yaml` (run configuration). Some important settings:

- `use_reml` - If set to true, `REML` will be used for Random-Effects calculation instead of the default `plm`.
- `validate` - This node lists the individual validation steps. For each step that has a value set to `true`, the script will validate that the custom method yields the same results to the expected ones. To see how to modify the custom methods, read the [Modifying custom methods](#modifying-custom-methods) section.

### Run in RStudio

1. Open this folder as an RStudio project (or set your working directory to the root of this folder).
1. Ensure dependencies are installed (see `packages.txt`). You can install missing packages via the RStudio Packages UI or by running `install.packages("<pkg>")`.
1. Open `index.R` and run it.

### Run in pure R (CLI)

1. From a shell, `cd` into this folder (the directory containing this `README.md`).
1. Install dependencies from `packages.txt` (one package name per line):

```r
pkgs <- readLines("packages.txt", warn = FALSE)
pkgs <- trimws(pkgs)
pkgs <- pkgs[nzchar(pkgs)]
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)
```

1. Run the script:

```bash
Rscript index.R
```

## Modifying custom methods

By default, all methods have an expected result for the predefined set of data (`base.xlsx`). These are listed in `src/pcc.R`.

If you wish to compare these against your custom method, **go to `src/custom.R`**. Here, I have laid out several placeholder functions, which calculate the results of one of several PCC methods. You can write down you custom code here, and the results of this code will be automatically compared against the expected results, yielded from the `pcc.R` scripts' methods.

When defining the custom methods, keep in mind the following:

- For `custom_pcc_variance`, the function **must return a numeric vector**. Otherwise, there are no restrictions.
- For the other custom methods, **they must all return a list with two keys - `est` and `t_value`**. For example:

  ```R
  # THIS IS A VALID RETURN
  my_list <- list(est = 1, t_value = 2)
  return(my_list)

  # WHILE THIS IS NOT
  return(list(custom_est_key = 1, some_t_value_key = 3))
  return(list(est = 1)) # Missing t_value
  return(list(t_value = 2)) # Missing est
  ```

- Each method accepts (among others) a `df` argument, as in _data frame_. This is the data frame, as you can see it in the `base.xlsx` file. Consequently, this allows you to easily access the effect, standard effect, and other variables easily using the following code:

  ```R
  some_custom_function <- function(df) {
    effect <- df$effect
    se <- df$se

    my_ols <- lm(effect ~ se, data = df)
    ...
  }
  ```

## Source data description

The main data frame to test against, `base.xlsx`, is a single meta-analysis from the file `1. Aid and Growth journals.xlsx`. To make the computatinos smoother, the following modifications have been done to obtain this data frame:

- Renamed columns
- Dropped rows which either do not report Partial Correlation Coefficient, effect size, or standard error.
- T-value was imputed where missing in the following manner - `t_value = effect / se`.
- Degrees of freedom were missing for all observations, and were thus imputed using the following function

  ```R
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

- PCC variance was calculated for two offsets - offset of 1, and offest of 2, using the function `pcc_variance` from the module `src/pcc.R:38`.

## Notes

- The expected results are also, by default, stored in the `expected_stats.xlsx` file for immediate access.
