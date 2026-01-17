# Publication Selection Bias (PSB) Analysis

This extension implements methods for detecting and quantifying publication selection bias in meta-analyses.

## Overview

The PSB analysis calculates several measures to assess whether there is an excess of statistically significant findings in a meta-analysis, which may indicate publication bias. The analysis is performed using three different mean effect estimates (UWLS, UWLS+3, and Hunter-Schmidt) to examine how different estimation methods affect the bias measures.

## Formulas

### Expected Proportion of Significant Effects (E_sig)

E_sig is calculated using a **one-sided** test that includes random heterogeneity from RE1's estimate of τ². This is not a simple power calculation, but rather accounts for the heterogeneity variance in the meta-analysis.

For each study *i* with standard error *SE`<sub>`i`</sub>`*, mean effect estimate *|UWLS|* (absolute value of the method's estimate), and heterogeneity variance *τ²* from RE1:

**Z-score for each study:**

```
Z_i = (z_α * SE_i - |UWLS|) / √(SE_i² + τ²)
```

where:

- *SE_i* = standard error for study *i*
- *|UWLS|* = absolute value of the mean effect estimate (from UWLS, UWLS+3, or HS method)
- *τ²* = heterogeneity variance from RE1's estimate (using S1 standard errors)
- *z_α* = critical value for one-sided test (e.g., for α = 0.05, z_α = 1.645)

**Expected significance probability for each study:**

```
E_sigi = 1 - N(Z_i)
```

where *N(Z_i)* is the cumulative normal distribution function (Φ in standard notation).

**Overall expected proportion:**

```
E_sig = (Σᵢ₌₁ᵏ E_sigi) / k
```

where *k* = number of effects/studies.

### Observed Proportion of Significant Studies (P_ss)

Count of studies that are statistically and positively significant, divided by the total number of studies. Uses t-distribution critical values adjusted for degrees of freedom:

```
P_ss = (Σ I(t_i > t_critical(df_i))) / k
```

where:

- *t_i* = t-value for study *i*
- *t_critical(df_i)* = critical value from t-distribution with *df_i* degrees of freedom for one-sided test (α = 0.05)
- Only positive significant results are counted (t_value > critical_value)
- *k* = number of studies

### Excess Statistical Significance (ESS)

ESS compares the proportion of statistically significant effects (P_ss) to the expected proportion (E_sig). ESS is expressed as a proportion:

```
ESS = P_ss - E_sig
```

where:

- *P_ss* = observed proportion of statistically significant effects (positive significant studies / total studies)
- *E_sig* = expected proportion of statistically significant effects (if there were no PSB)

ESS can also be expressed in terms of counts, then converted to a proportion:

```
ESS = (Observed_count - Expected_count) / k
```

## Methods

The PSB measures are calculated using three different mean effect estimates:

1. **UWLS** (Unweighted Least Squares)
2. **UWLS+3** (UWLS with adjusted degrees of freedom)
3. **HS** (Hunter-Schmidt)

**Important:**

- All methods use the same **τ² (heterogeneity variance)** from **RE1's estimate** (calculated using S1 standard errors with the random effects method specified, default "ML").
- Each method uses its own mean effect estimate in the E_sig calculation (the absolute value of the estimate is used in the formula).
- This allows comparison of how different estimation methods affect the bias measures, which is the core focus of this survey paper.

The E_sig calculation is **one-sided** (not two-sided) and includes the heterogeneity variance τ² in the denominator: √(SE_i² + τ²), which accounts for random heterogeneity in the meta-analysis.

**All output measures (ESS, E_sig, P_ss) are expressed as proportions for consistency.**

## Output

The analysis produces one row per meta-analysis with columns for ESS, E_sig, and P_ss (all as proportions) calculated using each method (e.g., `ess_uwls`, `ess_uwls3`, `ess_hs`, `esig_uwls`, etc.), plus `pss` and `k` (number of estimates) which are the same for all methods.

### Output File: `psb_results.csv`

The output file contains one row per meta-analysis (plus a final row for "All meta-analyses" if applicable). Each row includes the following columns:

#### Identification Columns

- **`idx`**: Index number for the meta-analysis (if enabled in configuration)
- **`meta`**: Name/identifier of the meta-analysis

#### Common Statistics (Same for All Methods)

- **`pss`**: P_ss - Observed proportion of statistically significant studies (number of positive significant studies / total studies)
- **`k`**: Number of estimates/studies in the meta-analysis

#### Publication Bias Measures (Per Method: UWLS, UWLS+3, HS)

For each method (suffix: `_uwls`, `_uwls3`, `_hs`):

- **`ess_<method>`**: ESS - Excess Statistical Significance as a proportion. Difference between observed and expected proportions of significant studies. Positive values indicate excess significance (potential publication bias).

  Formula: `ESS = P_ss - E_sig = (observed_count / k) - (expected_count / k)`

- **`esig_<method>`**: E_sig - Expected proportion of statistically significant studies based on one-sided E_sig calculations with heterogeneity variance (τ²) from RE1.

  Formula: `E_sig = (Σ E_sigi) / k`

#### Interpretation Notes

- **ESS**: Large positive values suggest strong publication bias. Negative values indicate fewer significant results than expected (may indicate conservative bias or low power).
- **E_sig**: The expected proportion of significant studies if there were no publication bias. Compare to `pss` to assess bias.
- **P_ss**: The observed proportion of positive statistically significant studies. Compare to `esig_<method>` to assess publication bias.
- All measures (ESS, E_sig, P_ss) are expressed as proportions for consistency.

The three methods (UWLS, UWLS+3, HS) may produce different estimates, allowing comparison of how different mean effect estimation methods affect the bias measures.
