# Publication Selection Bias (PSB) Analysis

This extension implements methods for detecting and quantifying publication selection bias in meta-analyses.

## Overview

The PSB analysis calculates several measures to assess whether there is an excess of statistically significant findings in a meta-analysis, which may indicate publication bias. The analysis is performed using three different mean effect estimates (UWLS, UWLS+3, and Hunter-Schmidt) to examine how different estimation methods affect the bias measures.

## Formulas

### Statistical Power

For each study *i* with standard error *se`<sub>`i`</sub>`* and assuming true effect = *μ* (mean effect estimate):

**Non-centrality parameter:**

```
λ_i = μ / se_i
```

**Power (two-tailed test, α = 0.05):**

```
Power_i = P(|Z| > 1.96 | Z ~ N(λ_i, 1))
       = 1 - P(-1.96 < Z < 1.96)
       = 1 - [Φ(1.96 - λ_i) - Φ(-1.96 - λ_i)]
```

where Φ is the standard normal cumulative distribution function.

### Expected Number of Significant Studies

Sum of power across all studies:

```
E[Significant] = Σ Power_i
```

### Observed Number of Significant Studies

Count of studies where |t-value| > 1.96:

```
Observed = Σ I(|t_i| > 1.96)
```

### Excess Statistical Significance (ESS)

Difference between observed and expected:

```
ESS = Observed - E[Significant]
```

### Proportion of Statistically Significant Tests (PSST)

Ratio of observed to expected proportion:

```
PSST = (Observed / N) / (E[Significant] / N)
     = Observed / E[Significant]
```

where *N* is the total number of studies.

### Proportion Selected to be Statistically Significant (Psss)

Proportion of results that were selectively reported:

```
Psss = ESS / (1 - E[Significant] / N)
```

### Falsely Positive Evidence

Proportion of studies that are falsely positive:

```
Falsely Positive = ESS / N
```

## Methods

The PSB measures are calculated using three different mean effect estimates:

1. **UWLS** (Unweighted Least Squares)
2. **UWLS+3** (UWLS with adjusted degrees of freedom)
3. **HS** (Hunter-Schmidt)

This allows comparison of how different estimation methods affect the bias measures, which is the core focus of this survey paper.

## Output

The analysis produces one row per meta-analysis with columns for each measure calculated using each method (e.g., `ess_uwls`, `ess_uwls3`, `ess_hs`, `psst_uwls`, etc.).

### Output File: `psb_results.csv`

The output file contains one row per meta-analysis (plus a final row for "All meta-analyses" if applicable). Each row includes the following columns:

#### Identification Columns

- **`idx`**: Index number for the meta-analysis (if enabled in configuration)
- **`meta`**: Name/identifier of the meta-analysis

#### Observed Statistics (Same for All Methods)

- **`observed_prop_ss`**: Observed proportion of statistically significant studies (number of significant studies / total studies)
- **`observed_count_ss`**: Observed count of statistically significant studies (number of studies where |t-value| > 1.96)

#### Expected Statistics (Per Method: UWLS, UWLS+3, HS)

For each method (suffix: `_uwls`, `_uwls3`, `_hs`):

- **`expected_prop_ss_<method>`**: Expected proportion of statistically significant studies based on statistical power calculations
- **`expected_count_ss_<method>`**: Expected count of statistically significant studies (sum of power across all studies)

#### Publication Bias Measures (Per Method: UWLS, UWLS+3, HS)

For each method (suffix: `_uwls`, `_uwls3`, `_hs`):

- **`ess_<method>`**: Excess Statistical Significance. Difference between observed and expected number of significant studies. Positive values indicate excess significance (potential publication bias).

  Formula: `ESS = observed_count_ss - expected_count_ss_<method>`
- **`psst_<method>`**: Proportion of Statistically Significant Tests. Ratio of observed to expected proportion of significant studies.

  Formula: `PSST = observed_prop_ss / expected_prop_ss_<method>`

  Values > 1 indicate more significant results than expected.
- **`psss_<method>`**: Proportion Selected to be Statistically Significant. Estimate of the proportion of results that were selectively reported due to statistical significance.

  Formula: `Psss = ESS / (1 - expected_prop_ss_<method>)`

  Higher values indicate stronger selection for significant results.
- **`falsely_positive_<method>`**: Falsely Positive Evidence. Proportion of studies that are falsely positive (excess significant studies as a proportion of total studies).

  Formula: `Falsely Positive = ESS / total_studies`

  Values range from -1 to 1, with positive values indicating excess false positives.

#### Interpretation Notes

- **ESS**: Large positive values suggest strong publication bias. Negative values indicate fewer significant results than expected (may indicate conservative bias or low power).
- **PSST**: Values > 1 indicate excess significance. Values < 1 indicate fewer significant results than expected.
- **Psss**: Estimates the proportion of selectively reported results. Higher values (closer to 1) indicate stronger selection bias.
- **Falsely Positive**: Proportion of studies that are likely false positives due to publication bias. Values around 0.4-0.5 suggest substantial bias.

The three methods (UWLS, UWLS+3, HS) may produce different estimates, allowing comparison of how different mean effect estimation methods affect the bias measures.
