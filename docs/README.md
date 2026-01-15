# Publication Selection Bias (PSB) Analysis

This extension implements methods for detecting and quantifying publication selection bias in meta-analyses.

## Overview

The PSB analysis calculates several measures to assess whether there is an excess of statistically significant findings in a meta-analysis, which may indicate publication bias. The analysis is performed using three different mean effect estimates (UWLS, UWLS+3, and Hunter-Schmidt) to examine how different estimation methods affect the bias measures.

## Formulas

### Statistical Power

For each study *i* with standard error *se<sub>i</sub>* and assuming true effect = *μ* (mean effect estimate):

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
