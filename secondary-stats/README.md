# Secondary statistics

Additional descriptive statistics and publication-selection-bias (PSB) tests for the
172 PCC meta-analyses, provided by Tom Stanley as a companion to the main replication
package. The primary per-MA results and Table 1 summary are produced by the R code in
this repository and written to `output/pcc_survey_results.csv` and
`output/estimator_summary.csv`; the files here cover the secondary analyses calculated
separately (largely in Stata/Statview/spreadsheets).

## Files in this folder

| File(s) | Contents |
| --- | --- |
| `ICT-Illustration.*`, `ICT-HS.xls`, `ICT-PSST.*`, `ICT-STATA.docx` | The ICT illustration: data and code (`ICT-*`). |
| `RevisedPCC-Descriptive-Stats.docx` | Simple descriptive statistics of the survey of 172 PCC MAs (from Statview). |
| `Aggregate PSST.xlsx` | Aggregate PSST with visible formulas. |
| `IndividualPSST.dta` | Individual PSST results (same formulas as the aggregate). |
| `FAT-PET-FE-MetaScience.docx` | Aggregate Egger test (FAT-PET-FE) with code. |
| `PCCcombined.dta` | Combined estimate-level dataset across the 172 MAs: the package's `output/pcc_combined_dataset.csv` (effect, S1 standard error, sample size, degrees of freedom, t-value and the imputed-sample-size flag) plus Tom's derived variables `Meta`, `Precision` (1 / se), `t` and `IV` (1 / se^2). Regenerated from the package on 27 October 2026 in double precision; the previous copy stored effect and se as 4-byte floats, which made the degrees of freedom backed out of them noisy for large samples, and carried 12 malformed rows from its CSV import. |
| `pcc_survey_results.dta` | Per-MA results (Stata copy of `output/pcc_survey_results.csv`, regenerated from the package on 27 October 2026, so with Fisher's z on 1 / (df - 1), the study-level weight shares and `n_studies`); Tom's added variables `tFAT`, `PSST`, `df` and `pvalue` (the individual Egger tests and PSSTs) are kept. The heterogeneity variables the package no longer reports (`tau2_re1`, `gamma_uwls1`, `q_re1`, `i2_re1`, `q_re2`, `i2_re2`, `q_uwls1`, `i2_uwls1`) are dropped. |
