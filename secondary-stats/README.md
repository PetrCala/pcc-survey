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
| `PCCcombined.dta` | Combined study-level dataset across the 172 MAs. |
| `pcc_survey_results.dta` | Per-MA results (Stata copy); individual Egger tests (FAT) live here. |
