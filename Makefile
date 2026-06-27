.DEFAULT_GOAL := help

.PHONY: help setup setup-dev snapshot document build check lint run run-psb validate clean doctor test replicate zip

# Absolute path to this repo's root (directory containing this Makefile).
# This avoids hard-coding the project directory name and works even if `make`
# is invoked from a different working directory.
PROJECT_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
RSCRIPT ?= Rscript

help:
	@echo ""
	@echo "Usage:"
	@echo "  make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  help      Show this help"
	@echo "  setup     Restore dependencies via renv (creates reproducible environment)"
	@echo "  setup-dev Install maintainer tooling (devtools, roxygen2, testthat, lintr)"
	@echo "  snapshot  Update renv.lock with current package versions (run after changing dependencies)"
	@echo "  replicate Run complete replication workflow (setup + data check + analysis)"
	@echo "  document  Generate roxygen docs (NAMESPACE/man)"
	@echo "  check     Run R CMD check via devtools"
	@echo "  lint      Run lintr on the package (fails on lint)"
	@echo "  run       Run the PCC Survey analysis (writes $(PROJECT_DIR)/output/pcc_survey_results.csv)"
	@echo "  run-psb   Run the PSB analysis (writes $(PROJECT_DIR)/output/psb_results.csv)"
	@echo "  validate  Alias for run"
	@echo "  test      Run testthat tests"
	@echo "  clean     Remove generated outputs"
	@echo "  doctor    Print environment info"
	@echo "  zip       Create pcc-survey-results-YY-MM-DD.zip with output CSVs, session_info.txt, and latest log"
	@echo ""

setup:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::restore(); if (!requireNamespace('pkgload', quietly=TRUE)) renv::install('pkgload')"

# Maintainer-only tooling (not needed to replicate the analysis). These packages
# are Suggests and are intentionally not in renv.lock (explicit snapshot), so
# install them separately before running document/check/lint/test.
setup-dev:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::install(c('devtools', 'roxygen2', 'testthat', 'lintr'))"

snapshot:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::snapshot(force = TRUE, prompt = FALSE)"

document:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup-dev'); devtools::document()"

build:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup-dev'); devtools::build()"

check:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup-dev'); devtools::check()"

lint:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup-dev'); if (!requireNamespace('lintr', quietly=TRUE)) stop('lintr not installed; run make setup-dev'); l <- lintr::lint_package(); if (length(l) > 0) { print(l); quit(status=1) }"

run:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('pkgload', quietly=TRUE)) stop('pkgload not installed; run make setup'); pkgload::load_all('.'); run_pcc_survey_analysis()"

run-psb:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('pkgload', quietly=TRUE)) stop('pkgload not installed; run make setup'); pkgload::load_all('.'); run_psb_analysis()"

validate: run

test:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup-dev'); devtools::test()"

clean:
	@$(RSCRIPT) -e "outs <- file.path('$(PROJECT_DIR)','output', c('pcc_survey_results.csv','estimator_summary.csv','smallest_estimate_counts.csv','pcc_combined_dataset.csv','psb_results.csv','session_info.txt')); existing <- outs[file.exists(outs)]; if (length(existing)) { file.remove(existing); cat('Removed:', existing, sep='\n'); cat('\n') } else { cat('No generated output to remove\n') }"

doctor:
	@$(RSCRIPT) -e "cat('R version:', R.version.string, '\n')"
	@$(RSCRIPT) -e "if (requireNamespace('devtools', quietly=TRUE)) { devtools::session_info() } else { cat('devtools not installed (run make setup-dev for full session info)\\n') }"

replicate:
	@echo "=== Starting replication workflow ==="
	@echo "Step 1: Restoring dependencies..."
	@cd "$(PROJECT_DIR)" && $(MAKE) setup
	@echo "Step 2: Checking data availability..."
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('pkgload', quietly=TRUE)) stop('pkgload not installed; run make setup'); pkgload::load_all('.'); if (exists('check_data_availability')) { check_data_availability() } else { cat('Data check function not yet implemented\\n') }"
	@echo "Step 3: Running analysis..."
	@cd "$(PROJECT_DIR)" && $(MAKE) run
	@echo "Step 4: Running publication-selection-bias (ESS) analysis..."
	@cd "$(PROJECT_DIR)" && $(MAKE) run-psb
	@echo "=== Replication workflow completed successfully ==="
	@echo ""
	@echo "Results are available in:"
	@echo "  - $(PROJECT_DIR)/output/pcc_survey_results.csv (main analysis results)"
	@echo "  - $(PROJECT_DIR)/output/estimator_summary.csv (Table 1 estimator summary)"
	@echo "  - $(PROJECT_DIR)/output/smallest_estimate_counts.csv (most-conservative counts)"
	@echo "  - $(PROJECT_DIR)/output/pcc_combined_dataset.csv (study-level FAT-PET panel)"
	@echo "  - $(PROJECT_DIR)/output/psb_results.csv (ESS / publication-selection bias)"
	@echo "  - $(PROJECT_DIR)/output/session_info.txt (session information for reproducibility)"
	@echo "  - $(PROJECT_DIR)/logs/ (timestamped log files)"

zip:
	@cd "$(PROJECT_DIR)" && \
	ZIP_NAME="pcc-survey-results-$$(date +%y-%m-%d).zip" && \
	rm -f "$$ZIP_NAME" && \
	(for f in output/pcc_survey_results.csv output/estimator_summary.csv output/smallest_estimate_counts.csv output/pcc_combined_dataset.csv output/psb_results.csv output/session_info.txt; do \
	  if [ ! -f "$$f" ]; then echo "Warning: $$f is missing"; fi; \
	done) && \
	LATEST_LOG=$$(ls -t logs/*.log 2>/dev/null | head -1) && \
	if [ -z "$$LATEST_LOG" ]; then echo "Warning: no log file found in logs/"; fi && \
	ZIP_FILES="" && \
	for f in output/pcc_survey_results.csv output/estimator_summary.csv output/smallest_estimate_counts.csv output/pcc_combined_dataset.csv output/psb_results.csv output/session_info.txt; do \
	  [ -f "$$f" ] && ZIP_FILES="$$ZIP_FILES $$f"; \
	done && \
	[ -n "$$LATEST_LOG" ] && ZIP_FILES="$$ZIP_FILES $$LATEST_LOG"; \
	if [ -n "$$ZIP_FILES" ]; then zip "$$ZIP_NAME" $$ZIP_FILES && echo "Created $$ZIP_NAME"; else echo "No files to zip (all expected files missing)."; fi

