.DEFAULT_GOAL := help

.PHONY: help setup snapshot document build check lint run run-psb validate clean doctor test replicate zip

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
	@echo "  snapshot  Update renv.lock with current package versions (run after changing dependencies)"
	@echo "  replicate Run complete replication workflow (setup + data check + analysis)"
	@echo "  document  Generate roxygen docs (NAMESPACE/man)"
	@echo "  check     Run R CMD check via devtools"
	@echo "  lint      Run lintr on the package (fails on lint)"
	@echo "  run       Run the chris analysis (writes $(PROJECT_DIR)/output/chris_results.csv)"
	@echo "  run-psb   Run the PSB analysis (writes $(PROJECT_DIR)/output/psb_results.csv)"
	@echo "  validate  Alias for run"
	@echo "  test      Run testthat tests"
	@echo "  clean     Remove generated outputs"
	@echo "  doctor    Print environment info"
	@echo "  zip       Create pcc-survey-results-YY-MM-DD.zip with output CSVs, session_info.txt, and latest log"
	@echo ""

setup:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::restore()"

snapshot:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::snapshot(force = TRUE, prompt = FALSE)"

document:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::document()"

build:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::build()"

check:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::check()"

lint:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); if (!requireNamespace('lintr', quietly=TRUE)) stop('lintr not installed; run make setup'); l <- lintr::lint_package(); if (length(l) > 0) { print(l); quit(status=1) }"

run:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::load_all('.'); run_chris_analysis()"

run-psb:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::load_all('.'); run_psb_analysis()"

validate: run

test:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::test()"

clean:
	@$(RSCRIPT) -e "f1 <- file.path('$(PROJECT_DIR)','output','chris_results.csv'); f2 <- file.path('$(PROJECT_DIR)','output','psb_results.csv'); removed <- FALSE; if (file.exists(f1)) { file.remove(f1); cat('Removed:', f1, '\n'); removed <- TRUE }; if (file.exists(f2)) { file.remove(f2); cat('Removed:', f2, '\n'); removed <- TRUE }; if (!removed) { cat('No generated output to remove\n') }"

doctor:
	@$(RSCRIPT) -e "cat('R version:', R.version.string, '\n')"
	@$(RSCRIPT) -e "if (requireNamespace('devtools', quietly=TRUE)) { devtools::session_info() } else { cat('devtools not installed (run make setup)\\n') }"

replicate:
	@echo "=== Starting replication workflow ==="
	@echo "Step 1: Restoring dependencies..."
	@cd "$(PROJECT_DIR)" && $(MAKE) setup
	@echo "Step 2: Checking data availability..."
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::load_all('.'); if (exists('check_data_availability')) { check_data_availability() } else { cat('Data check function not yet implemented\\n') }"
	@echo "Step 3: Running analysis..."
	@cd "$(PROJECT_DIR)" && $(MAKE) run
	@echo "=== Replication workflow completed successfully ==="
	@echo ""
	@echo "Results are available in:"
	@echo "  - $(PROJECT_DIR)/output/chris_results.csv (main analysis results)"
	@echo "  - $(PROJECT_DIR)/output/estimator_summary.csv (estimator summary statistics)"
	@echo "  - $(PROJECT_DIR)/output/session_info.txt (session information for reproducibility)"
	@echo "  - $(PROJECT_DIR)/logs/ (timestamped log files)"

zip:
	@cd "$(PROJECT_DIR)" && \
	ZIP_NAME="pcc-survey-results-$$(date +%y-%m-%d).zip" && \
	rm -f "$$ZIP_NAME" && \
	(for f in output/chris_results.csv output/estimator_summary.csv output/psb_results.csv output/session_info.txt; do \
	  if [ ! -f "$$f" ]; then echo "Warning: $$f is missing"; fi; \
	done) && \
	LATEST_LOG=$$(ls -t logs/*.log 2>/dev/null | head -1) && \
	if [ -z "$$LATEST_LOG" ]; then echo "Warning: no log file found in logs/"; fi && \
	ZIP_FILES="" && \
	for f in output/chris_results.csv output/estimator_summary.csv output/psb_results.csv output/session_info.txt; do \
	  [ -f "$$f" ] && ZIP_FILES="$$ZIP_FILES $$f"; \
	done && \
	[ -n "$$LATEST_LOG" ] && ZIP_FILES="$$ZIP_FILES $$LATEST_LOG"; \
	if [ -n "$$ZIP_FILES" ]; then zip "$$ZIP_NAME" $$ZIP_FILES && echo "Created $$ZIP_NAME"; else echo "No files to zip (all expected files missing)."; fi


