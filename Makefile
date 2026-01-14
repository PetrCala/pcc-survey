.DEFAULT_GOAL := help

.PHONY: help setup document build check lint run validate clean doctor

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
	@echo "  setup     Install dependencies via devtools (bootstraps devtools if needed)"
	@echo "  document  Generate roxygen docs (NAMESPACE/man)"
	@echo "  check     Run R CMD check via devtools"
	@echo "  lint      Run lintr on the package (fails on lint)"
	@echo "  run       Run the chris analysis (writes $(PROJECT_DIR)/data/chris_results.csv)"
	@echo "  validate  Alias for run"
	@echo "  clean     Remove generated outputs"
	@echo "  doctor    Print environment info"
	@echo ""

setup:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) install.packages('devtools', repos='https://cloud.r-project.org'); devtools::install_deps(dependencies = TRUE)"

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

validate: run

clean:
	@$(RSCRIPT) -e "f1 <- file.path('$(PROJECT_DIR)','data','expected_stats.xlsx'); f2 <- file.path('$(PROJECT_DIR)','data','chris_results.csv'); removed <- FALSE; if (file.exists(f1)) { file.remove(f1); cat('Removed:', f1, '\n'); removed <- TRUE }; if (file.exists(f2)) { file.remove(f2); cat('Removed:', f2, '\n'); removed <- TRUE }; if (!removed) { cat('No generated output to remove\n') }"

doctor:
	@$(RSCRIPT) -e "cat('R version:', R.version.string, '\n')"
	@$(RSCRIPT) -e "if (requireNamespace('devtools', quietly=TRUE)) { devtools::session_info() } else { cat('devtools not installed (run make setup)\\n') }"


