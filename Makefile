.DEFAULT_GOAL := help

.PHONY: help setup document build check run validate clean doctor

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
	@echo "  run       Run the validation (writes $(PROJECT_DIR)/data/expected_stats.xlsx)"
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

run:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) -e "if (!requireNamespace('devtools', quietly=TRUE)) stop('devtools not installed; run make setup'); devtools::load_all('.'); run_validation()"

validate: run

clean:
	@$(RSCRIPT) -e "f <- file.path('$(PROJECT_DIR)','data','expected_stats.xlsx'); if (file.exists(f)) { file.remove(f); cat('Removed:', f, '\n') } else { cat('No generated output to remove:', f, '\n') }"

doctor:
	@$(RSCRIPT) -e "cat('R version:', R.version.string, '\n')"
	@$(RSCRIPT) -e "if (requireNamespace('devtools', quietly=TRUE)) { devtools::session_info() } else { cat('devtools not installed (run make setup)\\n') }"


