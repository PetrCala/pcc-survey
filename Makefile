.DEFAULT_GOAL := help

.PHONY: help setup run validate clean doctor

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
	@echo "  setup     Install required R packages (from $(PROJECT_DIR)/packages.txt)"
	@echo "  run       Run the validation script (writes $(PROJECT_DIR)/data/expected_stats.xlsx)"
	@echo "  validate  Alias for run"
	@echo "  clean     Remove generated outputs"
	@echo "  doctor    Print environment info + report missing R packages"
	@echo ""

setup:
	@$(RSCRIPT) "$(PROJECT_DIR)/scripts/install-packages.R"

run:
	@cd "$(PROJECT_DIR)" && $(RSCRIPT) index.R

validate: run

clean:
	@$(RSCRIPT) -e "f <- file.path('$(PROJECT_DIR)','data','expected_stats.xlsx'); if (file.exists(f)) { file.remove(f); cat('Removed:', f, '\n') } else { cat('No generated output to remove:', f, '\n') }"

doctor:
	@$(RSCRIPT) -e "cat('R version:', R.version.string, '\n')"
	@$(RSCRIPT) -e "pkgs_file <- file.path('$(PROJECT_DIR)','packages.txt'); pkgs <- readLines(pkgs_file, warn=FALSE); pkgs <- trimws(pkgs); pkgs <- pkgs[nzchar(pkgs) & !startsWith(pkgs,'#')]; missing <- pkgs[!pkgs %in% rownames(installed.packages())]; if (length(missing) == 0) { cat('All required packages are installed.\n') } else { cat('Missing packages:\n'); cat(paste0('- ', missing, collapse='\n')); cat('\n') }"


