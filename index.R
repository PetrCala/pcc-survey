# Make execution location-independent by setting the working directory to the
# directory containing this script when run via Rscript.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_path <- sub("^--file=", "", file_arg[1])
  setwd(dirname(normalizePath(script_path)))
}
rm(list = ls())

# Source all files from the 'src' folder
script_directory <- "src/"

script_files <- list.files(path = script_directory, pattern = "\\.R$", full.names = TRUE)

for (file in script_files) {
  source(file)
}

STATIC <- read_static()

df <- load_data(file_name = STATIC$file_name)

### PCC validation

if (STATIC$validate$pcc_var_1) {
  message("Validating the PCC variance with an offset of 1.")
  custom_pcc_var_1 <- custom_pcc_variance(df, offset = 1)
  validate_vector(custom_vector = custom_pcc_var_1, expected_vector = df$pcc_var_1)
}

if (STATIC$validate$pcc_var_2) {
  message("Validating the PCC variance with an offset of 2.")
  custom_pcc_var_2 <- custom_pcc_variance(df, offset = 2)
  validate_vector(custom_vector = custom_pcc_var_2, expected_vector = df$pcc_var_2)
}

### Methods validation

message("Calculating the expected PCC statistics.")
expected_results <- list(
  re = re(df, method = STATIC$re_method),
  uwls = uwls(df),
  uwls3 = uwls3(df),
  hsma = hsma(df),
  fishers_z = fishers_z(df, method = STATIC$re_method_fishers_z)
)

#' A helper function to validate a single method
validate_method_wrapper <- function(method_name, method_func, expected_result) {
  message(paste("Validating the", method_name, "method."))
  custom_result <- method_func(df)
  validate_method(custom_results = custom_result, expected_results = expected_result)
}

if (STATIC$validate$re) {
  validate_method_wrapper("RE", custom_re, expected_results$re)
}

if (STATIC$validate$uwls) {
  validate_method_wrapper("UWLS", custom_uwls, expected_results$uwls)
}

if (STATIC$validate$uwls3) {
  validate_method_wrapper("UWLS+3", custom_uwls3, expected_results$uwls3)
}

if (STATIC$validate$hsma) {
  validate_method_wrapper("HSMA", custom_hsma, expected_results$hsma)
}

if (STATIC$validate$fishers_z) {
  validate_method_wrapper("Fisher's Z", custom_fishers_z, expected_results$fishers_z)
}

# Recreate the expected results data frame
save_data(results = expected_results, file_name = STATIC$expected_stats_file_name)

message("Done.")
