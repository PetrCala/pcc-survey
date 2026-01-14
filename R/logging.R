# Simplified logging setup for Chris analysis
# Extracted and simplified from meta-facilitator

#' Setup logging for the chris analysis
#'
#' @param log_level [character] Log level: "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
#' @param log_to_console_only [logical] If TRUE, only log to console
#' @param log_file_name [character] Name of the log file (or NULL to skip file logging)
#' @param log_flush_on_setup [logical] If TRUE, clear the log file on startup
#' @export
setup_chris_logging <- function(
    log_level = "INFO",
    log_to_console_only = FALSE,
    log_file_name = "chris_analysis.log",
    log_flush_on_setup = TRUE) {
  # Map log level string to logger constant
  log_level_map <- list(
    "DEBUG" = logger::DEBUG,
    "INFO" = logger::INFO,
    "WARN" = logger::WARN,
    "ERROR" = logger::ERROR,
    "FATAL" = logger::FATAL
  )

  if (!log_level %in% names(log_level_map)) {
    cli::cli_abort("Invalid log level specified. Choose from 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'.")
  }

  # Set the logging threshold
  logger::log_threshold(log_level_map[[log_level]])

  # Always set console logger
  logger::log_appender(logger::appender_console)

  # Set file appender if enabled
  if (!log_to_console_only && !is.null(log_file_name)) {
    # Create logs directory if it doesn't exist
    log_dir <- "logs"
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }

    log_file <- file.path(log_dir, log_file_name)

    # Flush log file if requested
    if (log_flush_on_setup && file.exists(log_file)) {
      file.remove(log_file)
    }

    logger::log_appender(logger::appender_file(log_file, max_files = 1L), index = 2)
  }
}
