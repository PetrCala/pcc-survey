# Simplified caching utilities for Chris analysis
# Extracted and simplified from meta-facilitator

#' Run a function with optional caching
#'
#' @param f [function] The function to call
#' @param use_cache [logical] Whether to use caching
#' @param cache_dir [character] Directory for cache storage
#' @param cache_age [numeric] Cache age in seconds
#' @param ... Arguments to pass to the function
#' @return The result of calling f(...)
#' @export
run_cached_function <- function(
    f,
    ...,
    use_cache = TRUE,
    cache_dir = "_cache",
    cache_age = 3600) {
  if (!is.function(f)) {
    cli::cli_abort("f must be a function")
  }

  if (!use_cache) {
    return(f(...))
  }

  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create disk cache
  disk_cache <- cachem::cache_disk(dir = cache_dir, max_size = 1e9, max_age = cache_age)

  # Memoize the function
  cached_f <- memoise::memoise(f, cache = disk_cache)
  cached_f(...)
}

#' Helper function to call a function with optional caching based on config
#'
#' @param config [list] Configuration list with caching settings
#' @param f [function] The function to call
#' @param ... Arguments to pass to the function
#' @return The result of calling f(...)
#' @export
maybe_cached <- function(config, f, ...) {
  if (config$caching$use_cache) {
    run_cached_function(
      f = f,
      ...,
      use_cache = TRUE,
      cache_dir = "_cache",
      cache_age = config$caching$cache_age
    )
  } else {
    f(...)
  }
}
