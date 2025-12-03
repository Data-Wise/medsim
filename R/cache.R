# Ground Truth Caching System
#
# Efficient caching to avoid expensive recomputation

#' Save Object to Cache
#'
#' @description
#' Saves an R object to a cache file for later retrieval. Uses RDS format for
#' efficient storage and compression.
#'
#' @param object Any R object to cache
#' @param file Character: Path to cache file. Parent directory will be created
#'   if it doesn't exist.
#' @param compress Logical: Compress the cached object (default TRUE). Can
#'   significantly reduce file size for large objects.
#' @param overwrite Logical: Overwrite existing cache file (default TRUE)
#'
#' @return Invisibly returns the file path
#'
#' @details
#' ## Cache File Format
#'
#' Cache files are stored in RDS format with metadata:
#' - Original object
#' - Timestamp of creation
#' - R version used
#' - Package version (if available)
#'
#' ## File Organization
#'
#' Recommended cache directory structure:
#' ```
#' cache/
#' ├── truth_scenario_1.rds
#' ├── truth_scenario_2.rds
#' └── ...
#' ```
#'
#' @examples
#' \dontrun{
#' # Cache simulation truth
#' truth <- compute_expensive_truth()
#' medsim_cache_save(truth, "cache/truth_scenario1.rds")
#'
#' # Cache with metadata
#' result <- list(value = 42, timestamp = Sys.time())
#' medsim_cache_save(result, "cache/my_result.rds")
#' }
#'
#' @seealso [medsim_cache_load()], [medsim_cache_clear()]
#'
#' @export
medsim_cache_save <- function(object,
                               file,
                               compress = TRUE,
                               overwrite = TRUE) {

  # Check if file exists
  if (file.exists(file) && !overwrite) {
    message(sprintf("Cache file already exists: %s (use overwrite=TRUE to replace)", file))
    return(invisible(file))
  }

  # Create directory if needed
  cache_dir <- dirname(file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create cache object with metadata
  cache_object <- list(
    data = object,
    timestamp = Sys.time(),
    r_version = R.version.string,
    medsim_version = tryCatch(
      as.character(utils::packageVersion("medsim")),
      error = function(e) "unknown"
    )
  )

  # Save to file
  saveRDS(
    cache_object,
    file = file,
    compress = compress
  )

  message(sprintf("✓ Cached to: %s", file))

  invisible(file)
}

#' Load Object from Cache
#'
#' @description
#' Loads a previously cached R object. Returns NULL if cache file doesn't exist.
#'
#' @param file Character: Path to cache file
#' @param check_expiry Logical: Check if cache has expired (default FALSE)
#' @param max_age Numeric: Maximum age of cache in days (default 30). Only
#'   used if `check_expiry = TRUE`.
#' @param verbose Logical: Print messages about cache status (default TRUE)
#'
#' @return Cached object if file exists and is valid, NULL otherwise
#'
#' @details
#' ## Cache Validation
#'
#' When loading, the cache is checked for:
#' - File exists
#' - File is readable
#' - Age is within max_age (if check_expiry = TRUE)
#'
#' ## Handling Missing Cache
#'
#' If cache doesn't exist:
#' - Returns NULL (no error)
#' - Calling code should compute and cache the result
#'
#' Example pattern:
#' ```r
#' result <- medsim_cache_load("cache/truth.rds")
#' if (is.null(result)) {
#'   result <- expensive_computation()
#'   medsim_cache_save(result, "cache/truth.rds")
#' }
#' ```
#'
#' @examples
#' \dontrun{
#' # Load from cache
#' truth <- medsim_cache_load("cache/truth_scenario1.rds")
#'
#' if (is.null(truth)) {
#'   # Cache miss - compute and cache
#'   truth <- compute_expensive_truth()
#'   medsim_cache_save(truth, "cache/truth_scenario1.rds")
#' }
#'
#' # Load with expiry check
#' result <- medsim_cache_load(
#'   "cache/old_result.rds",
#'   check_expiry = TRUE,
#'   max_age = 7  # 7 days
#' )
#' }
#'
#' @seealso [medsim_cache_save()], [medsim_cache_exists()]
#'
#' @export
medsim_cache_load <- function(file,
                               check_expiry = FALSE,
                               max_age = 30,
                               verbose = TRUE) {

  # Check if file exists
  if (!file.exists(file)) {
    if (verbose) {
      message(sprintf("Cache miss: %s (file not found)", basename(file)))
    }
    return(NULL)
  }

  # Load cache object
  cache_object <- tryCatch(
    readRDS(file),
    error = function(e) {
      warning(sprintf("Failed to load cache file %s: %s", file, e$message))
      return(NULL)
    }
  )

  if (is.null(cache_object)) {
    return(NULL)
  }

  # Check cache format
  if (!is.list(cache_object) || !"data" %in% names(cache_object)) {
    # Old format - just return the object
    if (verbose) {
      message(sprintf("✓ Cache hit: %s (legacy format)", basename(file)))
    }
    return(cache_object)
  }

  # Check expiry if requested
  if (check_expiry && "timestamp" %in% names(cache_object)) {
    age_days <- as.numeric(difftime(Sys.time(), cache_object$timestamp, units = "days"))

    if (age_days > max_age) {
      if (verbose) {
        message(sprintf(
          "Cache expired: %s (%.1f days old, max %d days)",
          basename(file), age_days, max_age
        ))
      }
      return(NULL)
    }
  }

  if (verbose) {
    message(sprintf("✓ Cache hit: %s", basename(file)))
  }

  return(cache_object$data)
}

#' Check if Cache Exists
#'
#' @description
#' Checks if a cache file exists and is readable.
#'
#' @param file Character: Path to cache file
#'
#' @return Logical: TRUE if cache exists and is readable, FALSE otherwise
#'
#' @examples
#' if (medsim_cache_exists("cache/truth.rds")) {
#'   truth <- medsim_cache_load("cache/truth.rds")
#' } else {
#'   truth <- compute_truth()
#'   medsim_cache_save(truth, "cache/truth.rds")
#' }
#'
#' @export
medsim_cache_exists <- function(file) {
  file.exists(file) && file.access(file, mode = 4) == 0
}

#' Get Cache Info
#'
#' @description
#' Retrieves metadata about a cached object without loading the full object.
#'
#' @param file Character: Path to cache file
#'
#' @return List with cache metadata:
#'   - `exists`: Logical - file exists
#'   - `size_mb`: Numeric - file size in MB
#'   - `modified`: POSIXct - last modified time
#'   - `age_days`: Numeric - age in days
#'   - `timestamp`: POSIXct - creation time (if available)
#'   - `r_version`: Character - R version used (if available)
#'
#' @examples
#' info <- medsim_cache_info("cache/truth.rds")
#' print(info)
#'
#' @export
medsim_cache_info <- function(file) {

  info <- list(
    exists = file.exists(file),
    size_mb = NA_real_,
    modified = as.POSIXct(NA),
    age_days = NA_real_,
    timestamp = as.POSIXct(NA),
    r_version = NA_character_
  )

  if (!info$exists) {
    return(info)
  }

  # Get file info
  file_info <- file.info(file)
  info$size_mb <- file_info$size / 1024^2
  info$modified <- file_info$mtime
  info$age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  # Try to load metadata without loading full object
  cache_object <- tryCatch(
    readRDS(file),
    error = function(e) NULL
  )

  if (!is.null(cache_object) && is.list(cache_object)) {
    if ("timestamp" %in% names(cache_object)) {
      info$timestamp <- cache_object$timestamp
    }
    if ("r_version" %in% names(cache_object)) {
      info$r_version <- cache_object$r_version
    }
  }

  return(info)
}

#' Clear Cache
#'
#' @description
#' Deletes cache files from a directory. Can delete all files or files matching
#' a pattern.
#'
#' @param cache_dir Character: Directory containing cache files
#' @param pattern Character: Optional pattern to match files (e.g., "truth_*.rds")
#' @param max_age Numeric: Delete only files older than this many days. If NULL,
#'   deletes all matching files.
#' @param confirm Logical: Ask for confirmation before deleting (default TRUE)
#'
#' @return Invisibly returns number of files deleted
#'
#' @examples
#' \dontrun{
#' # Clear all cache
#' medsim_cache_clear("cache")
#'
#' # Clear old cache only
#' medsim_cache_clear("cache", max_age = 30)
#'
#' # Clear specific pattern
#' medsim_cache_clear("cache", pattern = "truth_*.rds")
#'
#' # Without confirmation
#' medsim_cache_clear("cache", confirm = FALSE)
#' }
#'
#' @export
medsim_cache_clear <- function(cache_dir,
                                pattern = "*.rds",
                                max_age = NULL,
                                confirm = TRUE) {

  if (!dir.exists(cache_dir)) {
    message(sprintf("Cache directory does not exist: %s", cache_dir))
    return(invisible(0))
  }

  # Find cache files
  cache_files <- list.files(
    cache_dir,
    pattern = glob2rx(pattern),
    full.names = TRUE
  )

  if (length(cache_files) == 0) {
    message("No cache files found")
    return(invisible(0))
  }

  # Filter by age if specified
  if (!is.null(max_age)) {
    file_info <- file.info(cache_files)
    age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))
    cache_files <- cache_files[age_days > max_age]

    if (length(cache_files) == 0) {
      message(sprintf("No cache files older than %d days", max_age))
      return(invisible(0))
    }
  }

  # Calculate total size
  total_size_mb <- sum(file.info(cache_files)$size) / 1024^2

  # Confirm deletion
  if (confirm) {
    message(sprintf(
      "About to delete %d cache files (%.1f MB)",
      length(cache_files), total_size_mb
    ))

    response <- readline("Continue? (yes/no): ")

    if (!tolower(response) %in% c("yes", "y")) {
      message("Cache clear cancelled")
      return(invisible(0))
    }
  }

  # Delete files
  deleted <- 0
  for (file in cache_files) {
    if (file.remove(file)) {
      deleted <- deleted + 1
    } else {
      warning(sprintf("Failed to delete: %s", file))
    }
  }

  message(sprintf(
    "✓ Deleted %d cache files (%.1f MB freed)",
    deleted, total_size_mb
  ))

  invisible(deleted)
}

#' List Cache Files
#'
#' @description
#' Lists all cache files in a directory with their metadata.
#'
#' @param cache_dir Character: Directory containing cache files
#' @param pattern Character: Pattern to match files (default "*.rds")
#'
#' @return data.frame with columns:
#'   - `file`: File name
#'   - `path`: Full path
#'   - `size_mb`: Size in MB
#'   - `age_days`: Age in days
#'   - `modified`: Last modified time
#'
#' @examples
#' # List all cache
#' cache_list <- medsim_cache_list("cache")
#' print(cache_list)
#'
#' # List specific pattern
#' truth_cache <- medsim_cache_list("cache", pattern = "truth_*.rds")
#'
#' @export
medsim_cache_list <- function(cache_dir, pattern = "*.rds") {

  if (!dir.exists(cache_dir)) {
    warning(sprintf("Cache directory does not exist: %s", cache_dir))
    return(data.frame())
  }

  # Find cache files
  cache_files <- list.files(
    cache_dir,
    pattern = glob2rx(pattern),
    full.names = TRUE
  )

  if (length(cache_files) == 0) {
    return(data.frame())
  }

  # Get file info
  file_info <- file.info(cache_files)

  # Create data.frame
  cache_df <- data.frame(
    file = basename(cache_files),
    path = cache_files,
    size_mb = file_info$size / 1024^2,
    age_days = as.numeric(difftime(Sys.time(), file_info$mtime, units = "days")),
    modified = file_info$mtime,
    stringsAsFactors = FALSE
  )

  # Sort by age (newest first)
  cache_df <- cache_df[order(cache_df$age_days), ]

  return(cache_df)
}

#' Initialize Cache Directory
#'
#' @description
#' Creates a cache directory with proper structure and a README file explaining
#' the cache system.
#'
#' @param cache_dir Character: Path to cache directory
#' @param create_readme Logical: Create README.md explaining cache (default TRUE)
#'
#' @return Invisibly returns cache directory path
#'
#' @examples
#' medsim_cache_init("cache")
#' medsim_cache_init("simulation_results/cache")
#'
#' @export
medsim_cache_init <- function(cache_dir, create_readme = TRUE) {

  # Create directory
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    message(sprintf("✓ Created cache directory: %s", cache_dir))
  } else {
    message(sprintf("Cache directory already exists: %s", cache_dir))
  }

  # Create README
  if (create_readme) {
    readme_file <- file.path(cache_dir, "README.md")

    if (!file.exists(readme_file)) {
      readme_content <- "# Simulation Cache

This directory contains cached simulation results to avoid expensive recomputation.

## Contents

- `truth_scenario_*.rds` - Ground truth values for each scenario
- `*.rds` - Other cached results

## Cache Management

Clear old cache:
```r
medsim::medsim_cache_clear(\"cache\", max_age = 30)
```

List cache files:
```r
medsim::medsim_cache_list(\"cache\")
```

## Notes

- Cache files are in RDS format (compressed R objects)
- Automatically managed by medsim package
- Safe to delete (will be regenerated as needed)
"

      writeLines(readme_content, readme_file)
      message(sprintf("✓ Created README: %s", readme_file))
    }
  }

  invisible(cache_dir)
}
