# Parallel Processing Utilities
#
# Parallel execution with progress reporting and error handling

#' Run Tasks in Parallel
#'
#' @description
#' Executes a list of tasks in parallel using multiple CPU cores. Provides
#' progress bars, proper error handling, and automatic cluster management.
#'
#' @param tasks Vector or list: Tasks to process (e.g., 1:100, list of params)
#' @param fun Function: Function to apply to each task. Should accept one
#'   argument (the task) and return a result.
#' @param n_cores Integer: Number of CPU cores to use. If NULL, uses all
#'   available cores minus 2.
#' @param progress Logical: Show progress bar (default TRUE). Automatically
#'   suppressed when running on cluster (batch jobs).
#' @param export Character vector: Names of objects to export to workers.
#'   These objects must be available in the calling environment.
#' @param packages Character vector: Names of packages to load on each worker.
#'   Use this if `fun` requires specific packages.
#' @param cluster_type Character: Type of cluster ("PSOCK" or "FORK"). FORK
#'   is more efficient but only works on Unix. Auto-detected by default.
#'
#' @return List: Results from applying `fun` to each task (same length as tasks)
#'
#' @details
#' ## Cluster Types
#'
#' - **PSOCK** (default on Windows): Creates separate R sessions. Requires
#'   explicit export of objects and packages. Works on all platforms.
#' - **FORK** (default on Unix): Copies current R session. More efficient,
#'   automatic object sharing, but Unix-only.
#'
#' ## Progress Bars
#'
#' Progress bars are shown in interactive sessions and local execution, but
#' suppressed on HPC clusters to avoid cluttering log files.
#'
#' ## Error Handling
#'
#' If a task fails:
#' - Error is caught and returned as an error object
#' - Other tasks continue processing
#' - Check results with `sapply(results, inherits, "error")`
#'
#' ## Memory Considerations
#'
#' Each worker process requires memory. For large tasks:
#' - Reduce `n_cores` if running out of memory
#' - Process tasks in chunks
#' - Use FORK cluster (Unix) which shares memory
#'
#' @examples
#' \dontrun{
#' # Simple parallel computation
#' results <- medsim_run_parallel(
#'   tasks = 1:100,
#'   fun = function(i) {
#'     Sys.sleep(0.1)  # Simulate work
#'     i^2
#'   },
#'   n_cores = 4
#' )
#'
#' # With exports
#' my_data <- data.frame(x = 1:10, y = 11:20)
#'
#' results <- medsim_run_parallel(
#'   tasks = 1:10,
#'   fun = function(i) {
#'     mean(my_data$x[1:i])
#'   },
#'   export = "my_data"
#' )
#'
#' # With packages
#' results <- medsim_run_parallel(
#'   tasks = 1:10,
#'   fun = function(i) {
#'     dplyr::tibble(x = i, y = i^2)
#'   },
#'   packages = "dplyr"
#' )
#'
#' # Check for errors
#' errors <- sapply(results, inherits, "error")
#' if (any(errors)) {
#'   cat("Errors occurred in:", which(errors), "\n")
#' }
#' }
#'
#' @seealso [parallel::makeCluster()], [parallel::parLapply()], [pbapply::pblapply()]
#'
#' @export
medsim_run_parallel <- function(tasks,
                                fun,
                                n_cores = NULL,
                                progress = TRUE,
                                export = NULL,
                                packages = NULL,
                                cluster_type = NULL) {

  # Validate inputs
  if (!is.function(fun)) {
    stop("fun must be a function")
  }

  if (length(tasks) == 0) {
    return(list())
  }

  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 2
    n_cores <- max(1, n_cores)
  }

  # Don't use more cores than tasks
  n_cores <- min(n_cores, length(tasks))

  # If only 1 core or very few tasks, run sequentially
  if (n_cores == 1 || length(tasks) < 4) {
    return(medsim_run_sequential(tasks, fun, progress))
  }

  # Auto-detect cluster type
  if (is.null(cluster_type)) {
    cluster_type <- if (.Platform$OS.type == "unix") "FORK" else "PSOCK"
  }

  # Suppress progress bar on cluster/batch jobs
  if (medsim_is_batch_job()) {
    progress <- FALSE
  }

  # Create cluster
  cl <- parallel::makeCluster(n_cores, type = cluster_type)

  # Ensure cluster is stopped on exit
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export objects to workers (PSOCK only)
  if (cluster_type == "PSOCK" && !is.null(export)) {
    # Get objects from parent environment
    parent_env <- parent.frame()
    export_list <- mget(export, envir = parent_env, ifnotfound = list(NULL))

    # Remove NULLs
    export_list <- export_list[!sapply(export_list, is.null)]

    if (length(export_list) > 0) {
      parallel::clusterExport(cl, names(export_list), envir = parent_env)
    }
  }

  # Load packages on workers
  if (!is.null(packages)) {
    parallel::clusterEvalQ(cl, {
      for (pkg in packages) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      }
    })
  }

  # Run tasks with progress bar
  if (progress && requireNamespace("pbapply", quietly = TRUE)) {
    # Configure pbapply
    pboptions(type = "timer", char = "=", style = 3)

    results <- pbapply::pblapply(
      X = tasks,
      FUN = function(task) {
        tryCatch(
          fun(task),
          error = function(e) {
            structure(
              list(message = e$message, task = task),
              class = c("medsim_error", "error", "condition")
            )
          }
        )
      },
      cl = cl
    )
  } else {
    # No progress bar
    results <- parallel::parLapply(
      cl = cl,
      X = tasks,
      fun = function(task) {
        tryCatch(
          fun(task),
          error = function(e) {
            structure(
              list(message = e$message, task = task),
              class = c("medsim_error", "error", "condition")
            )
          }
        )
      }
    )
  }

  return(results)
}

#' Run Tasks Sequentially
#'
#' @description
#' Runs tasks sequentially (no parallelization). Used as fallback when parallel
#' processing is not needed or not available.
#'
#' @param tasks Vector or list of tasks
#' @param fun Function to apply to each task
#' @param progress Show progress bar
#'
#' @return List of results
#'
#' @keywords internal
medsim_run_sequential <- function(tasks, fun, progress = TRUE) {

  # Suppress progress on cluster
  if (medsim_is_batch_job()) {
    progress <- FALSE
  }

  if (progress && requireNamespace("pbapply", quietly = TRUE)) {
    pboptions(type = "timer", char = "=", style = 3)

    results <- pbapply::pblapply(
      X = tasks,
      FUN = function(task) {
        tryCatch(
          fun(task),
          error = function(e) {
            structure(
              list(message = e$message, task = task),
              class = c("medsim_error", "error", "condition")
            )
          }
        )
      }
    )
  } else {
    results <- lapply(tasks, function(task) {
      tryCatch(
        fun(task),
        error = function(e) {
          structure(
            list(message = e$message, task = task),
            class = c("medsim_error", "error", "condition")
          )
        }
      )
    })
  }

  return(results)
}

#' Check if Running as Batch Job
#'
#' @description
#' Detects if code is running as a batch job on an HPC cluster. Used to
#' suppress progress bars that clutter log files.
#'
#' @return Logical: TRUE if running as batch job, FALSE if interactive
#'
#' @details
#' Checks for:
#' - Interactive session: `interactive()`
#' - HPC scheduler environment variables (SLURM_JOB_ID, PBS_JOBID, LSB_JOBID)
#'
#' @keywords internal
medsim_is_batch_job <- function() {
  # If interactive, definitely not a batch job
  if (interactive()) {
    return(FALSE)
  }

  # Check for scheduler environment variables
  scheduler_vars <- c("SLURM_JOB_ID", "PBS_JOBID", "LSB_JOBID")

  for (var in scheduler_vars) {
    if (Sys.getenv(var) != "") {
      return(TRUE)
    }
  }

  # Non-interactive and no scheduler = likely batch
  return(TRUE)
}

#' Get Optimal Number of Cores
#'
#' @description
#' Determines the optimal number of cores to use for parallel processing based
#' on system capabilities and task requirements.
#'
#' @param n_tasks Integer: Number of tasks to process. If fewer tasks than
#'   cores, returns `n_tasks`.
#' @param reserve Integer: Number of cores to reserve for system (default 2).
#'   Only applied on local machines, not clusters.
#' @param max_cores Integer: Maximum number of cores to use. If NULL, uses
#'   all available (minus reserve).
#'
#' @return Integer: Optimal number of cores to use
#'
#' @examples
#' # For 100 tasks
#' medsim_get_optimal_cores(n_tasks = 100)
#'
#' # For few tasks (returns n_tasks)
#' medsim_get_optimal_cores(n_tasks = 3)
#'
#' # With maximum limit
#' medsim_get_optimal_cores(n_tasks = 1000, max_cores = 8)
#'
#' @export
medsim_get_optimal_cores <- function(n_tasks,
                                     reserve = 2,
                                     max_cores = NULL) {

  # Get total available cores
  total_cores <- parallel::detectCores()

  # Check for cluster-allocated cores
  if (Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
    total_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
    reserve <- 0  # Don't reserve on cluster
  } else if (Sys.getenv("PBS_NUM_PPN") != "") {
    total_cores <- as.numeric(Sys.getenv("PBS_NUM_PPN"))
    reserve <- 0
  } else if (Sys.getenv("LSB_DJOB_NUMPROC") != "") {
    total_cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))
    reserve <- 0
  }

  # Calculate available cores
  available_cores <- total_cores - reserve
  available_cores <- max(1, available_cores)

  # Apply max_cores limit if specified
  if (!is.null(max_cores)) {
    available_cores <- min(available_cores, max_cores)
  }

  # Don't use more cores than tasks
  optimal_cores <- min(available_cores, n_tasks)

  return(optimal_cores)
}

#' Setup Progress Bar Options
#'
#' @description
#' Configures pbapply package options for progress bars. Automatically adjusts
#' based on execution environment (interactive vs batch).
#'
#' @param style Character: Progress bar style ("timer", "txt", "none")
#' @param char Character: Character to use for progress bar (default "=")
#'
#' @return Invisibly returns previous options
#'
#' @keywords internal
medsim_setup_progress <- function(style = NULL, char = "=") {

  if (!requireNamespace("pbapply", quietly = TRUE)) {
    return(invisible(NULL))
  }

  # Auto-detect style
  if (is.null(style)) {
    style <- if (medsim_is_batch_job()) "none" else "timer"
  }

  # Set options
  old_options <- pbapply::pboptions(
    type = style,
    char = char,
    style = 3
  )

  invisible(old_options)
}

#' Estimate Parallel Speedup
#'
#' @description
#' Estimates the expected speedup from parallel processing based on number of
#' cores and task characteristics.
#'
#' @param n_tasks Integer: Number of tasks
#' @param n_cores Integer: Number of cores
#' @param overhead Numeric: Overhead per core (default 0.1 = 10%)
#'
#' @return Named list with:
#'   - `speedup`: Expected speedup factor
#'   - `efficiency`: Parallel efficiency (0-1)
#'   - `recommendation`: Character recommendation
#'
#' @details
#' Speedup is estimated using Amdahl's law with overhead:
#'
#' Speedup = n_cores / (1 + overhead * n_cores)
#'
#' Efficiency = Speedup / n_cores
#'
#' @examples
#' # For 100 tasks on 10 cores
#' est <- medsim_estimate_speedup(100, 10)
#' cat(sprintf("Expected speedup: %.1fx\n", est$speedup))
#' cat(sprintf("Efficiency: %.0f%%\n", est$efficiency * 100))
#'
#' @export
medsim_estimate_speedup <- function(n_tasks, n_cores, overhead = 0.1) {

  # Don't expect speedup if more cores than tasks
  effective_cores <- min(n_cores, n_tasks)

  # Estimate speedup with overhead
  speedup <- effective_cores / (1 + overhead * effective_cores)

  # Calculate efficiency
  efficiency <- speedup / effective_cores

  # Make recommendation
  if (efficiency > 0.8) {
    recommendation <- "Excellent parallel efficiency"
  } else if (efficiency > 0.6) {
    recommendation <- "Good parallel efficiency"
  } else if (efficiency > 0.4) {
    recommendation <- "Moderate parallel efficiency"
  } else {
    recommendation <- "Consider reducing n_cores or increasing task size"
  }

  return(list(
    speedup = speedup,
    efficiency = efficiency,
    recommendation = recommendation
  ))
}

#' Check Results for Errors
#'
#' @description
#' Checks a list of results from parallel execution for errors and provides
#' a summary.
#'
#' @param results List of results from [medsim_run_parallel()]
#' @param stop_on_error Logical: If TRUE, stops with error if any task failed
#'
#' @return Invisibly returns indices of failed tasks
#'
#' @examples
#' \dontrun{
#' results <- medsim_run_parallel(...)
#' medsim_check_results(results)
#' }
#'
#' @export
medsim_check_results <- function(results, stop_on_error = FALSE) {

  # Find errors
  is_error <- sapply(results, inherits, "medsim_error")
  n_errors <- sum(is_error)
  n_total <- length(results)

  if (n_errors == 0) {
    message(sprintf("* All %d tasks completed successfully", n_total))
    return(invisible(integer(0)))
  }

  # Report errors
  error_indices <- which(is_error)

  warning(sprintf(
    "%d/%d tasks failed (%.1f%%)",
    n_errors, n_total, 100 * n_errors / n_total
  ))

  # Print first few error messages
  n_show <- min(3, n_errors)
  for (i in 1:n_show) {
    idx <- error_indices[i]
    err <- results[[idx]]
    message(sprintf(
      "  Task %d: %s",
      idx,
      substr(err$message, 1, 60)
    ))
  }

  if (n_errors > n_show) {
    message(sprintf("  ... and %d more errors", n_errors - n_show))
  }

  if (stop_on_error) {
    stop(sprintf("%d tasks failed", n_errors))
  }

  invisible(error_indices)
}
