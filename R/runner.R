# Simulation Execution Engine
#
# Core simulation runner functions adapted from product-of-three patterns

#' Run Simulation Study
#'
#' @description
#' Executes a complete simulation study by running a user-defined method across
#' multiple scenarios and replications. Supports parallel processing and
#' automatic result aggregation.
#'
#' @param method Function: User-defined simulation method. Must accept two
#'   arguments: `data` (data.frame) and `params` (list). Should return a named
#'   list of results.
#' @param scenarios List of scenario objects from [medsim_scenario()] or
#'   [medsim_scenarios_mediation()]
#' @param config Configuration object from [medsim_config()]
#' @param compute_truth Function: Optional function to compute ground truth.
#'   Takes same arguments as `method`. If NULL, no ground truth computed.
#' @param parallel Logical: Use parallel processing (default TRUE)
#' @param verbose Logical: Print progress messages (default TRUE)
#'
#' @return A medsim_results object (list) containing:
#'   - `results`: data.frame with all simulation results
#'   - `summary`: data.frame with summary statistics
#'   - `truth`: data.frame with ground truth values (if compute_truth provided)
#'   - `config`: configuration used
#'   - `scenarios`: scenarios used
#'   - `method_name`: name of method function
#'   - `timestamp`: when simulation was run
#'
#' @details
#' ## Method Function Requirements
#'
#' The `method` function must:
#' - Accept `data` (data.frame) as first argument
#' - Accept `params` (list) as second argument
#' - Return a named list with at least one numeric element
#'
#' Example:
#' ```r
#' my_method <- function(data, params) {
#'   fit_m <- lm(M ~ X, data = data)
#'   fit_y <- lm(Y ~ X + M, data = data)
#'
#'   a <- coef(fit_m)["X"]
#'   b <- coef(fit_y)["M"]
#'
#'   list(
#'     indirect = a * b,
#'     a_path = a,
#'     b_path = b,
#'     se_indirect = ...,  # Optional
#'     ci_lower = ...,     # Optional
#'     ci_upper = ...      # Optional
#'   )
#' }
#' ```
#'
#' ## Parallel Processing
#'
#' When `parallel = TRUE`:
#' - Uses [medsim_run_parallel()] internally
#' - Number of cores from `config$n_cores`
#' - Progress bars shown (local) or suppressed (cluster)
#' - All necessary objects exported to workers
#'
#' ## Ground Truth
#'
#' If `compute_truth` is provided:
#' - Computed once per scenario (cached automatically)
#' - Used to calculate errors/bias in results
#' - Can use Monte Carlo or analytical methods
#'
#' @examples
#' \dontrun{
#' # Define method
#' my_method <- function(data, params) {
#'   fit_m <- lm(M ~ X, data = data)
#'   fit_y <- lm(Y ~ X + M, data = data)
#'   list(indirect = coef(fit_m)["X"] * coef(fit_y)["M"])
#' }
#'
#' # Configure and run
#' config <- medsim_config("local")
#' scenarios <- medsim_scenarios_mediation()
#'
#' results <- medsim_run(
#'   method = my_method,
#'   scenarios = scenarios,
#'   config = config
#' )
#'
#' # View results
#' print(results)
#' summary(results)
#' }
#'
#' @seealso [medsim_config()], [medsim_scenario()], [medsim_run_parallel()]
#'
#' @export
medsim_run <- function(method,
                       scenarios,
                       config,
                       compute_truth = NULL,
                       parallel = TRUE,
                       verbose = TRUE) {

  # --- Input Validation ---
  if (!is.function(method)) {
    stop("method must be a function")
  }

  if (!is.list(scenarios)) {
    stop("scenarios must be a list")
  }

  if (!inherits(config, "medsim_config")) {
    stop("config must be a medsim_config object from medsim_config()")
  }

  # Set random seed
  set.seed(config$seed)

  # Create output directory
  dir.create(config$output_dir, showWarnings = FALSE, recursive = TRUE)

  # Print header
  if (verbose) {
    cat("\n")
    cat("========================================================\n")
    cat("  Simulation Study\n")
    cat("========================================================\n\n")

    cat(sprintf("Method:              %s\n", deparse(substitute(method))[1]))
    cat(sprintf("Scenarios:           %d\n", length(scenarios)))
    cat(sprintf("Replications:        %d\n", config$n_replications))
    cat(sprintf("Total iterations:    %d\n",
                length(scenarios) * config$n_replications))
    cat(sprintf("Parallel:            %s\n", ifelse(parallel, "Yes", "No")))
    if (parallel) {
      cat(sprintf("Cores:               %d\n", config$n_cores))
    }
    cat("\n")
  }

  # --- Step 1: Compute Ground Truth (if requested) ---
  truth_results <- NULL

  if (!is.null(compute_truth)) {
    if (verbose) {
      cat("[STEP 1] Computing ground truth...\n")
    }

    truth_results <- medsim_compute_all_truth(
      scenarios = scenarios,
      truth_function = compute_truth,
      config = config,
      parallel = parallel,
      verbose = verbose
    )

    if (verbose) {
      cat(sprintf("  Ground truth computed for %d scenarios\n",
                  length(scenarios)))
      cat("\n")
    }
  }

  # --- Step 2: Run Simulation Replications ---
  if (verbose) {
    cat("[STEP 2] Running simulation replications...\n\n")
  }

  all_results <- list()

  for (s_idx in seq_along(scenarios)) {
    scenario <- scenarios[[s_idx]]

    if (verbose) {
      cat(sprintf("Scenario %d/%d: %s\n",
                  s_idx, length(scenarios), scenario$name))
    }

    # Create parameter grid
    param_grid <- data.frame(
      scenario_idx = s_idx,
      replication = 1:config$n_replications,
      stringsAsFactors = FALSE
    )

    # Run replications
    if (parallel) {
      scenario_results <- medsim_run_parallel(
        tasks = 1:nrow(param_grid),
        fun = function(i) {
          medsim_run_single_replication(
            scenario = scenario,
            rep_id = param_grid$replication[i],
            method = method,
            config = config
          )
        },
        n_cores = config$n_cores,
        progress = verbose,
        export = c("scenario", "method", "config",
                   "medsim_run_single_replication"),
        packages = NULL  # Add if method requires specific packages
      )
    } else {
      scenario_results <- lapply(1:nrow(param_grid), function(i) {
        medsim_run_single_replication(
          scenario = scenario,
          rep_id = param_grid$replication[i],
          method = method,
          config = config
        )
      })
    }

    # Combine results
    scenario_results_df <- do.call(rbind, scenario_results)
    all_results[[s_idx]] <- scenario_results_df

    # Save intermediate results
    if (!is.null(config$output_dir)) {
      intermediate_file <- file.path(
        config$output_dir,
        sprintf("results_scenario_%d.csv", s_idx)
      )
      write.csv(scenario_results_df, intermediate_file, row.names = FALSE)

      if (verbose) {
        cat(sprintf("  Saved to: %s\n", intermediate_file))
      }
    }

    if (verbose) {
      cat("\n")
    }
  }

  # Combine all results
  all_results_df <- do.call(rbind, all_results)

  # --- Step 3: Store Ground Truth (if available) ---
  # Note: truth_results columns are plain (no _truth suffix)
  # The analyze.R functions will add suffixes during their merge operations
  # We don't merge here - just store truth separately
  truth_combined <- truth_results

  # --- Step 4: Compute Summary Statistics ---
  if (verbose) {
    cat("[STEP 4] Computing summary statistics...\n")
  }

  summary_stats <- medsim_summarize_results(all_results_df)

  # --- Step 5: Save Final Results ---
  if (verbose) {
    cat("[STEP 5] Saving final results...\n")
  }

  if (!is.null(config$output_dir)) {
    write.csv(all_results_df,
              file.path(config$output_dir, "all_results.csv"),
              row.names = FALSE)

    write.csv(summary_stats,
              file.path(config$output_dir, "summary_stats.csv"),
              row.names = FALSE)

    if (!is.null(truth_results)) {
      write.csv(truth_results,
                file.path(config$output_dir, "truth.csv"),
                row.names = FALSE)
    }

    if (verbose) {
      cat(sprintf("\nResults saved to: %s\n", config$output_dir))
    }
  }

  # Create results object
  results <- list(
    results = all_results_df,
    summary = summary_stats,
    truth = truth_combined,  # Use truth_combined (may be NULL if no compute_truth)
    config = config,
    scenarios = scenarios,
    method_name = deparse(substitute(method))[1],
    timestamp = Sys.time()
  )

  class(results) <- c("medsim_results", "list")

  if (verbose) {
    cat("\n========================================================\n")
    cat("  Simulation Complete!\n")
    cat("========================================================\n\n")
  }

  return(results)
}

#' Run Single Simulation Replication
#'
#' @description
#' Internal function to run a single replication of a simulation. This is
#' called repeatedly by [medsim_run()].
#'
#' @param scenario Scenario object
#' @param rep_id Replication ID number
#' @param method User-defined method function
#' @param config Configuration object
#'
#' @return data.frame with results for this replication
#'
#' @keywords internal
medsim_run_single_replication <- function(scenario, rep_id, method, config) {

  # Generate data
  data <- scenario$data_generator(n = config$n %||% 200)

  # Time the method
  start_time <- Sys.time()

  result <- tryCatch(
    method(data, scenario$params),
    error = function(e) {
      warning(sprintf("Method failed for %s, rep %d: %s",
                      scenario$name, rep_id, e$message))
      list(error = e$message)
    }
  )

  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Ensure result is a list
  if (!is.list(result)) {
    result <- list(result = result)
  }

  # Create result data.frame
  result_df <- data.frame(
    scenario = scenario$name,
    replication = rep_id,
    elapsed = elapsed_time,
    stringsAsFactors = FALSE
  )

  # Add method results
  for (name in names(result)) {
    value <- result[[name]]
    if (length(value) == 1 && (is.numeric(value) || is.character(value))) {
      result_df[[name]] <- value
    }
  }

  return(result_df)
}

#' Compute Ground Truth for All Scenarios
#'
#' @description
#' Internal function to compute ground truth across all scenarios using
#' caching to avoid recomputation.
#'
#' @param scenarios List of scenario objects
#' @param truth_function Function to compute ground truth
#' @param config Configuration object
#' @param parallel Use parallel processing
#' @param verbose Print progress messages
#'
#' @return data.frame with ground truth for each scenario
#'
#' @keywords internal
medsim_compute_all_truth <- function(scenarios,
                                     truth_function,
                                     config,
                                     parallel = TRUE,
                                     verbose = TRUE) {

  truth_list <- list()

  for (s_idx in seq_along(scenarios)) {
    scenario <- scenarios[[s_idx]]

    if (verbose) {
      cat(sprintf("  Scenario %d/%d: %s\n",
                  s_idx, length(scenarios), scenario$name))
    }

    # Create cache file path
    cache_file <- file.path(
      config$output_dir,
      sprintf("truth_scenario_%d.rds", s_idx)
    )

    # Try to load from cache
    truth_value <- medsim_cache_load(cache_file)

    if (is.null(truth_value)) {
      # Compute truth
      if (verbose) {
        cat("    Computing ground truth...\n")
      }

      # Generate large sample for truth
      n_truth <- config$n_truth %||% 10000
      data_truth <- scenario$data_generator(n = n_truth)

      truth_result <- tryCatch(
        truth_function(data_truth, scenario$params),
        error = function(e) {
          warning(sprintf("Truth computation failed for %s: %s",
                          scenario$name, e$message))
          list(error = e$message)
        }
      )

      # Cache result
      medsim_cache_save(truth_result, cache_file)
      truth_value <- truth_result

      if (verbose) {
        cat("    Cached for future use\n")
      }
    } else {
      if (verbose) {
        cat("    Loaded from cache\n")
      }
    }

    # Create truth data.frame
    truth_df <- data.frame(
      scenario = scenario$name,
      stringsAsFactors = FALSE
    )

    # Add truth values (WITHOUT _truth suffix - analyze.R adds it during merge)
    if (is.list(truth_value)) {
      for (name in names(truth_value)) {
        value <- truth_value[[name]]
        if (length(value) == 1 && is.numeric(value)) {
          truth_df[[name]] <- value
        }
      }
    }

    truth_list[[s_idx]] <- truth_df
  }

  # Combine all truth results
  truth_combined <- do.call(rbind, truth_list)

  return(truth_combined)
}

#' Summarize Simulation Results
#'
#' @description
#' Computes summary statistics from simulation results including mean, median,
#' standard deviation, and quantiles.
#'
#' @param results data.frame of simulation results
#' @param by Character vector: columns to group by (default: "scenario")
#'
#' @return data.frame with summary statistics
#'
#' @keywords internal
medsim_summarize_results <- function(results, by = "scenario") {

  # Identify numeric columns (excluding grouping variables and metadata)
  exclude_cols <- c(by, "scenario_idx", "replication", "time_elapsed")
  numeric_cols <- names(results)[sapply(results, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, exclude_cols)

  if (length(numeric_cols) == 0) {
    warning("No numeric columns to summarize")
    return(data.frame())
  }

  # Compute summary statistics
  summary_list <- list()

  for (col in numeric_cols) {
    col_summary <- aggregate(
      results[[col]],
      by = results[by],
      FUN = function(x) {
        c(
          mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          q25 = quantile(x, 0.25, na.rm = TRUE),
          q75 = quantile(x, 0.75, na.rm = TRUE)
        )
      }
    )

    # Convert to data.frame
    col_summary_df <- data.frame(col_summary[[1]], col_summary$x)
    names(col_summary_df) <- c(by, paste0(col, "_",
                                          c("mean", "median", "sd", "min",
                                            "max", "q25", "q75")))

    summary_list[[col]] <- col_summary_df
  }

  # Merge all summaries
  summary_combined <- Reduce(
    function(x, y) merge(x, y, by = by, all = TRUE),
    summary_list
  )

  return(summary_combined)
}

#' Print Simulation Results
#'
#' @param x A medsim_results object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_results <- function(x, ...) {
  cat("\n")
  cat("Simulation Results\n")
  cat("==================\n\n")

  cat(sprintf("Method:        %s\n", x$method_name))
  cat(sprintf("Scenarios:     %d\n", length(x$scenarios)))
  cat(sprintf("Replications:  %d\n", x$config$n_replications))
  cat(sprintf("Total runs:    %d\n", nrow(x$results)))
  cat(sprintf("Timestamp:     %s\n", format(x$timestamp)))
  cat("\n")

  cat("Results saved to:", x$config$output_dir, "\n")
  cat("  - all_results.csv\n")
  cat("  - summary_stats.csv\n")

  if (!is.null(x$truth)) {
    cat("  - truth.csv\n")
  }

  cat("\n")
  cat("Use summary() for detailed statistics\n")
  cat("\n")

  invisible(x)
}

#' Summarize Simulation Results
#'
#' @param object A medsim_results object
#' @param ... Additional arguments (ignored)
#'
#' @return Summary data.frame
#'
#' @export
summary.medsim_results <- function(object, ...) {
  cat("\n")
  cat("Simulation Summary Statistics\n")
  cat("==============================\n\n")

  print(object$summary, row.names = FALSE)

  cat("\n")

  invisible(object$summary)
}
