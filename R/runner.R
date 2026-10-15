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
#' @param truth_on_failure `"stop"` (default) or `"warn"`: what to do when
#'   `compute_truth` errors, or succeeds but returns no usable parameter, for a
#'   scenario. `"stop"` aborts before any replication runs; `"warn"` continues
#'   with the scenario omitted from `$truth`. Nothing is cached either way. See
#'   *Ground Truth*.
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
#' - NOT use the reserved field name `error` -- it marks failed replications
#'   in the results schema (a non-NA `error` forces `converged = 0`); a
#'   method returning it stops with an informative error
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
#' `compute_truth` must return a **named list of length-1 numerics**, e.g.
#' `list(indirect = 0.09)`. A bare named vector (`c(indirect = 0.09)`) or a list
#' element of length > 1 contributes no parameter and is treated as a failure.
#'
#' A failed or unusable truth is never cached, so the next run retries it, and
#' the scenario is left out of `$truth`. What happens to the run is set by
#' `truth_on_failure`: `"stop"` (the default) raises a
#' `medsim_truth_failure_error` (subclass of `medsim_error`) carrying
#' `$scenario` and `$parent`; `"warn"` warns and continues. Truth is computed in
#' Step 1, before any replication runs, so stopping wastes no replication
#' compute.
#'
#' ## Task-level failures
#'
#' An error that escapes a replication task -- a contract violation (e.g. the
#' reserved-`error`-field stop) or an infrastructure error, as opposed to a
#' `method()` or `data_generator()` failure (both of which become failure
#' rows and let the run continue) -- stops [medsim_run()] with a
#' `medsim_run_failure` condition (subclass of `medsim_error`) rather than
#' silently dropping the failed replications. The condition carries whatever
#' completed before the failure: `$results` is a partial `medsim_results`
#' object (prior scenarios complete, the failing scenario's successful reps
#' included, later scenarios never started), and `$errors` is a list of
#' `list(scenario, global_rep_id, message)`. Recover the partial run instead
#' of losing it:
#' ```r
#' partial <- tryCatch(
#'   medsim_run(method, scenarios, config),
#'   medsim_run_failure = function(e) e$results
#' )
#' ```
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
                       verbose = TRUE,
                       truth_on_failure = c("stop", "warn")) {
  truth_on_failure <- match.arg(truth_on_failure)

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

  # NOTE: this does NOT seed the replication draws -- each replication
  # re-seeds deterministically from (scenario_name, global_rep_id) via
  # .medsim_det_seed() (see medsim_run_single_replication), so replication
  # independence is invariant to config$seed. This set.seed only affects the
  # compute_truth pass below (Step 1), where it keeps ground-truth computation
  # reproducible and identical across chunks. config$seed is otherwise
  # provenance-only (see medsim_config() / medsim_run_parallel() docs).
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
      verbose = verbose,
      truth_on_failure = truth_on_failure
    )

    if (verbose) {
      cat(sprintf("  Ground truth computed for %d of %d scenarios\n",
                  if (is.null(truth_results)) 0L else nrow(truth_results),
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

    # Create parameter grid. seq_len (not 1:n) so an empty chunk
    # (n_replications == 0, e.g. n_chunks > total reps) yields ZERO rows rather
    # than the `1:0 == c(1,0)` footgun that fabricates 2 phantom replications.
    # scenario_idx uses rep() (not a length-1 scalar) so it is length 0 too when
    # n_replications == 0 -- a length-1 column cannot recycle into a 0-row frame.
    reps <- seq_len(config$n_replications)
    param_grid <- data.frame(
      scenario_idx = rep(s_idx, length(reps)),
      replication = reps,
      stringsAsFactors = FALSE
    )

    # Run replications
    if (parallel) {
      scenario_results <- medsim_run_parallel(
        tasks = seq_len(nrow(param_grid)),
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
      # Wrapped exactly like medsim_run_parallel()'s own per-task tryCatch
      # (#43 R3/case 6: serial and parallel must produce the SAME classed
      # object on a task-level error -- the reserved-`error` contract stop,
      # an error inside .medsim_det_seed()/set.seed(), or one escaping
      # result_df assembly. Previously these aborted medsim_run() immediately
      # here, unlike the parallel path, where the same error was silently
      # DROPPED instead (#43's core defect) -- neither behavior is the fix;
      # the sweep below is.
      scenario_results <- lapply(seq_len(nrow(param_grid)), function(i) {
        tryCatch(
          medsim_run_single_replication(
            scenario = scenario,
            rep_id = param_grid$replication[i],
            method = method,
            config = config
          ),
          error = function(e) {
            structure(
              list(message = e$message, task = i),
              class = c("medsim_error", "error", "condition")
            )
          }
        )
      })
    }

    # Sweep for task-level errors BEFORE combining (#43 §2). A medsim_error
    # condition object reaching .medsim_rbind_reps() would be silently
    # dropped by its nrow() filter -- the core defect this fix closes. No
    # fail-fast machinery is needed (Q2): stop()ping here already unwinds out
    # of this scenario loop, so a later scenario simply never starts.
    is_task_error <- vapply(scenario_results, inherits, logical(1),
                            what = "medsim_error")
    if (any(is_task_error)) {
      good_df <- .medsim_rbind_reps(scenario_results[!is_task_error])
      all_results[[s_idx]] <- good_df
      partial_df <- .medsim_stamp_schema_v2(.medsim_rbind_reps(all_results))
      partial_summary <- if (is.null(partial_df) || nrow(partial_df) == 0L) {
        NULL
      } else {
        suppressWarnings(medsim_summarize_results(partial_df))
      }
      partial_results <- list(
        results     = partial_df,
        summary     = partial_summary,
        truth       = truth_results,
        config      = config,
        scenarios   = scenarios,
        method_name = deparse(substitute(method))[1],
        timestamp   = Sys.time()
      )
      class(partial_results) <- c("medsim_results", "list")

      failed_tasks <- scenario_results[is_task_error]
      task_ids <- vapply(failed_tasks, function(e) e$task, integer(1))
      errors <- lapply(seq_along(failed_tasks), function(k) {
        list(
          scenario      = scenario$name,
          global_rep_id = (config$rep_offset %||% 0L) +
                           param_grid$replication[task_ids[k]],
          message       = failed_tasks[[k]]$message
        )
      })

      cond <- structure(
        class = c("medsim_run_failure", "medsim_error", "error", "condition"),
        list(
          message = sprintf(paste(
            "medsim_run: scenario '%s': %d of %d replication task(s) failed",
            "with an unrecoverable error. First error (global rep %d): %s",
            "\n(Partial results are attached to this condition:",
            "tryCatch(..., medsim_run_failure = function(e) e$results).)"),
            scenario$name, length(failed_tasks), nrow(param_grid),
            errors[[1L]]$global_rep_id, errors[[1L]]$message),
          call    = NULL,
          results = partial_results,
          errors  = errors
        )
      )
      stop(cond)
    }

    # Combine results. .medsim_rbind_reps harmonizes row schemas first: a
    # failed replication's row ({..., error}) has different columns than a
    # success row ({..., indirect, ...}), and a bare rbind crashes with
    # "names do not match previous names" -- turning one transient rep failure
    # into a dead scenario (or, chunked, a missing chunk file).
    scenario_results_df <- .medsim_rbind_reps(scenario_results)
    all_results[[s_idx]] <- scenario_results_df

    # Save intermediate results. Skipped in chunk mode (config$chunk_id set):
    # n_chunks concurrent array tasks share one output_dir, so these fixed-name
    # CSVs would overwrite each other (last writer wins) and leave partial data
    # that LOOKS like a complete per-scenario record (#38). The chunk .rds
    # written by medsim_run_chunk() is the real artifact.
    if (!is.null(config$output_dir) && is.null(config$chunk_id)) {
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

  # Combine all results (same harmonization across scenarios: methods may emit
  # scenario-dependent columns).
  all_results_df <- .medsim_rbind_reps(all_results)

  all_results_df <- .medsim_stamp_schema_v2(all_results_df)

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

  # Final CSVs likewise skipped in chunk mode (#38): fixed names in a shared
  # output_dir, clobbered across concurrent array tasks, never read by combine.
  if (!is.null(config$output_dir) && is.null(config$chunk_id)) {
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

#' Deterministic Per-Replication Seed
#'
#' @description
#' Derives a seed from `(scenario_name, rep_id)` alone, independent of chunk
#' count, worker count, cluster type, or execution order. This is what makes
#' [medsim_run_chunk()] safe to split across any number of SLURM array tasks:
#' replication `k` of a given scenario draws the same data regardless of
#' which chunk or worker happens to process it.
#'
#' @param scenario_name Character: scenario name.
#' @param rep_id Integer: **global** replication id (i.e. already offset by
#'   `config$rep_offset` for chunked runs -- see [medsim_run_single_replication()]).
#'
#' @return Integer in `[1, .Machine$integer.max]`.
#'
#' @keywords internal
.medsim_det_seed <- function(scenario_name, rep_id) {
  # Polynomial rolling hash (order-sensitive -- unlike a plain character sum,
  # anagrams of a scenario name do NOT collide) over a ~1e6-bucket space.
  # medsim is shared infrastructure accumulating scenario names across many
  # studies; a name_hash collision would silently reproduce the exact
  # cross-run correlation bug this function exists to fix, just relocated
  # from "chunk boundary" to "scenario-name space".
  chars <- utf8ToInt(scenario_name)
  name_hash <- Reduce(function(acc, ch) (acc * 31L + ch) %% 1000003L, chars, 0L)
  seed <- (name_hash * 1e6) + rep_id
  as.integer(seed %% .Machine$integer.max) + 1L
}

#' Run Single Simulation Replication
#'
#' @description
#' Internal function to run a single replication of a simulation. This is
#' called repeatedly by [medsim_run()].
#'
#' @param scenario Scenario object
#' @param rep_id Replication ID number (local to the current chunk, if any --
#'   offset by `config$rep_offset` internally to get the global id, which is
#'   both used for seeding and recorded as the row's `replication` value
#'   (schema v2: `replication` is the global rep id, never chunk-local))
#' @param method User-defined method function
#' @param config Configuration object
#'
#' @return data.frame with results for this replication. A `data_generator()`
#'   failure returns a failure row (`error = "data_generator error: <msg>"`,
#'   `elapsed = NA_real_`, no `converged`) without ever calling `method()`. A
#'   `method()` failure returns a failure row with the plain error message
#'   (no prefix). Anything else escaping this function (a non-function
#'   `data_generator`, an error inside [.medsim_det_seed()]/`set.seed()`, or
#'   one during result-row assembly) is a contract/infrastructure error and
#'   propagates uncaught -- [medsim_run()]'s per-task sweep is what turns
#'   that into a `medsim_run_failure` condition, not this function.
#'
#' @keywords internal
medsim_run_single_replication <- function(scenario, rep_id, method, config) {

  # Seed deterministically from (scenario, global rep id) -- NOT from
  # config$seed alone. config$seed is a single scalar shared by every chunk
  # in a SLURM array job; without this, medsim_run_chunk() resets to the same
  # RNG state on every chunk and every chunk regenerates the same short
  # sequence of "replications" (see medsim issue: chunked coverage runs
  # silently collapsing to ~n_chunks distinct outcomes). rep_offset (set by
  # medsim_run_chunk()) shifts rep_id to its true position in the full
  # 1..n_replications sequence so chunk 2's rep 1 gets a different seed than
  # chunk 1's rep 1.
  global_rep_id <- (config$rep_offset %||% 0L) + rep_id
  set.seed(.medsim_det_seed(scenario$name, global_rep_id))
  # NOTE: global_rep_id is also what the result row records as `replication`
  # (schema v2, SPEC-medsim-chunked-run-gates-2026-07-31): one honest global id,
  # not a chunk-local one that collides across chunks (#36).

  # Contract error, not a runtime failure: a non-function data_generator
  # would otherwise silently become nsim rows of garbage inside the tryCatch
  # below instead of failing loudly here (#43 review finding). Deterministic
  # on the first replication.
  if (!is.function(scenario$data_generator)) {
    stop(sprintf(
      "scenario '%s': data_generator must be a function(n), not %s",
      scenario$name, class(scenario$data_generator)[1L]), call. = FALSE)
  }

  # Generate data. A data_generator() failure is a REAL but distinct failure
  # mode from a method() failure: the estimator never even ran. Tagged with a
  # "data_generator error: " prefix (method() failures are NOT prefixed) so
  # downstream accounting (medsim_analyze) and combine-time auditing (Gate
  # A.4's dgp_failure violation) can tell the two apart -- without the tag a
  # DGP failure looked identical to a method failure and their rates could
  # not be reported separately (#43 G1/G3).
  dgp_failed  <- FALSE
  dgp_message <- NA_character_
  data <- tryCatch(
    scenario$data_generator(n = config$n %||% 200),
    error = function(e) {
      warning(sprintf("data_generator failed for %s, rep %d: %s",
                      scenario$name, rep_id, e$message))
      dgp_failed  <<- TRUE
      dgp_message <<- e$message
      NULL
    }
  )

  if (dgp_failed) {
    # method() never runs, so this row carries NO `converged` field --
    # forcing converged = 0 here poisoned converged_mean (measured 0 with 2
    # of 3 reps actually succeeding, #43 review round 4).
    return(data.frame(
      scenario    = scenario$name,
      replication = global_rep_id,
      elapsed     = NA_real_,
      error       = paste0("data_generator error: ", dgp_message),
      stringsAsFactors = FALSE
    ))
  }

  # Time the method
  start_time <- Sys.time()

  method_failed <- FALSE
  result <- tryCatch(
    method(data, scenario$params),
    error = function(e) {
      warning(sprintf("Method failed for %s, rep %d: %s",
                      scenario$name, rep_id, e$message))
      method_failed <<- TRUE
      list(error = e$message)
    }
  )

  # `error` is a RESERVED field name: the failure-row schema uses it to mark
  # failed replications (non-NA error => converged forced to 0, column treated
  # as metadata). A method returning its own `error` field on success would be
  # silently corrupted -- refuse loudly instead. Fires deterministically on
  # the first replication, so this is an immediate contract error, not a
  # mid-run surprise.
  if (!method_failed && "error" %in% names(result)) {
    stop(sprintf(paste(
      "method() for scenario '%s' returned a field named 'error' -- that name",
      "is reserved for medsim's failure-row schema (a non-NA value marks a",
      "FAILED replication and forces converged = 0). Rename the field",
      "(e.g. 'est_error')."), scenario$name), call. = FALSE)
  }

  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Ensure result is a list
  if (!is.list(result)) {
    result <- list(result = result)
  }

  # Create result data.frame. `replication` carries the GLOBAL rep id (schema
  # v2) -- in a chunked run this is the rep's true position in 1..nsim, so ids
  # never collide across chunks and max(replication) == nsim after combine.
  result_df <- data.frame(
    scenario = scenario$name,
    replication = global_rep_id,
    elapsed = elapsed_time,
    stringsAsFactors = FALSE
  )

  # Add method results. is.logical() is included so the documented method()
  # contract fields (branch_switch, converged -- often logical NA/TRUE/FALSE)
  # survive to $results instead of being silently dropped.
  for (name in names(result)) {
    value <- result[[name]]
    if (length(value) == 1 &&
        (is.numeric(value) || is.character(value) || is.logical(value))) {
      result_df[[name]] <- value
    }
  }

  return(result_df)
}

#' Content fingerprint of a scenario's truth inputs
#'
#' @description
#' Serialized identity of everything that determines a scenario's ground truth:
#' name, params (by value), and the *deparsed source* of the data-generator and
#' the truth function. Used by [medsim_compute_all_truth()] to invalidate an
#' index-keyed truth cache on any content change (guards against stale-truth
#' reuse when an `output_dir` is reused after a scenario changes).
#'
#' Deparse (source text) is used rather than serializing the live closures on
#' purpose: a closure's `serialize()` includes its whole enclosing environment,
#' which mutates as the caller adds unrelated bindings -- that would make the
#' fingerprint unstable across two identical calls and defeat legitimate cache
#' reuse. Deparse is environment-independent, and the *values* a data-generator
#' depends on live in `params` (medsim convention), which is fingerprinted by
#' value. Residual limitation: a generator that closes over a value NOT in
#' `params` (e.g. `local({ m <- 5; function(n) rnorm(n, m) })`) with an identical
#' body and identical `params` would not be distinguished by a change to `m`;
#' put such parameters in `params` for them to be tracked.
#'
#' @param scenario A `medsim_scenario`.
#' @param truth_function The truth function passed to `medsim_run()`.
#' @return A raw vector (compared with `identical()`).
#' @keywords internal
.medsim_truth_fingerprint <- function(scenario, truth_function) {
  serialize(
    list(name   = scenario$name,
         params = scenario$params,
         dgm    = deparse(scenario$data_generator),
         truth  = deparse(truth_function)),
    connection = NULL
  )
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
#' @param truth_on_failure `"warn"` or `"stop"`; see [medsim_run()].
#'
#' @return data.frame with ground truth for each scenario whose truth
#'   succeeded (failed scenarios are absent), or `NULL` if none did.
#'
#' @keywords internal
medsim_compute_all_truth <- function(scenarios,
                                     truth_function,
                                     config,
                                     parallel = TRUE,
                                     verbose = TRUE,
                                     truth_on_failure = c("stop", "warn")) {
  truth_on_failure <- match.arg(truth_on_failure)

  truth_list <- vector("list", length(scenarios))

  for (s_idx in seq_along(scenarios)) {
    scenario <- scenarios[[s_idx]]

    if (verbose) {
      cat(sprintf("  Scenario %d/%d: %s\n",
                  s_idx, length(scenarios), scenario$name))
    }

    # Create cache file path (keyed by index; content validated by fingerprint
    # below -- the filename alone is NOT a safe key, see fingerprint note).
    cache_file <- file.path(
      config$output_dir,
      sprintf("truth_scenario_%d.rds", s_idx)
    )

    # Content fingerprint: reusing an output_dir after changing a scenario's
    # DGM/params must NOT reload stale truth. Serializing name+params+DGM+truth
    # captures closure values too (e.g. a data_generator that closes over mu via
    # local()), so an anagram-of-body or a changed closed-over constant both
    # invalidate. The cached object is list(truth=, fingerprint=); a legacy bare
    # cache (no $fingerprint) is treated as a miss and recomputed (safe).
    fingerprint <- .medsim_truth_fingerprint(scenario, truth_function)
    cached <- medsim_cache_load(cache_file)
    # A cached failure sentinel is a MISS (#41 D5): pre-#41 medsim wrote the
    # unclassed list(error = <msg>) to disk, and loading it would poison every
    # later run with the same fingerprint. Recompute instead (self-heal).
    truth_value <- if (is.list(cached) && !is.null(cached$fingerprint) &&
                       identical(cached$fingerprint, fingerprint) &&
                       !.medsim_is_truth_sentinel(cached$truth)) {
      cached$truth
    } else {
      NULL
    }
    from_cache <- !is.null(truth_value)

    if (is.null(truth_value)) {
      # Compute truth
      if (verbose) {
        cat("    Computing ground truth...\n")
      }

      # Generate large sample for truth
      n_truth <- config$n_truth %||% 10000
      data_truth <- scenario$data_generator(n = n_truth)

      truth_value <- tryCatch(
        truth_function(data_truth, scenario$params),
        error = function(e) {
          structure(list(error = conditionMessage(e)), parent = e,
                    class = "medsim_truth_failure")
        }
      )
    } else if (verbose) {
      cat("    Loaded from cache\n")
    }

    if (inherits(truth_value, "medsim_truth_failure")) {
      # Never cached (#41 D5): the next run retries.
      .medsim_truth_failed(
        scenario$name, truth_on_failure,
        sprintf("Truth computation failed for %s: %s",
                scenario$name, truth_value$error),
        parent = attr(truth_value, "parent"))
      next
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

    # A truth function can SUCCEED and still contribute nothing (#41 D8): a bare
    # named vector, or list elements longer than 1, are dropped above.
    if (ncol(truth_df) == 1L) {
      .medsim_truth_failed(
        scenario$name, truth_on_failure,
        sprintf(paste(
          "Truth for %s has no usable parameter: compute_truth() returned %s.",
          "It must return a NAMED LIST of length-1 numerics, e.g.",
          "list(indirect = 0.09); a bare named vector or a list element of",
          "length > 1 is dropped."),
          scenario$name, .medsim_describe_value(truth_value)),
        parent = NULL)
      next
    }

    if (!from_cache) {
      # Cache result WITH its fingerprint so a later content change invalidates.
      medsim_cache_save(list(truth = truth_value, fingerprint = fingerprint),
                        cache_file)
      if (verbose) {
        cat("    Cached for future use\n")
      }
    }

    truth_list[[s_idx]] <- truth_df
  }

  # Failed scenarios are ABSENT (not present-with-fewer-columns), and scenarios
  # may return different parameter sets: bind on the column union (#41 R4).
  .medsim_rbind_union(truth_list)
}

# A truth failure sentinel: the classed one this version builds in memory, or
# the unclassed list(error = <chr>) that medsim < 0.6.0 wrote to the cache.
# The legacy arm requires a CHARACTER error, so a legitimate numeric truth
# parameter named `error` is not mistaken for a failure (#41 R1, case 9).
.medsim_is_truth_sentinel <- function(x) {
  inherits(x, "medsim_truth_failure") ||
    (is.list(x) && identical(names(x), "error") && is.character(x$error))
}

# Raise or warn for a scenario whose truth failed or was unusable.
.medsim_truth_failed <- function(scenario, truth_on_failure, message, parent) {
  if (truth_on_failure == "stop") {
    stop(structure(
      class = c("medsim_truth_failure_error", "medsim_error", "error", "condition"),
      list(message = paste(message,
                           "(truth_on_failure = \"warn\" continues without it)"),
           call = NULL, scenario = scenario, parent = parent)))
  }
  warning(paste(message, "-- the scenario is left out of $truth and not cached."),
          call. = FALSE)
}

.medsim_describe_value <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    lens <- vapply(x, length, integer(1))
    sprintf("a list with element length(s) %s", paste(lens, collapse = ", "))
  } else {
    sprintf("a %s of length %d", class(x)[1L], length(x))
  }
}

# rbind data.frames on the union of their columns, NA-filling absent ones.
# NULL entries are dropped; an empty input returns NULL.
.medsim_rbind_union <- function(frames) {
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0L) return(NULL)
  cols <- unique(unlist(lapply(frames, names)))
  frames <- lapply(frames, function(d) {
    for (col in setdiff(cols, names(d))) d[[col]] <- NA_real_
    d[cols]
  })
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
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

  # An all-NA cell (e.g. every replication in a scenario is a
  # data_generator() failure, #43) makes min(x, na.rm = TRUE)/max(...) warn
  # "no non-missing arguments" and return +-Inf. NA_real_ is the honest value
  # for "nothing to summarize here", not a fabricated infinity.
  safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

  for (col in numeric_cols) {
    col_summary <- aggregate(
      results[[col]],
      by = results[by],
      FUN = function(x) {
        c(
          mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          min = safe_min(x),
          max = safe_max(x),
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

#' Stamp a combined results frame with the schema-v2 attributes
#'
#' @description
#' Shared between [medsim_run()]'s normal completion and its `medsim_run_failure`
#' partial payload (#43) -- both need the SAME schema-v2 stamp
#' (`replication` is the GLOBAL rep id) + `medsim_meta_cols` provenance, so
#' combine-time auditing treats a partial run exactly like a complete one.
#' @param df A combined results data.frame, or `NULL`.
#' @return `df` with the schema-v2 attributes set, or `NULL` unchanged.
#' @keywords internal
.medsim_stamp_schema_v2 <- function(df) {
  if (is.null(df)) return(df)
  attr(df, "medsim_schema") <- 2L
  attr(df, "medsim_meta_cols") <-
    intersect(c("scenario", "replication", "elapsed", "error"), names(df))
  df
}

#' Schema-harmonized rbind of replication rows
#'
#' @description
#' Failed replications produce rows whose columns differ from success rows
#' (a failure row carries `error` but no estimate columns), and a bare
#' `do.call(rbind, ...)` crashes on the mismatch -- one transient rep failure
#' used to kill the entire scenario (chunked: the whole chunk file).
#'
#' This helper unions the column sets, fills absent columns with `NA`,
#' guarantees an `error` character column on EVERY row (`NA` on success), and
#' sets `converged = 0` on failure rows when a `converged` column exists --
#' so all rows share one schema and downstream code sees the documented
#' failure semantics instead of a crash.
#'
#' @param rows List of single-row (or multi-row) result data.frames.
#' @return One combined data.frame (0-row frames are dropped).
#' @keywords internal
.medsim_rbind_reps <- function(rows) {
  # Defense in depth (#43 D3/§3): real callers can no longer reach this after
  # medsim_run()'s per-scenario sweep intercepts task-level errors first, but
  # stopping loud here beats nrow(<condition object>) silently evaluating to
  # NULL and the failed replication vanishing through the filter below.
  # Pinned by a direct unit test since normal use can no longer trigger it.
  if (any(vapply(rows, inherits, logical(1), what = "medsim_error"))) {
    stop(paste(
      ".medsim_rbind_reps: received a medsim_error condition object --",
      "the caller must intercept task-level errors before combining rows",
      "(see medsim_run()'s per-scenario sweep)."), call. = FALSE)
  }
  rows <- Filter(function(d) !is.null(d) && nrow(d) > 0L, rows)
  # NULL (not a 0x0 data.frame) preserves the pre-existing empty-chunk
  # semantics: do.call(rbind, list()) was NULL, and downstream code
  # (medsim_summarize_results, saveRDS of an empty chunk) guards on NULL.
  if (length(rows) == 0L) return(NULL)

  all_cols <- unique(unlist(lapply(rows, names)))
  if (!("error" %in% all_cols)) all_cols <- c(all_cols, "error")

  rows <- lapply(rows, function(d) {
    for (col in setdiff(all_cols, names(d))) {
      d[[col]] <- if (col == "error") NA_character_ else NA
    }
    d[all_cols]
  })

  out <- do.call(rbind, rows)

  # ROW-wise failure semantics (this helper is applied twice -- per scenario,
  # then across scenarios -- so it must be idempotent and must never treat a
  # multi-row frame as one failure): rows whose error is non-NA report
  # converged = 0 (the documented contract) when the contract field exists --
  # EXCEPT a data_generator() failure (#43 G1/round 4), where method() never
  # ran at all: forcing converged = 0 there poisons converged_mean (measured
  # 0 with 2 of 3 reps actually succeeding). Those rows keep converged = NA.
  if ("converged" %in% names(out)) {
    force_zero <- !is.na(out$error) & !.medsim_is_dgp_failure(out$error)
    out$converged[force_zero] <- 0
  }
  out
}
