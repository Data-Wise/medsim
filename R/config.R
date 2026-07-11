# Configuration and Environment Detection
#
# Adapted from product-of-three simulation patterns

#' Create Simulation Configuration
#'
#' @description
#' Creates a configuration object for simulation studies. Automatically detects
#' whether running on local machine or HPC cluster and adjusts parameters
#' accordingly.
#'
#' @param mode Character: "auto", "test", "local", or "cluster"
#'   - "auto": Detect based on environment variables (SLURM, PBS, LSF)
#'   - "test": Quick validation (~30 seconds)
#'   - "local": Development on local machine (~15 minutes)
#'   - "cluster": Production on HPC cluster (hours)
#' @param n_replications Integer: Number of Monte Carlo replications. If NULL,
#'   uses mode defaults (test=20, local=100, cluster=1000)
#' @param n_cores Integer: Number of CPU cores for parallel processing. If NULL,
#'   auto-detects (all cores - 2 on local, SLURM_CPUS_PER_TASK on cluster)
#' @param scenarios Character: "all" or "test". Use "test" for single challenging
#'   scenario during development
#' @param output_dir Character: Directory for saving results
#' @param seed Integer: Base seed, stored on the config and printed for
#'   provenance. **Does not itself determine replication draws** -- since the
#'   `.medsim_det_seed()` fix, [medsim_run()] seeds each replication
#'   deterministically from `(scenario_name, global_rep_id)`, independent of
#'   `seed`/`seed_stream`/chunk/worker/cluster-type. `seed`/`seed_stream` are
#'   only consulted if you call [medsim_run_parallel()] directly (its own
#'   `seed=` argument, documented there).
#' @param chunk_id Integer: SLURM array task index (1-based) for this chunk.
#'   Auto-detected from `SLURM_ARRAY_TASK_ID` when running inside a SLURM
#'   array job and not supplied explicitly.  `NULL` = no chunking.
#' @param n_chunks Integer: Total number of chunks (SLURM array size).
#'   Used by `medsim_run_chunk()` to slice the replication index.
#' @param array_size Integer: Alias for `n_chunks` (matches SLURM terminology).
#'   When both are supplied, `n_chunks` wins.
#' @param seed_stream Integer: Master seed for L'Ecuyer-CMRG reproducible
#'   per-worker RNG streams, consulted only by a direct
#'   [medsim_run_parallel()] call (its `seed=` argument) -- see the `seed`
#'   param above for why [medsim_run()]/[medsim_run_chunk()] no longer need
#'   it.  Defaults to `seed` when `NULL`.
#' @param partition Character: SLURM partition (queue) name.  Defaults to
#'   `"general"` in cluster mode; `NULL` otherwise.
#' @param walltime Character: SLURM wall-time limit (HH:MM:SS).  Defaults to
#'   `"08:00:00"` in cluster mode.
#' @param mem_per_cpu Character: SLURM memory per CPU.  Defaults to `"4G"` in
#'   cluster mode.
#' @param r_module Character: Environment module string for `module load`.
#'   Defaults to `"r/4.4.0-ytj2"` (UNM CARC Hopper) in cluster mode.
#' @param ... Additional custom parameters
#'
#' @return A list with simulation configuration parameters
#'
#' @details
#' ## Execution Modes
#'
#' | Mode | Replications | Cores | Runtime | Use Case |
#' |------|--------------|-------|---------|----------|
#' | test | 20 | 4 | ~30s | Quick validation |
#' | local | 100 | auto | ~15m | Development |
#' | cluster | 1000 | SLURM | hours | Production |
#'
#' ## Environment Detection
#'
#' When mode = "auto", checks for:
#' - SLURM_JOB_ID (SLURM scheduler)
#' - PBS_JOBID (PBS/Torque scheduler)
#' - LSB_JOBID (LSF scheduler)
#'
#' If any are found, uses "cluster" mode. Otherwise, uses "local" mode.
#'
#' ## Custom Parameters
#'
#' You can add custom parameters via `...`:
#' ```r
#' config <- medsim_config(
#'   mode = "local",
#'   n_bootstrap = 5000,
#'   alpha = 0.05,
#'   custom_param = "value"
#' )
#' ```
#'
#' @examples
#' # Auto-detect environment
#' config <- medsim_config(mode = "auto")
#'
#' # Explicit test mode for quick validation
#' config_test <- medsim_config(mode = "test")
#'
#' # Local mode with custom replications
#' config_local <- medsim_config(
#'   mode = "local",
#'   n_replications = 500
#' )
#'
#' # Cluster mode (auto-detects SLURM cores)
#' config_cluster <- medsim_config(mode = "cluster")
#'
#' # Custom parameters
#' config_custom <- medsim_config(
#'   mode = "local",
#'   n_bootstrap = 1000,
#'   ci_level = 0.95
#' )
#'
#' # Print configuration
#' print(config)
#'
#' @seealso [medsim_detect_environment()], [print.medsim_config()]
#'
#' @export
medsim_config <- function(mode = "auto",
                          n_replications = NULL,
                          n_cores = NULL,
                          scenarios = NULL,
                          output_dir = NULL,
                          seed = 12345,
                          chunk_id = NULL,
                          n_chunks = NULL,
                          array_size = NULL,
                          seed_stream = NULL,
                          partition = NULL,
                          walltime = NULL,
                          mem_per_cpu = NULL,
                          r_module = NULL,
                          ...) {

  # Detect environment if auto mode
  if (mode == "auto") {
    mode <- medsim_detect_environment()
    message(sprintf("Auto-detected: %s environment", toupper(mode)))
  }

  # Validate mode
  valid_modes <- c("test", "local", "cluster")
  if (!mode %in% valid_modes) {
    stop(sprintf("Unknown mode: '%s'. Use 'auto', 'test', 'local', or 'cluster'",
                 mode))
  }

  # Define mode-specific defaults
  mode_defaults <- list(
    test = list(
      name = "TEST MODE",
      n_replications = 20,
      n_cores = 4,
      scenarios = "test",
      output_dir = "simulation_results_test"
    ),
    local = list(
      name = "LOCAL MODE",
      n_replications = 100,
      n_cores = NULL,
      scenarios = "all",
      output_dir = "simulation_results"
    ),
    cluster = list(
      name = "CLUSTER MODE",
      n_replications = 1000,
      n_cores = NULL,
      scenarios = "all",
      output_dir = "simulation_results"
    )
  )

  # Get defaults for this mode
  defaults <- mode_defaults[[mode]]

  # Auto-detect chunk_id from SLURM array env var when not supplied
  if (is.null(chunk_id)) {
    slurm_task <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "")
    if (nchar(slurm_task) > 0L) {
      chunk_id <- as.integer(slurm_task)
    }
  }

  # Override defaults with user-specified values
  config <- list(
    mode           = mode,
    name           = defaults$name,
    n_replications = n_replications %||% defaults$n_replications,
    n_cores        = n_cores %||% defaults$n_cores,
    scenarios      = scenarios %||% defaults$scenarios,
    output_dir     = output_dir %||% defaults$output_dir,
    seed           = seed,
    # Chunked array-job parameters (all NULL by default = no chunking)
    chunk_id       = chunk_id,
    n_chunks       = n_chunks,
    array_size     = array_size,
    seed_stream    = seed_stream,
    # Hopper/SLURM submission parameters (NULL = system defaults)
    partition      = partition %||% if (mode == "cluster") "general" else NULL,
    walltime       = walltime  %||% if (mode == "cluster") "08:00:00" else NULL,
    mem_per_cpu    = mem_per_cpu %||% if (mode == "cluster") "4G" else NULL,
    r_module       = r_module  %||% if (mode == "cluster") "r/4.4.0-ytj2" else NULL
  )

  # Determine number of cores if NULL
  if (is.null(config$n_cores)) {
    config$n_cores <- medsim_detect_cores()
  }

  # Add custom parameters
  custom_params <- list(...)
  config <- c(config, custom_params)

  # Set class
  class(config) <- c("medsim_config", "list")

  return(config)
}

#' Detect Computing Environment
#'
#' @description
#' Detects whether code is running on local machine or HPC cluster by checking
#' for scheduler environment variables.
#'
#' @return Character: "local" or "cluster"
#'
#' @details
#' Checks for the following environment variables:
#' - SLURM_JOB_ID (SLURM scheduler)
#' - PBS_JOBID (PBS/Torque scheduler)
#' - LSB_JOBID (LSF scheduler)
#'
#' If any are found, returns "cluster". Otherwise, returns "local".
#'
#' @examples
#' env <- medsim_detect_environment()
#' if (env == "cluster") {
#'   message("Running on HPC cluster")
#' } else {
#'   message("Running on local machine")
#' }
#'
#' @export
medsim_detect_environment <- function() {
  # Check for common HPC scheduler environment variables
  scheduler_vars <- c(
    "SLURM_JOB_ID",   # SLURM
    "PBS_JOBID",      # PBS/Torque
    "LSB_JOBID"       # LSF
  )

  for (var in scheduler_vars) {
    if (Sys.getenv(var) != "") {
      return("cluster")
    }
  }

  return("local")
}

#' Detect Number of Available Cores
#'
#' @description
#' Detects the number of CPU cores available for parallel processing. On HPC
#' clusters, uses scheduler-allocated cores. On local machines, uses all cores
#' minus 2 to avoid overwhelming the system.
#'
#' @return Integer: Number of cores to use
#'
#' @details
#' On clusters, checks for:
#' - SLURM_CPUS_PER_TASK (SLURM)
#' - PBS_NUM_PPN (PBS/Torque)
#' - LSB_DJOB_NUMPROC (LSF)
#'
#' On local machines, uses `parallel::detectCores() - 2`.
#'
#' @examples
#' n_cores <- medsim_detect_cores()
#' message(sprintf("Using %d cores", n_cores))
#'
#' @export
medsim_detect_cores <- function() {
  # Check for cluster-allocated cores
  if (Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
    return(as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")))
  }

  if (Sys.getenv("PBS_NUM_PPN") != "") {
    return(as.numeric(Sys.getenv("PBS_NUM_PPN")))
  }

  if (Sys.getenv("LSB_DJOB_NUMPROC") != "") {
    return(as.numeric(Sys.getenv("LSB_DJOB_NUMPROC")))
  }

  # Local machine: use all cores minus 2
  n_cores <- parallel::detectCores() - 2
  return(max(1, n_cores))  # Ensure at least 1 core
}

#' Print Configuration Summary
#'
#' @param x A medsim_config object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_config <- function(x, ...) {
  cat("\n")
  cat("========================================================\n")
  cat(sprintf("  %s\n", x$name))
  cat("========================================================\n\n")

  cat("Simulation Parameters:\n")
  cat(sprintf("  Replications:        %d\n", x$n_replications))
  cat(sprintf("  Scenarios:           %s\n", x$scenarios))
  cat("\n")

  cat("Computing Resources:\n")
  cat(sprintf("  Cores:               %d\n", x$n_cores))
  cat(sprintf("  Base Seed:           %d (provenance only -- replications are seeded per-rep, see ?medsim_run)\n", x$seed))
  cat("\n")

  cat("Output:\n")
  cat(sprintf("  Directory:           %s\n", x$output_dir))
  cat("\n")

  # Show custom parameters if any
  standard_params <- c("mode", "name", "n_replications", "n_cores",
                       "scenarios", "output_dir", "seed")
  custom_params <- setdiff(names(x), standard_params)

  if (length(custom_params) > 0) {
    cat("Custom Parameters:\n")
    for (param in custom_params) {
      value <- x[[param]]
      if (is.numeric(value) && value > 1000) {
        value_str <- format(value, scientific = TRUE, digits = 2)
      } else {
        value_str <- as.character(value)
      }
      cat(sprintf("  %-20s %s\n", paste0(param, ":"), value_str))
    }
    cat("\n")
  }

  cat("========================================================\n\n")

  invisible(x)
}

#' Compare Multiple Configurations
#'
#' @description
#' Creates a comparison table showing differences between test, local, and
#' cluster configurations.
#'
#' @return A data.frame with configuration comparison
#'
#' @examples
#' comparison <- medsim_compare_configs()
#' print(comparison)
#'
#' @export
medsim_compare_configs <- function() {
  modes <- c("test", "local", "cluster")
  configs <- lapply(modes, medsim_config)

  comparison <- data.frame(
    Parameter = c("Replications", "Cores", "Scenarios", "Output Directory"),
    Test = c(
      configs[[1]]$n_replications,
      configs[[1]]$n_cores,
      configs[[1]]$scenarios,
      configs[[1]]$output_dir
    ),
    Local = c(
      configs[[2]]$n_replications,
      configs[[2]]$n_cores,
      configs[[2]]$scenarios,
      configs[[2]]$output_dir
    ),
    Cluster = c(
      configs[[3]]$n_replications,
      configs[[3]]$n_cores,
      configs[[3]]$scenarios,
      configs[[3]]$output_dir
    ),
    stringsAsFactors = FALSE
  )

  return(comparison)
}

# Helper function: NULL-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
