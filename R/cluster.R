# Hopper / SLURM chunk harness
# Provides medsim_write_submit_script(), medsim_run_chunk(),
# medsim_combine_chunks() -- the three-piece pattern used by the Tofighi lab's
# chunked SLURM array jobs (UNM CARC Hopper partition).

# -- medsim_write_submit_script ---------------------------------------------

#' Write a SLURM array submit script for chunked simulation jobs
#'
#' @description
#' Generates a `submit_array.sh` bash script targeting the UNM CARC Hopper
#' cluster (and compatible SLURM systems).  The script submits one SLURM array
#' job per scenario chunk; each task runs an R script that calls
#' [medsim_run_chunk()].
#'
#' @param config A `medsim_config` object (from [medsim_config()]).  Must have
#'   `n_chunks` > 0 and `mode == "cluster"`.
#' @param run_script Character: path (on the cluster) to the per-chunk R script
#'   (the one that calls [medsim_run_chunk()]).  Default `"run_simulation_chunk.R"`.
#' @param output_file Character: where to write the bash script.  Default
#'   `"submit_array.sh"` in the current directory.
#' @param account Character: SLURM account/PI account string (e.g. `"pi-dtofighi"`).
#'   If `NULL`, no `--account` line is emitted.
#'
#' @return Invisibly, the path to the written script.
#'
#' @examples
#' cfg <- medsim_config(
#'   mode = "cluster", n_replications = 1000, n_chunks = 10,
#'   walltime = "04:00:00", mem_per_cpu = "8G"
#' )
#' tmp <- tempfile(fileext = ".sh")
#' medsim_write_submit_script(cfg, output_file = tmp)
#' # cat(readLines(tmp), sep = "\n")
#'
#' @seealso [medsim_run_chunk()], [medsim_combine_chunks()]
#'
#' @export
medsim_write_submit_script <- function(config,
                                        run_script  = "run_simulation_chunk.R",
                                        output_file = "submit_array.sh",
                                        account     = NULL) {
  if (!inherits(config, "medsim_config")) {
    stop("config must be a medsim_config object from medsim_config()")
  }

  n_chunks <- config$n_chunks %||% config$array_size
  if (is.null(n_chunks) || n_chunks < 1L) {
    stop("config$n_chunks (or config$array_size) must be a positive integer")
  }

  partition   <- config$partition  %||% "general"
  walltime    <- config$walltime   %||% "08:00:00"
  mem_per_cpu <- config$mem_per_cpu %||% "4G"
  r_module    <- config$r_module   %||% "r/4.4.0-ytj2"
  n_cores     <- config$n_cores    %||% 4L

  account_line <- if (!is.null(account)) sprintf("#SBATCH --account=%s", account) else ""

  lines <- c(
    "#!/bin/bash",
    "#SBATCH --job-name=medsim",
    sprintf("#SBATCH --array=1-%d", n_chunks),
    sprintf("#SBATCH --partition=%s", partition),
    sprintf("#SBATCH --time=%s", walltime),
    sprintf("#SBATCH --mem-per-cpu=%s", mem_per_cpu),
    sprintf("#SBATCH --cpus-per-task=%d", n_cores),
    if (nchar(account_line) > 0L) account_line,
    "#SBATCH --output=logs/medsim_%A_%a.out",
    "#SBATCH --error=logs/medsim_%A_%a.err",
    "",
    "# Load R module (UNM CARC Hopper)",
    sprintf("module load %s", r_module),
    "",
    "# Run the chunk script -- $SLURM_ARRAY_TASK_ID is passed via environment",
    sprintf("Rscript %s", run_script)
  )

  writeLines(lines, output_file)
  invisible(output_file)
}

# -- medsim_run_chunk -------------------------------------------------------

#' Run one chunk of a chunked SLURM array simulation
#'
#' @description
#' Runs replications `[(chunk_id-1)*chunk_size+1 .. chunk_id*chunk_size]`
#' and saves a single RDS file named `chunk_<chunk_id>.rds` in `output_dir`.
#' Designed to be called from inside a SLURM array task script; `chunk_id` and
#' `n_chunks` are auto-detected from `config` (which auto-reads
#' `SLURM_ARRAY_TASK_ID`).
#'
#' @param scenarios A list of [medsim_scenario()] objects.
#' @param method A function with signature `method(data, params)`.
#' @param config A `medsim_config` object with `chunk_id` and `n_chunks` set.
#' @param verbose Logical: print progress messages.
#'
#' @return Invisibly, the path of the RDS file written.
#'
#' @seealso [medsim_combine_chunks()], [medsim_write_submit_script()]
#'
#' @export
medsim_run_chunk <- function(scenarios, method, config, verbose = TRUE) {
  if (!inherits(config, "medsim_config")) {
    stop("config must be a medsim_config object")
  }

  chunk_id <- config$chunk_id
  n_chunks <- config$n_chunks %||% config$array_size

  if (is.null(chunk_id)) stop("config$chunk_id must be set (or SLURM_ARRAY_TASK_ID)")
  if (is.null(n_chunks))  stop("config$n_chunks must be set")

  n_rep   <- config$n_replications
  indices <- .medsim_chunk_indices(n_rep, n_chunks, chunk_id)

  if (verbose) {
    message(sprintf("[medsim_run_chunk] chunk %d/%d -- reps %d:%d",
                    chunk_id, n_chunks, indices[1L], indices[length(indices)]))
  }

  # Build a per-chunk config with only the reps for this chunk
  chunk_config <- config
  chunk_config$n_replications <- length(indices)
  chunk_config$rep_offset     <- indices[1L] - 1L

  results <- medsim_run(method, scenarios, chunk_config)

  output_dir  <- config$output_dir %||% "simulation_results"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, sprintf("chunk_%04d.rds", chunk_id))
  saveRDS(results, out_path)

  if (verbose) message(sprintf("[medsim_run_chunk] saved -> %s", out_path))
  invisible(out_path)
}

# -- medsim_combine_chunks -------------------------------------------------

#' Combine chunk RDS files produced by medsim_run_chunk()
#'
#' @description
#' Reads all `chunk_<k>.rds` files from `output_dir`, merges their `$results`
#' and `$truth` data frames, and returns a single `medsim_results` object
#' that is equivalent to running all replications in one call.
#'
#' @param output_dir Character: directory containing `chunk_*.rds` files.
#' @param pattern Character: glob pattern for chunk files.
#'   Default `"chunk_*.rds"`.
#' @param expected_chunks Integer or `NULL`: if supplied (e.g. the SLURM array
#'   size), warn when fewer chunk files are found -- catching a failed/timed-out
#'   task instead of silently combining a partial grid. Default `NULL` (no check).
#' @param verbose Logical: print file counts.
#'
#' @return A `medsim_results` object with combined `$results` and `$truth`.
#'
#' @examples
#' # After all SLURM chunks complete:
#' # combined <- medsim_combine_chunks("simulation_results/")
#' # coverage <- medsim_analyze_coverage(combined)
#'
#' @seealso [medsim_run_chunk()], [medsim_analyze_coverage()]
#'
#' @export
medsim_combine_chunks <- function(output_dir, pattern = "chunk_*.rds",
                                   expected_chunks = NULL, verbose = TRUE) {
  # Convert glob to regex for list.files
  files <- list.files(output_dir, pattern = glob2rx(pattern), full.names = TRUE)
  files <- sort(files)

  if (length(files) == 0L) {
    stop(sprintf("No chunk files matching '%s' found in '%s'", pattern, output_dir))
  }

  # Loudly flag a gap: a timed-out/failed SLURM array task leaves fewer chunk
  # files than submitted, and silently combining a partial grid as if complete
  # would bias every downstream number. Pass expected_chunks (= the array size)
  # to catch this.
  if (!is.null(expected_chunks) && length(files) < expected_chunks) {
    warning(sprintf(
      "medsim_combine_chunks: expected %d chunk files but found %d -- a chunk is missing (failed/timed-out task?); combining the partial grid.",
      as.integer(expected_chunks), length(files)))
  }

  if (verbose) message(sprintf("[medsim_combine_chunks] reading %d files", length(files)))

  chunks <- lapply(files, readRDS)

  # Merge results data frames
  all_results <- do.call(rbind, lapply(chunks, function(ch) ch$results))

  # Truth is scenario-level (same across chunks) -- use the first chunk's
  truth <- chunks[[1L]]$truth
  if (!is.null(truth)) {
    for (ch in chunks[-1L]) {
      if (is.null(ch$truth)) next
      new_rows <- ch$truth[!ch$truth$scenario %in% truth$scenario, , drop = FALSE]
      if (nrow(new_rows) > 0L) truth <- rbind(truth, new_rows)
    }
  }

  # Build combined medsim_results preserving first chunk's metadata
  combined <- chunks[[1L]]
  combined$results <- all_results
  combined$truth   <- truth
  combined$n_chunks_combined <- length(files)

  class(combined) <- c("medsim_results", "list")
  combined
}

# -- internal --------------------------------------------------------------

# Split n_rep into n_chunks even-ish groups; return indices for chunk_id.
.medsim_chunk_indices <- function(n_rep, n_chunks, chunk_id) {
  if (chunk_id < 1L || chunk_id > n_chunks) {
    stop(sprintf("chunk_id (%d) must be between 1 and n_chunks (%d)", chunk_id, n_chunks))
  }
  base    <- n_rep %/% n_chunks
  extras  <- n_rep %% n_chunks
  # First `extras` chunks get one extra rep
  start <- (chunk_id - 1L) * base + min(chunk_id - 1L, extras) + 1L
  size  <- base + if (chunk_id <= extras) 1L else 0L
  seq_len(size) + start - 1L
}
