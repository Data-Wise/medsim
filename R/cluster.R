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
#' The emitted script is fail-loud (Gate B of the chunked-run integrity layer):
#' a login shell (`#!/bin/bash -l` -- on Hopper `module` is only defined in
#' login shells), `set -eo pipefail`, a hard-failing `module load` (never
#' `|| true`), a `command -v Rscript` pre-check, `#SBATCH --requeue`, and the
#' `Rscript` call as the final command so its exit code is the task's exit
#' code. There is deliberately NO output-file gate in the script: the writer's
#' config and the run script's runtime config are independent, so a baked-in
#' path could fail successful tasks; completeness is audited at combine time by
#' [medsim_combine_chunks()] instead. Set `config$array_throttle = K` to cap
#' concurrently-running array tasks (`--array=1-N%K`).
#'
#' @param config A `medsim_config` object (from [medsim_config()]).  Must have
#'   `n_chunks` > 0 and `mode == "cluster"`. Optional: `array_throttle`.
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

  # Optional array throttle: --array=1-N%K caps concurrently-running tasks.
  throttle <- config$array_throttle
  array_spec <- if (!is.null(throttle)) {
    sprintf("1-%d%%%d", n_chunks, as.integer(throttle))
  } else {
    sprintf("1-%d", n_chunks)
  }

  # Hardened template (Gate B of SPEC-medsim-chunked-run-gates-2026-07-31;
  # fixes #37, subsumes #28B):
  # - `#!/bin/bash -l`: on Hopper `module` is a shell FUNCTION sourced only by
  #   login-shell init; a plain `#!/bin/bash` batch shell has no `module` and
  #   the load fails with "module: command not found" (see
  #   inst/hopper-tests/submit_chunk.sh, where this was field-verified).
  # - `set -eo pipefail` immediately; `set -u` only AFTER module init (module
  #   init scripts on HPC systems routinely reference unset variables and
  #   would trip nounset).
  # - `module load` hard-fails (never `|| true` -- that plus a trailing echo
  #   resetting $? was the historical COMPLETED/0:0/no-output mode).
  # - NO output-path gate here: the writer's config and the run script's
  #   runtime config are independent, so a baked path would exit 1 on every
  #   successful task whenever they differ (and --requeue would loop).
  #   Completeness is medsim_combine_chunks()'s job (the Gate A audit).
  lines <- c(
    "#!/bin/bash -l",
    "#SBATCH --job-name=medsim",
    sprintf("#SBATCH --array=%s", array_spec),
    sprintf("#SBATCH --partition=%s", partition),
    sprintf("#SBATCH --time=%s", walltime),
    sprintf("#SBATCH --mem-per-cpu=%s", mem_per_cpu),
    sprintf("#SBATCH --cpus-per-task=%d", n_cores),
    "#SBATCH --requeue",
    if (nchar(account_line) > 0L) account_line,
    "#SBATCH --output=logs/medsim_%A_%a.out",
    "#SBATCH --error=logs/medsim_%A_%a.err",
    "",
    "set -eo pipefail",
    "",
    "# Module init fallback (login shell above is the primary mechanism)",
    "if ! command -v module >/dev/null 2>&1; then",
    "  source /etc/profile.d/modules.sh 2>/dev/null || true",
    "fi",
    "",
    "# Load R module (UNM CARC Hopper) -- hard fail, never silently continue",
    sprintf("module load %s || { echo \"FATAL: module load %s failed\" >&2; exit 1; }",
            r_module, r_module),
    "",
    "set -u",
    "command -v Rscript >/dev/null || { echo \"FATAL: Rscript not on PATH\" >&2; exit 1; }",
    "",
    "# Run the chunk script -- $SLURM_ARRAY_TASK_ID is passed via environment.",
    "# Last command: its exit code IS the task's exit code.",
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
#' The chunk `.rds` is the sole artifact: in chunk mode the intermediate
#' per-scenario/summary CSVs that a standalone [medsim_run()] writes are
#' skipped (concurrent array tasks sharing an `output_dir` would clobber the
#' fixed-name files, leaving partial data that looks complete). Result rows
#' record the **global** replication id (schema v2): chunk 2 of a
#' 4-chunk/nsim-20 run writes `replication` 6..10, not 1..5, so ids never
#' collide across chunks and [medsim_combine_chunks()] can audit the combined
#' grid.
#'
#' @param scenarios A list of [medsim_scenario()] objects.
#' @param method A function with signature `method(data, params)`.
#' @param config A `medsim_config` object with `chunk_id` and `n_chunks` set.
#' @param verbose Logical: print progress messages.
#'
#' @return Invisibly, the path of the RDS file written.
#'
#' @seealso [medsim_combine_chunks()], [medsim_audit_results()],
#'   [medsim_write_submit_script()]
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
  out_path <- file.path(output_dir, .medsim_chunk_filename(chunk_id))
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
#'   size), a shortfall in chunk files is reported as an integrity violation
#'   (see `on_violation`) -- catching a failed/timed-out task instead of
#'   silently combining a partial grid. Default `NULL` (no check).
#' @param on_violation One of `"stop"` (default), `"warn"`, `"ignore"`: what to
#'   do when an integrity violation is found (missing chunks, duplicate or
#'   gapped replication ids, ragged cells, output collapse, all-failed cells,
#'   cross-scenario seed collisions). `"stop"` signals a
#'   `medsim_combine_violation` condition that CARRIES the combined results --
#'   recover an hours-long run's good cells with
#'   `tryCatch(medsim_combine_chunks(...), medsim_combine_violation = function(e) e$results)`.
#'   For a deliberate partial combine (an interim look at a still-running
#'   array), pass `on_violation = "warn"`.
#' @param nsim Integer or `NULL`: if supplied, additionally assert that every
#'   scenario has exactly `nsim` replications (the contiguity audit is
#'   otherwise self-validating and needs no external count).
#' @param collapse_threshold,collapse_digits,collapse_min_cell Tuning for the
#'   collapse-signature audit: per scenario and per continuous estimate column,
#'   require `n_distinct(round(x, collapse_digits)) > collapse_threshold * n_ok`
#'   (defaults 0.9 and 12; calibration: the 0.3.1 seed-collapse produced ~17
#'   distinct outcomes in 1000). Cells with fewer than `collapse_min_cell`
#'   non-NA values (default 30) are skipped as too small to diagnose.
#' @param verbose Logical: print file counts.
#'
#' @return A `medsim_results` object with combined `$results` and `$truth`.
#'
#' @examples
#' # After all SLURM chunks complete:
#' # combined <- medsim_combine_chunks("simulation_results/")
#' # coverage <- medsim_analyze_coverage(combined)
#'
#' @seealso [medsim_run_chunk()], [medsim_audit_results()],
#'   [medsim_analyze_coverage()]
#'
#' @export
medsim_combine_chunks <- function(output_dir, pattern = "chunk_*.rds",
                                   expected_chunks = NULL,
                                   on_violation = c("stop", "warn", "ignore"),
                                   nsim = NULL,
                                   collapse_threshold = 0.9,
                                   collapse_digits = 12L,
                                   collapse_min_cell = 30L,
                                   verbose = TRUE) {
  on_violation <- match.arg(on_violation)

  # Convert glob to regex for list.files
  files <- list.files(output_dir, pattern = glob2rx(pattern), full.names = TRUE)
  files <- sort(files)

  if (length(files) == 0L) {
    stop(sprintf("No chunk files matching '%s' found in '%s'", pattern, output_dir))
  }

  violations <- list()

  # A timed-out/failed SLURM array task leaves fewer chunk files than
  # submitted; silently combining a partial grid as if complete would bias
  # every downstream number. Reported through the same on_violation control as
  # the other integrity gates (a partial interim look is on_violation = "warn").
  if (!is.null(expected_chunks) && length(files) < expected_chunks) {
    violations[[length(violations) + 1L]] <- list(
      type = "missing_chunks",
      message = sprintf(
        "expected %d chunk files but found %d -- a chunk is missing (failed/timed-out task?)",
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

  # Build combined medsim_results. Chunk 1's $summary and $config describe ONE
  # chunk's slice (its config$n_replications is the CHUNK size and its summary
  # was computed over chunk-1 rows only) -- returning them unrebuilt presented
  # quarter-run statistics as the study. Rebuild both over the combined frame.
  combined <- chunks[[1L]]

  # rbind drops attributes; restore the schema/provenance stamps when every
  # chunk carries them (schema v2: `replication` is the global rep id).
  schema_v2 <- all(vapply(chunks, function(ch) {
    identical(attr(ch$results, "medsim_schema", exact = TRUE), 2L)
  }, logical(1)))
  if (schema_v2) {
    attr(all_results, "medsim_schema") <- 2L
    attr(all_results, "medsim_meta_cols") <-
      attr(chunks[[1L]]$results, "medsim_meta_cols", exact = TRUE)
  }

  combined$results <- all_results
  combined$truth   <- truth
  combined$summary <- medsim_summarize_results(all_results)

  # Rows-per-scenario is the true combined replication count for both schema
  # versions (schema v1's local ids under-count via max()); chunk-only fields
  # are meaningless on a combined object.
  combined$config$n_replications <- max(table(all_results$scenario))
  combined$config$rep_offset <- NULL
  combined$config$chunk_id   <- NULL
  combined$n_chunks_combined <- length(files)

  class(combined) <- c("medsim_results", "list")

  # Gate A: seed-provenance audit (contiguity, collapse signature, cross-
  # scenario seed collisions). Violations -- including the missing_chunks one
  # collected above -- are signalled through one on_violation control; "stop"
  # raises a condition that carries the combined object so no compute is lost.
  violations <- c(violations, .medsim_audit_seed_provenance(
    all_results,
    nsim               = nsim,
    collapse_threshold = collapse_threshold,
    collapse_digits    = collapse_digits,
    collapse_min_cell  = collapse_min_cell))
  .medsim_signal_violations(violations, combined, on_violation,
                            context = "medsim_combine_chunks")

  combined
}

# -- medsim_audit_results ---------------------------------------------------

#' Audit a combined results object for chunked-run integrity violations
#'
#' @description
#' Standalone entry point for Gate A of the chunked-run integrity layer
#' (SPEC-medsim-chunked-run-gates-2026-07-31): run the same audit
#' [medsim_combine_chunks()] performs, on an already-combined
#' `medsim_results` object. Checks, per scenario:
#'
#' 1. **Contiguity** -- `replication` ids form a gapless, duplicate-free
#'    `1..max` run, with the same `max` in every scenario (self-validating:
#'    needs no external replication count).
#' 2. **Collapse signature** -- every continuous estimate column has more than
#'    `collapse_threshold * n_ok` distinct values (the 0.3.1 seed-collapse
#'    produced ~17 distinct outcomes in 1000 while every chunk exited 0).
#'    An all-failed cell (`n_ok == 0`) is reported as `cell_failed`, not
#'    collapse.
#' 3. **Seed-space collisions** -- no two scenarios map onto overlapping
#'    [.medsim_det_seed()] sequences (the ~1e6-bucket name hash can collide).
#'
#' Frames without the schema-v2 stamp (produced before medsim 0.5.0, when
#' `replication` was chunk-LOCAL and collided across chunks) skip checks 1
#' and 3 with a warning -- they cannot be audited for id integrity.
#'
#' @param results A `medsim_results` object (e.g. from
#'   [medsim_combine_chunks()]), or a bare results data.frame.
#' @inheritParams medsim_combine_chunks
#'
#' @return Invisibly, the list of violations found (empty if clean). Signalling
#'   follows `on_violation`, exactly as in [medsim_combine_chunks()].
#'
#' @seealso [medsim_combine_chunks()], [medsim_run_chunk()]
#'
#' @export
medsim_audit_results <- function(results,
                                 on_violation = c("stop", "warn", "ignore"),
                                 nsim = NULL,
                                 collapse_threshold = 0.9,
                                 collapse_digits = 12L,
                                 collapse_min_cell = 30L) {
  on_violation <- match.arg(on_violation)
  df <- if (is.data.frame(results)) results else results$results
  if (is.null(df) || nrow(df) == 0L) {
    stop("medsim_audit_results: no result rows to audit")
  }
  violations <- .medsim_audit_seed_provenance(
    df, nsim = nsim,
    collapse_threshold = collapse_threshold,
    collapse_digits    = collapse_digits,
    collapse_min_cell  = collapse_min_cell)
  .medsim_signal_violations(violations, results, on_violation,
                            context = "medsim_audit_results")
  invisible(violations)
}

# -- Gate A internals -------------------------------------------------------

#' Seed-provenance audit over a combined results frame (Gate A)
#'
#' @param df Combined results data.frame.
#' @param nsim Optional integer: pin the absolute per-scenario rep count.
#' @param collapse_threshold,collapse_digits,collapse_min_cell See
#'   [medsim_combine_chunks()].
#' @return List of violations, each `list(type =, scenario =, message =)`.
#' @keywords internal
.medsim_audit_seed_provenance <- function(df,
                                          nsim = NULL,
                                          collapse_threshold = 0.9,
                                          collapse_digits = 12L,
                                          collapse_min_cell = 30L) {
  violations <- list()
  add <- function(type, message, scenario = NA_character_) {
    violations[[length(violations) + 1L]] <<-
      list(type = type, scenario = scenario, message = message)
  }

  scenarios <- unique(df$scenario)
  schema_v2 <- identical(attr(df, "medsim_schema", exact = TRUE), 2L)

  # --- A.1 contiguity (schema v2 only: legacy local ids CANNOT be audited --
  # they collide across chunks by construction) ------------------------------
  if (schema_v2) {
    maxima <- integer(0)
    for (sc in scenarios) {
      ids <- sort(df$replication[df$scenario == sc])
      if (anyDuplicated(ids)) {
        add("dup_rep_id", sprintf(
          "scenario '%s': duplicated replication id(s) (e.g. %s) -- two chunks claimed the same rep",
          sc, paste(utils::head(unique(ids[duplicated(ids)]), 3L), collapse = ", ")), sc)
      }
      expected <- seq_len(max(ids))
      gaps <- setdiff(expected, ids)
      if (length(gaps)) {
        add("rep_gap", sprintf(
          "scenario '%s': replication ids have %d gap(s) (missing e.g. %s) -- a chunk is missing or short",
          sc, length(gaps), paste(utils::head(gaps, 3L), collapse = ", ")), sc)
      }
      if (!is.null(nsim) && length(unique(ids)) != as.integer(nsim)) {
        add("nsim_mismatch", sprintf(
          "scenario '%s': %d distinct replication ids, expected nsim = %d",
          sc, length(unique(ids)), as.integer(nsim)), sc)
      }
      maxima[sc] <- max(ids)
    }
    if (length(unique(maxima)) > 1L) {
      add("ragged_cells", sprintf(
        "scenarios disagree on total replications (max ids: %s) -- ragged grid",
        paste(sprintf("%s=%d", names(maxima), maxima), collapse = ", ")))
    }
  } else {
    warning(paste(
      "medsim_audit_results: results lack the schema-v2 stamp (produced by an",
      "older medsim where `replication` was chunk-local); skipping the",
      "contiguity and seed-collision audits. Re-run under medsim >= 0.5.0 for",
      "full auditability."), call. = FALSE)
  }

  # --- A.2 collapse signature ----------------------------------------------
  # Continuous estimate columns: the runner-declared metadata complement
  # (legacy fallback: name subtraction), numeric, > 2 distinct values overall
  # (structurally excludes 0/1 contract fields like converged/branch_switch).
  meta_cols <- attr(df, "medsim_meta_cols", exact = TRUE) %||%
    c("scenario", "replication", "elapsed", "error")
  est_cols <- setdiff(names(df), meta_cols)
  est_cols <- est_cols[vapply(est_cols, function(cl) {
    is.numeric(df[[cl]]) &&
      length(unique(df[[cl]][!is.na(df[[cl]])])) > 2L
  }, logical(1))]

  for (sc in scenarios) {
    cell <- df[df$scenario == sc, , drop = FALSE]
    for (cl in est_cols) {
      x <- cell[[cl]][!is.na(cell[[cl]])]
      n_ok <- length(x)
      if (n_ok == 0L) {
        # An all-failed cell is a convergence problem, not seed collapse --
        # 0 > 0 is FALSE, so the naive threshold check would misreport it.
        add("cell_failed", sprintf(
          "scenario '%s', column '%s': all %d replications failed (no non-NA values)",
          sc, cl, nrow(cell)), sc)
        next
      }
      if (n_ok < collapse_min_cell) next  # too small to diagnose
      n_dist <- length(unique(round(x, collapse_digits)))
      if (n_dist <= collapse_threshold * n_ok) {
        add("collapse", sprintf(
          "scenario '%s', column '%s': only %d distinct values in %d non-NA replications -- the 0.3.1 seed-collapse signature (or a DGM calling set.seed() internally)",
          sc, cl, n_dist, n_ok), sc)
      }
    }
  }

  # --- A.3 cross-scenario seed collisions (schema v2 only) -----------------
  # Recompute each scenario's deterministic seed block THROUGH the real
  # .medsim_det_seed() (no formula duplication): det_seed(name, 0L) recovers
  # the scenario's base, and seeds are (base + rep) mod .Machine$integer.max.
  if (schema_v2 && length(scenarios) > 1L) {
    m <- .Machine$integer.max
    seed_pool <- lapply(scenarios, function(sc) {
      base <- as.numeric(.medsim_det_seed(sc, 0L)) - 1
      reps <- unique(df$replication[df$scenario == sc])
      (base + reps) %% m + 1
    })
    names(seed_pool) <- scenarios
    all_seeds <- unlist(seed_pool, use.names = FALSE)
    if (anyDuplicated(all_seeds)) {
      dup_seed <- all_seeds[duplicated(all_seeds)][1L]
      holders <- scenarios[vapply(seed_pool, function(s) dup_seed %in% s,
                                  logical(1))]
      add("seed_collision", sprintf(
        "scenarios %s share RNG seed(s) (name-hash bucket collision in .medsim_det_seed) -- their draws are not independent; rename one scenario",
        paste(sprintf("'%s'", holders), collapse = " and ")))
    }
  }

  violations
}

#' Signal collected integrity violations per the on_violation control
#'
#' @param violations List of violations from [.medsim_audit_seed_provenance()].
#' @param results The combined object to attach to a stop condition.
#' @param on_violation `"stop"`, `"warn"`, or `"ignore"`.
#' @param context Character: calling-function name for messages.
#' @keywords internal
.medsim_signal_violations <- function(violations, results, on_violation,
                                      context = "medsim_combine_chunks") {
  if (length(violations) == 0L || on_violation == "ignore") {
    return(invisible())
  }
  msg <- paste0(
    context, ": ", length(violations), " integrity violation(s):\n",
    paste0("  - [", vapply(violations, `[[`, "", "type"), "] ",
           vapply(violations, `[[`, "", "message"), collapse = "\n"))
  if (on_violation == "warn") {
    warning(msg, call. = FALSE)
    return(invisible())
  }
  # Data-carrying condition: the combined results ride along so an unguarded
  # call fails loud while a tryCatch recovers hours of cluster compute:
  #   tryCatch(medsim_combine_chunks(...),
  #            medsim_combine_violation = function(e) e$results)
  cond <- structure(
    class = c("medsim_combine_violation", "medsim_error", "error", "condition"),
    list(message = paste0(
           msg, "\n(Partial results are attached to this condition: ",
           "tryCatch(..., medsim_combine_violation = function(e) e$results). ",
           "For a deliberate partial combine use on_violation = \"warn\".)"),
         call = NULL, results = results, violations = violations))
  stop(cond)
}

# -- internal --------------------------------------------------------------

#' Chunk-file naming authority
#'
#' @description
#' The single source of the `chunk_%04d.rds` convention, shared by the writer
#' ([medsim_run_chunk()]) and matched by [medsim_combine_chunks()]'s default
#' `pattern = "chunk_*.rds"` -- change the convention here (and that default)
#' or the writer and the combiner silently stop finding each other's files.
#'
#' @param chunk_id Integer chunk id.
#' @return Character filename, e.g. `"chunk_0007.rds"`.
#' @keywords internal
.medsim_chunk_filename <- function(chunk_id) {
  sprintf("chunk_%04d.rds", as.integer(chunk_id))
}

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
