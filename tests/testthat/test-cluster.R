# Tests for Phase 2 — Hopper/SLURM cluster harness
# Covers: medsim_config() chunk params, medsim_run_parallel() seed,
#         medsim_write_submit_script(), medsim_run_chunk(),
#         medsim_combine_chunks(), .medsim_chunk_indices()

# ── medsim_config — chunk / SLURM params ──────────────────────────────────

test_that("medsim_config stores chunk_id and n_chunks when supplied", {
  cfg <- medsim_config("test", chunk_id = 3L, n_chunks = 10L)
  expect_equal(cfg$chunk_id, 3L)
  expect_equal(cfg$n_chunks, 10L)
})

test_that("medsim_config n_chunks is NULL by default", {
  cfg <- medsim_config("test")
  expect_null(cfg$n_chunks)
  expect_null(cfg$chunk_id)
})

test_that("medsim_config auto-detects chunk_id from SLURM_ARRAY_TASK_ID", {
  withr::local_envvar(SLURM_ARRAY_TASK_ID = "5")
  cfg <- medsim_config("cluster", n_chunks = 10L)
  expect_equal(cfg$chunk_id, 5L)
})

test_that("medsim_config explicit chunk_id wins over SLURM_ARRAY_TASK_ID", {
  withr::local_envvar(SLURM_ARRAY_TASK_ID = "5")
  cfg <- medsim_config("cluster", chunk_id = 2L, n_chunks = 10L)
  expect_equal(cfg$chunk_id, 2L)
})

test_that("medsim_config stores seed_stream", {
  cfg <- medsim_config("cluster", seed_stream = 99L)
  expect_equal(cfg$seed_stream, 99L)
})

test_that("medsim_config cluster mode sets Hopper defaults", {
  cfg <- medsim_config("cluster")
  expect_equal(cfg$partition, "general")
  expect_equal(cfg$walltime, "08:00:00")
  expect_equal(cfg$mem_per_cpu, "4G")
  expect_equal(cfg$r_module, "r/4.4.0-ytj2")
})

test_that("medsim_config local mode leaves Hopper fields NULL", {
  cfg <- medsim_config("local")
  expect_null(cfg$partition)
  expect_null(cfg$walltime)
  expect_null(cfg$mem_per_cpu)
  expect_null(cfg$r_module)
})

test_that("medsim_config cluster mode custom Hopper params override defaults", {
  cfg <- medsim_config("cluster", walltime = "02:00:00", mem_per_cpu = "8G",
                       r_module = "r/4.3.0", partition = "bigmem")
  expect_equal(cfg$walltime, "02:00:00")
  expect_equal(cfg$mem_per_cpu, "8G")
  expect_equal(cfg$r_module, "r/4.3.0")
  expect_equal(cfg$partition, "bigmem")
})

# ── medsim_run_parallel — seed parameter ──────────────────────────────────

test_that("medsim_run_parallel accepts seed param without error (sequential path)", {
  result <- medsim_run_parallel(
    tasks = 1:3,        # < 4 tasks → sequential path, seed ignored
    fun   = function(i) i * 2L,
    seed  = 12345L
  )
  expect_length(result, 3L)
})

test_that("medsim_run_parallel seed produces identical results on PSOCK cluster", {
  skip_on_cran()
  skip_on_os("windows")  # avoid PSOCK flakiness on Windows CI
  run_with_seed <- function(s) {
    medsim_run_parallel(
      tasks        = 1:5,
      # Draw from the worker's RNG directly — do NOT call set.seed(NULL),
      # which would reset the L'Ecuyer sub-stream and destroy reproducibility.
      fun          = function(i) runif(1L),
      n_cores      = 2L,
      cluster_type = "PSOCK",
      seed         = s
    )
  }
  r1 <- run_with_seed(42L)
  r2 <- run_with_seed(42L)
  # With L'Ecuyer each worker stream is seeded deterministically → same results
  expect_equal(unlist(r1), unlist(r2))
})

# ── .medsim_chunk_indices ─────────────────────────────────────────────────

test_that(".medsim_chunk_indices covers all reps exactly once", {
  n_rep <- 17L; n_chunks <- 5L
  all_idx <- unlist(lapply(seq_len(n_chunks), function(k)
    medsim:::.medsim_chunk_indices(n_rep, n_chunks, k)))
  expect_equal(sort(all_idx), seq_len(n_rep))
})

test_that(".medsim_chunk_indices chunk sizes differ by at most 1", {
  n_rep <- 17L; n_chunks <- 5L
  sizes <- vapply(seq_len(n_chunks), function(k)
    length(medsim:::.medsim_chunk_indices(n_rep, n_chunks, k)), integer(1L))
  expect_true(max(sizes) - min(sizes) <= 1L)
})

test_that(".medsim_chunk_indices errors on out-of-range chunk_id", {
  expect_error(
    medsim:::.medsim_chunk_indices(10L, 3L, 0L),
    "chunk_id"
  )
  expect_error(
    medsim:::.medsim_chunk_indices(10L, 3L, 4L),
    "chunk_id"
  )
})

test_that(".medsim_chunk_indices works when n_rep is exactly divisible", {
  n_rep <- 12L; n_chunks <- 4L
  sizes <- vapply(seq_len(n_chunks), function(k)
    length(medsim:::.medsim_chunk_indices(n_rep, n_chunks, k)), integer(1L))
  expect_true(all(sizes == 3L))
})

# ── medsim_write_submit_script ─────────────────────────────────────────────

test_that("medsim_write_submit_script errors without medsim_config object", {
  expect_error(
    medsim_write_submit_script(list(n_chunks = 5L)),
    "medsim_config"
  )
})

test_that("medsim_write_submit_script errors when n_chunks is NULL", {
  cfg <- medsim_config("cluster")    # n_chunks not set
  expect_error(
    medsim_write_submit_script(cfg),
    "n_chunks"
  )
})

test_that("medsim_write_submit_script writes a file with SBATCH headers", {
  cfg <- medsim_config("cluster", n_chunks = 8L, walltime = "04:00:00")
  tmp <- tempfile(fileext = ".sh")
  on.exit(unlink(tmp))
  out <- medsim_write_submit_script(cfg, output_file = tmp)
  expect_equal(out, tmp)
  expect_true(file.exists(tmp))
  lines <- readLines(tmp)
  expect_true(any(grepl("^#!/bin/bash", lines)))
  expect_true(any(grepl("--array=1-8", lines)))
  expect_true(any(grepl("--time=04:00:00", lines)))
  expect_true(any(grepl("--partition=general", lines)))
})

test_that("medsim_write_submit_script includes account line when supplied", {
  cfg <- medsim_config("cluster", n_chunks = 4L)
  tmp <- tempfile(fileext = ".sh")
  on.exit(unlink(tmp))
  medsim_write_submit_script(cfg, output_file = tmp, account = "pi-dtofighi")
  lines <- readLines(tmp)
  expect_true(any(grepl("--account=pi-dtofighi", lines)))
})

test_that("medsim_write_submit_script omits account line when NULL", {
  cfg <- medsim_config("cluster", n_chunks = 4L)
  tmp <- tempfile(fileext = ".sh")
  on.exit(unlink(tmp))
  medsim_write_submit_script(cfg, output_file = tmp, account = NULL)
  lines <- readLines(tmp)
  expect_false(any(grepl("--account", lines)))
})

test_that("medsim_write_submit_script includes module load line", {
  cfg <- medsim_config("cluster", n_chunks = 4L, r_module = "r/4.3.1")
  tmp <- tempfile(fileext = ".sh")
  on.exit(unlink(tmp))
  medsim_write_submit_script(cfg, output_file = tmp)
  lines <- readLines(tmp)
  expect_true(any(grepl("module load r/4.3.1", lines)))
})

# ── medsim_combine_chunks ─────────────────────────────────────────────────

# Helper: build a minimal medsim_results object
make_chunk_result <- function(scenario = "s1", reps = 1:3,
                               nde_truth = 0.2, nie_truth = 0.3) {
  results_df <- data.frame(
    scenario = scenario,
    rep      = reps,
    estimate = rnorm(length(reps))
  )
  truth_df <- data.frame(scenario = scenario, NDE = nde_truth, NIE = nie_truth)
  r <- list(results = results_df, truth = truth_df, config = list(),
            scenarios = list(), method_name = "test", timestamp = Sys.time())
  class(r) <- c("medsim_results", "list")
  r
}

test_that("medsim_combine_chunks errors when no files match", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  expect_error(
    medsim_combine_chunks(tmp_dir, verbose = FALSE),
    "No chunk files"
  )
})

test_that("medsim_combine_chunks merges two chunks correctly", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  ch1 <- make_chunk_result(scenario = "s1", reps = 1:3)
  ch2 <- make_chunk_result(scenario = "s1", reps = 4:6)
  saveRDS(ch1, file.path(tmp_dir, "chunk_0001.rds"))
  saveRDS(ch2, file.path(tmp_dir, "chunk_0002.rds"))

  combined <- medsim_combine_chunks(tmp_dir, verbose = FALSE)

  expect_s3_class(combined, "medsim_results")
  expect_equal(nrow(combined$results), 6L)
  expect_equal(sort(combined$results$rep), 1:6)
  expect_equal(combined$n_chunks_combined, 2L)
})

test_that("medsim_combine_chunks deduplicates truth rows across chunks", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Same scenario → truth row should appear only once
  ch1 <- make_chunk_result(scenario = "s1", reps = 1:2)
  ch2 <- make_chunk_result(scenario = "s1", reps = 3:4)
  saveRDS(ch1, file.path(tmp_dir, "chunk_0001.rds"))
  saveRDS(ch2, file.path(tmp_dir, "chunk_0002.rds"))

  combined <- medsim_combine_chunks(tmp_dir, verbose = FALSE)
  expect_equal(nrow(combined$truth), 1L)
})

test_that("medsim_combine_chunks includes new scenarios from later chunks", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  ch1 <- make_chunk_result(scenario = "s1", reps = 1:2)
  ch2 <- make_chunk_result(scenario = "s2", reps = 1:2)
  saveRDS(ch1, file.path(tmp_dir, "chunk_0001.rds"))
  saveRDS(ch2, file.path(tmp_dir, "chunk_0002.rds"))

  combined <- medsim_combine_chunks(tmp_dir, verbose = FALSE)
  expect_equal(sort(unique(combined$truth$scenario)), c("s1", "s2"))
})

# ── medsim_run_chunk ───────────────────────────────────────────────────────

test_that("medsim_run_chunk errors without chunk_id", {
  cfg <- medsim_config("test", n_chunks = 3L)
  # chunk_id is NULL (SLURM_ARRAY_TASK_ID not set)
  withr::local_envvar(SLURM_ARRAY_TASK_ID = NA)
  expect_error(
    medsim_run_chunk(list(), identity, cfg, verbose = FALSE),
    "chunk_id"
  )
})

test_that("medsim_run_chunk errors without n_chunks", {
  cfg <- medsim_config("test", chunk_id = 1L)
  expect_error(
    medsim_run_chunk(list(), identity, cfg, verbose = FALSE),
    "n_chunks"
  )
})

test_that("medsim_run_chunk runs successfully and writes chunk RDS", {
  sc <- medsim_scenario(
    name = "chunk_run_test",
    data_generator = function(n = 100) {
      data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n))
    },
    params = list(indirect = 0.0)
  )
  method <- function(data, params) {
    list(indirect = 0.1, indirect_ci_lower = 0, indirect_ci_upper = 0.3,
         indirect_p = 0.2, branch_switch = NA, converged = 1L)
  }
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  cfg <- medsim_config("test", chunk_id = 1L, n_chunks = 2L, output_dir = tmp_dir)
  out_path <- medsim_run_chunk(list(sc), method, cfg, verbose = FALSE)

  expect_true(file.exists(out_path))
  result <- readRDS(out_path)
  expect_s3_class(result, "medsim_results")
})

test_that("medsim_run_chunk emits verbose messages", {
  sc <- medsim_scenario(
    name = "chunk_verbose_test",
    data_generator = function(n = 100) {
      data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n))
    },
    params = list(indirect = 0.0)
  )
  method <- function(data, params) {
    list(indirect = 0.1, indirect_ci_lower = 0, indirect_ci_upper = 0.3,
         indirect_p = 0.2, branch_switch = NA, converged = 1L)
  }
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  cfg <- medsim_config("test", chunk_id = 1L, n_chunks = 2L, output_dir = tmp_dir)
  expect_message(
    medsim_run_chunk(list(sc), method, cfg, verbose = TRUE),
    "chunk"
  )
})

test_that("medsim_run_chunk produces independent draws across chunks (regression: cross-chunk RNG collapse)", {
  # Prior to the .medsim_det_seed fix, every chunk called set.seed(config$seed)
  # with the SAME scalar and always started rep_id at 1 -- chunk_config$rep_offset
  # was computed but never consumed. Every chunk regenerated the identical short
  # sequence of "replications", so a 60-chunk/1000-rep coverage study collapsed
  # to ~n_chunks distinct outcomes and produced spurious coverage = 1.0.
  sc <- medsim_scenario(
    name = "rng_independence_test",
    data_generator = function(n = 100) data.frame(x = rnorm(n)),
    params = list()
  )
  method <- function(data, params) list(x1 = data$x[1])

  run_chunk_values <- function(chunk_id) {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE))
    cfg <- medsim_config("test", chunk_id = chunk_id, n_chunks = 2L,
                          n_replications = 4L, output_dir = tmp_dir)
    out_path <- medsim_run_chunk(list(sc), method, cfg, verbose = FALSE)
    readRDS(out_path)$results$x1
  }

  chunk1 <- run_chunk_values(1L)
  chunk2 <- run_chunk_values(2L)

  # Different chunks must draw different data -- NOT identical.
  expect_false(identical(chunk1, chunk2))
  # Within a chunk, distinct replications must also differ from each other.
  expect_gt(length(unique(chunk1)), 1L)
})

test_that("medsim_run_chunk is reproducible: same chunk_id gives same draws", {
  sc <- medsim_scenario(
    name = "rng_reproducibility_test",
    data_generator = function(n = 100) data.frame(x = rnorm(n)),
    params = list()
  )
  method <- function(data, params) list(x1 = data$x[1])

  run_chunk_values <- function() {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE))
    cfg <- medsim_config("test", chunk_id = 1L, n_chunks = 2L,
                          n_replications = 4L, output_dir = tmp_dir)
    out_path <- medsim_run_chunk(list(sc), method, cfg, verbose = FALSE)
    readRDS(out_path)$results$x1
  }

  expect_identical(run_chunk_values(), run_chunk_values())
})

test_that(".medsim_det_seed is a pure function of (scenario_name, rep_id)", {
  expect_identical(.medsim_det_seed("scenario_a", 5L), .medsim_det_seed("scenario_a", 5L))
  expect_false(identical(.medsim_det_seed("scenario_a", 5L), .medsim_det_seed("scenario_a", 6L)))
  expect_false(identical(.medsim_det_seed("scenario_a", 5L), .medsim_det_seed("scenario_b", 5L)))
})

test_that(".medsim_det_seed is order-sensitive (anagrams do not collide)", {
  expect_false(identical(.medsim_det_seed("abc", 1L), .medsim_det_seed("bca", 1L)))
  expect_false(identical(.medsim_det_seed("abc", 1L), .medsim_det_seed("cab", 1L)))
})

test_that("medsim_run_chunk output is invariant to how the total is split into chunks", {
  # The actual value proposition of .medsim_det_seed: replication k of a given
  # scenario must draw the same data whether it lands in a 2-chunk or a
  # 5-chunk split of the same 10-replication total -- not just "chunk 1 !=
  # chunk 2" (already covered above), but "chunk boundaries don't matter".
  sc <- medsim_scenario(
    name = "chunking_invariance_test",
    data_generator = function(n = 100) data.frame(x = rnorm(n)),
    params = list()
  )
  method <- function(data, params) list(x1 = data$x[1])

  run_all_chunks <- function(n_chunks, n_replications = 10L) {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE))
    for (cid in seq_len(n_chunks)) {
      cfg <- medsim_config("test", chunk_id = cid, n_chunks = n_chunks,
                            n_replications = n_replications, output_dir = tmp_dir)
      medsim_run_chunk(list(sc), method, cfg, verbose = FALSE)
    }
    combined <- medsim_combine_chunks(tmp_dir, verbose = FALSE)
    # NOTE: combined$results$replication is the per-chunk LOCAL rep_id (it
    # collides across chunks -- e.g. every chunk's first row is labeled
    # replication=1), so it cannot align rows to a common global order across
    # different chunkings. Compare the drawn VALUES as a set instead, which
    # sidesteps that ambiguity and still proves the real invariant: the same
    # global draws occur regardless of chunk boundaries.
    sort(combined$results$x1)
  }

  values_2_chunks <- run_all_chunks(n_chunks = 2L)
  values_5_chunks <- run_all_chunks(n_chunks = 5L)

  expect_equal(values_2_chunks, values_5_chunks)
})

test_that("medsim_combine_chunks verbose=TRUE prints file count", {
  sc <- medsim_scenario(
    name = "combine_verbose_test",
    data_generator = function(n = 100) {
      data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n))
    },
    params = list(indirect = 0.0)
  )
  method <- function(data, params) {
    list(indirect = 0.1, indirect_ci_lower = 0, indirect_ci_upper = 0.3,
         indirect_p = 0.2, branch_switch = NA, converged = 1L)
  }
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  for (k in 1:2) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = 2L, output_dir = tmp_dir)
    medsim_run_chunk(list(sc), method, cfg, verbose = FALSE)
  }

  expect_message(
    medsim_combine_chunks(tmp_dir, verbose = TRUE),
    "reading 2 files"
  )
})
