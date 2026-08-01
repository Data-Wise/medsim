# Prerequisites P1-P3 of SPEC-medsim-chunked-run-gates-2026-07-31 (#34; fixes
# #36 and #38 plus two empirically-reproduced crashers). Each test encodes a
# planted defect from the spec's acceptance matrix, reproduced on dev@dd1c318
# before the fix.

.prereq_scenario <- function() {
  medsim_scenario(
    name = "prereq_s1",
    data_generator = function(n = 30) data.frame(x = stats::rnorm(n)),
    params = list()
  )
}

.prereq_method <- function(data, params) list(indirect = mean(data$x))

.run_chunks <- function(out, nsim = 20L, n_chunks = 4L,
                        method = .prereq_method) {
  for (k in seq_len(n_chunks)) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = n_chunks,
                         n_replications = nsim, n_cores = 1L,
                         output_dir = out)
    medsim_run_chunk(list(.prereq_scenario()), method, cfg, verbose = FALSE)
  }
  medsim_combine_chunks(out, verbose = FALSE)
}

# ---- P1: global replication ids -------------------------------------------

test_that("P1: combined chunks carry distinct GLOBAL replication ids (#36)", {
  out <- withr::local_tempdir()
  combined <- .run_chunks(out)
  # Planted defect (pre-fix): 5 distinct local ids, each appearing 4 times.
  expect_equal(sort(combined$results$replication), 1:20)
  expect_equal(max(table(combined$results$replication)), 1L)
})

test_that("P1: chunk 2's first row continues the global sequence", {
  out <- withr::local_tempdir()
  cfg <- medsim_config("test", chunk_id = 2L, n_chunks = 4L,
                       n_replications = 20L, n_cores = 1L, output_dir = out)
  medsim_run_chunk(list(.prereq_scenario()), .prereq_method, cfg,
                   verbose = FALSE)
  chunk2 <- readRDS(file.path(out, "chunk_0002.rds"))
  expect_equal(min(chunk2$results$replication), 6L)  # chunk size 5 -> 6..10
  expect_equal(max(chunk2$results$replication), 10L)
})

test_that("P1: standalone runs are unchanged (rep_offset 0 -> ids 1..nsim)", {
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                       n_cores = 1L)
  cfg$n_replications <- 5L
  res <- medsim_run(.prereq_method, list(.prereq_scenario()), cfg,
                    parallel = FALSE, verbose = FALSE)
  expect_equal(res$results$replication, 1:5)
})

test_that("P1: results carry schema v2 + meta-cols provenance attributes", {
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                       n_cores = 1L)
  cfg$n_replications <- 3L
  res <- medsim_run(.prereq_method, list(.prereq_scenario()), cfg,
                    parallel = FALSE, verbose = FALSE)
  expect_identical(attr(res$results, "medsim_schema", exact = TRUE), 2L)
  meta <- attr(res$results, "medsim_meta_cols", exact = TRUE)
  expect_true(all(c("scenario", "replication", "elapsed") %in% meta))
  # The provenance split keeps bookkeeping columns out of estimate analysis.
  expect_false("indirect" %in% meta)
})

# ---- P2: combine rebuilds metadata ----------------------------------------

test_that("P2: combine rebuilds $config and $summary over the full run", {
  out <- withr::local_tempdir()
  combined <- .run_chunks(out)
  # Planted defect (pre-fix): config$n_replications == 5 (chunk-1 size) and
  # $summary computed over chunk 1's rows only.
  expect_equal(combined$config$n_replications, 20L)
  expect_null(combined$config$rep_offset)
  expect_null(combined$config$chunk_id)
  # Summary must reflect all 20 rows: recompute independently and compare.
  expect_equal(combined$summary$indirect_mean,
               mean(combined$results$indirect))
  # medsim_analyze-level check (#36's headline symptom).
  expect_equal(max(combined$results$replication), 20L)
})

test_that("P2: schema attributes survive the combine rbind", {
  out <- withr::local_tempdir()
  combined <- .run_chunks(out)
  expect_identical(attr(combined$results, "medsim_schema", exact = TRUE), 2L)
})

# ---- P3: failure rows, logical fields, CSV clobber ------------------------

test_that("P3: a failing replication yields an NA row, not a crashed run", {
  # Planted defect (pre-fix): 'names do not match previous names' rbind crash.
  flaky <- function(data, params) {
    if (abs(mean(data$x)) < 0.05) stop("transient failure")
    list(indirect = mean(data$x), converged = 1)
  }
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                       n_cores = 1L)
  cfg$n_replications <- 40L
  res <- suppressWarnings(
    medsim_run(flaky, list(.prereq_scenario()), cfg,
               parallel = FALSE, verbose = FALSE))
  expect_equal(nrow(res$results), 40L)
  expect_true("error" %in% names(res$results))
  failed <- !is.na(res$results$error)
  expect_true(any(failed))               # the planted defect actually fired
  expect_true(all(is.na(res$results$indirect[failed])))
  expect_true(all(res$results$converged[failed] == 0))
  expect_true(all(res$results$converged[!failed] == 1))
})

test_that("P3: logical contract fields survive to $results", {
  # Planted defect (pre-fix): branch_switch/converged (logical) silently
  # dropped -> medsim_summarize_branch_switch() stops on missing column.
  meth <- function(data, params) {
    list(indirect = mean(data$x), branch_switch = NA, converged = TRUE)
  }
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                       n_cores = 1L)
  cfg$n_replications <- 3L
  res <- medsim_run(meth, list(.prereq_scenario()), cfg,
                    parallel = FALSE, verbose = FALSE)
  expect_true(all(c("branch_switch", "converged") %in% names(res$results)))
  expect_true(all(res$results$converged))
})

test_that("P3: chunk mode writes no intermediate CSVs (#38)", {
  # Planted defect (pre-fix): 4 concurrent chunks overwrite one
  # results_scenario_1.csv (last writer wins), leaving partial data that
  # looks like a complete per-scenario record.
  out <- withr::local_tempdir()
  invisible(.run_chunks(out))
  expect_length(list.files(out, pattern = "\\.csv$"), 0L)
  # The authoritative artifacts are the chunk .rds files.
  expect_length(list.files(out, pattern = "^chunk_\\d{4}\\.rds$"), 4L)
})

test_that("P3: standalone (non-chunk) runs still write their CSVs", {
  out <- withr::local_tempdir()
  cfg <- medsim_config("test", output_dir = out, n_cores = 1L)
  cfg$n_replications <- 3L
  invisible(medsim_run(.prereq_method, list(.prereq_scenario()), cfg,
                       parallel = FALSE, verbose = FALSE))
  expect_true(file.exists(file.path(out, "all_results.csv")))
})
