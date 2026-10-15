# Ground-truth integrity (#41): the acceptance matrix of
# internal/SPEC-medsim-41-truth-integrity-2026-08-22.md. Case numbers below
# match the SPEC's table. Each case fails against the pre-#41 code.

ti_gen <- function(n) data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n))
ti_sc <- function(name = "ti", indirect = 0.1) {
  medsim_scenario(name, data_generator = ti_gen, params = list(indirect = indirect))
}
ti_cfg <- function(out, reps = 4L) {
  cfg <- medsim_config("test", output_dir = out, n_cores = 1L)
  cfg$n_replications <- reps
  cfg$n_truth <- 50L
  cfg
}
ti_method <- function(data, params) {
  list(indirect = 0.1, indirect_ci_lower = 0, indirect_ci_upper = 0.2)
}
ti_good_truth <- function(data, params) list(indirect = params$indirect)
ti_run <- function(scenarios, truth, out, ...) {
  suppressMessages(medsim_run(ti_method, scenarios, ti_cfg(out), compute_truth = truth,
                              parallel = FALSE, verbose = FALSE, ...))
}
ti_types <- function(v) vapply(v, `[[`, character(1), "type")
ti_capture_warnings <- function(expr) {
  w <- character()
  value <- withCallingHandlers(expr, warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = w)
}

# ---- Layer 1: D5 + D6 + D8 ---------------------------------------------------

test_that("case 1 (E-1): a transient truth failure is retried, not reused from cache", {
  out <- withr::local_tempdir()
  attempts <- 0L
  flaky <- function(data, params) {
    attempts <<- attempts + 1L
    if (attempts == 1L) stop("transient")
    list(indirect = params$indirect)
  }
  expect_error(ti_run(list(ti_sc()), flaky, out, truth_on_failure = "stop"),
               class = "medsim_truth_failure_error")
  res <- ti_run(list(ti_sc()), flaky, out)
  expect_identical(attempts, 2L)          # recomputed, the failure was not cached
  expect_equal(res$truth$indirect, 0.1)

  # same under truth_on_failure = "warn"
  out2 <- withr::local_tempdir()
  attempts <- 0L
  first <- ti_capture_warnings(ti_run(list(ti_sc()), flaky, out2,
                                      truth_on_failure = "warn"))
  expect_null(first$value$truth)
  ti_run(list(ti_sc()), flaky, out2)
  expect_identical(attempts, 2L)
})

test_that("case 1 negative: a successful truth is cached and reused", {
  out <- withr::local_tempdir()
  attempts <- 0L
  counted <- function(data, params) {
    attempts <<- attempts + 1L
    list(indirect = params$indirect)
  }
  ti_run(list(ti_sc()), counted, out)
  ti_run(list(ti_sc()), counted, out)
  expect_identical(attempts, 1L)
})

test_that("case 2: a failed truth computation leaves no cache file", {
  out <- withr::local_tempdir()
  expect_error(ti_run(list(ti_sc()), function(data, params) stop("boom"), out),
               class = "medsim_truth_failure_error")
  expect_false(file.exists(file.path(out, "truth_scenario_1.rds")))
  ti_run(list(ti_sc()), ti_good_truth, out)
  expect_true(file.exists(file.path(out, "truth_scenario_1.rds")))
})

test_that("case 3: a LEGACY unclassed poisoned cache self-heals", {
  out <- withr::local_tempdir()
  sc <- ti_sc()
  attempts <- 0L
  counted <- function(data, params) {
    attempts <<- attempts + 1L
    list(indirect = params$indirect)
  }
  fp <- .medsim_truth_fingerprint(sc, counted)
  # exactly what medsim < 0.6.0 wrote after a failed truth call
  suppressMessages(medsim_cache_save(list(truth = list(error = "old failure"),
                                          fingerprint = fp),
                                     file.path(out, "truth_scenario_1.rds")))
  res <- ti_run(list(sc), counted, out)
  expect_identical(attempts, 1L)
  expect_equal(res$truth$indirect, 0.1)
})

test_that("cases 3 + 9 negative: a valid cache, including a numeric `error` parameter, is loaded", {
  out <- withr::local_tempdir()
  attempts <- 0L
  with_error_param <- function(data, params) {
    attempts <<- attempts + 1L
    list(indirect = params$indirect, error = 0.5)
  }
  first <- ti_run(list(ti_sc()), with_error_param, out)
  second <- ti_run(list(ti_sc()), with_error_param, out)
  expect_identical(attempts, 1L)
  expect_equal(second$truth$error, 0.5)
  expect_false(.medsim_is_truth_sentinel(list(error = 0.5)))
  expect_true(.medsim_is_truth_sentinel(list(error = "boom")))
})

test_that("case 4: truth_on_failure = 'stop' names the scenario and the original error", {
  out <- withr::local_tempdir()
  err <- tryCatch(
    ti_run(list(ti_sc("only_one")), function(data, params) stop("deliberate"), out,
           truth_on_failure = "stop"),
    medsim_truth_failure_error = function(e) e)
  expect_s3_class(err, "medsim_truth_failure_error")
  expect_s3_class(err, "medsim_error")
  expect_identical(err$scenario, "only_one")
  expect_match(conditionMessage(err), "deliberate")
  expect_identical(conditionMessage(err$parent), "deliberate")
})

test_that("case 4: the default is 'stop' (flipped for 0.6.0, decision D-1); 'warn' continues", {
  # formals() holds the package's own default, c("stop", "warn"); eval() only
  # evaluates that literal to read its first element.
  expect_identical(eval(formals(medsim_run)$truth_on_failure)[1L], "stop")
  expect_identical(eval(formals(medsim_run_chunk)$truth_on_failure)[1L], "stop")
  out <- withr::local_tempdir()
  got <- ti_capture_warnings(
    ti_run(list(ti_sc("a"), ti_sc("b")), function(data, params) stop("all fail"), out,
           truth_on_failure = "warn"))
  expect_true(any(grepl("Truth computation failed for a", got$warnings)))
  expect_null(got$value$truth)            # absent, not a degenerate frame
})

test_that("case 5 (E-3): a mixed failure under 'warn' completes; the failed scenario is absent", {
  out <- withr::local_tempdir()
  half <- function(data, params) {
    if (params$indirect == 0) stop("fails for null")
    list(indirect = params$indirect)
  }
  got <- ti_capture_warnings(ti_run(list(ti_sc("ok", 0.1), ti_sc("bad", 0)), half, out,
                                    truth_on_failure = "warn"))
  expect_identical(got$value$truth$scenario, "ok")
  expect_identical(names(got$value$truth), c("scenario", "indirect"))
})

test_that("case 15 (D8): a successful but unreadable truth is reported, with the required shape", {
  out <- withr::local_tempdir()
  vec <- function(data, params) c(indirect = 0.09)
  expect_error(ti_run(list(ti_sc()), vec, out, truth_on_failure = "stop"),
               "NAMED LIST of length-1 numerics")
  len2 <- function(data, params) list(indirect = c(0.05, 0.15))
  got <- ti_capture_warnings(ti_run(list(ti_sc()), len2, withr::local_tempdir(),
                                    truth_on_failure = "warn"))
  expect_true(any(grepl("has no usable parameter", got$warnings)))
  expect_null(got$value$truth)
  expect_false(file.exists(file.path(out, "truth_scenario_1.rds")))   # never cached
  # negative: the correct shape is silent, one parameter column
  ok <- ti_run(list(ti_sc()), ti_good_truth, withr::local_tempdir())
  expect_identical(names(ok$truth), c("scenario", "indirect"))
})

test_that("case 16 (R4): scenarios with different parameter sets bind on the column union", {
  out <- withr::local_tempdir()
  ragged <- function(data, params) {
    if (params$indirect == 0) list(a_path = 0.3) else list(indirect = params$indirect)
  }
  res <- ti_run(list(ti_sc("x", 0.1), ti_sc("y", 0)), ragged, out)
  expect_setequal(names(res$truth), c("scenario", "indirect", "a_path"))
  expect_true(is.na(res$truth$a_path[res$truth$scenario == "x"]))
})

# ---- Layer 3: D7 -------------------------------------------------------------

test_that("cases 8 + 14 (D7): both analysis entry points stop on a parameter-less truth", {
  out <- withr::local_tempdir()
  res <- ti_run(list(ti_sc()), ti_good_truth, out)
  res$truth <- data.frame(scenario = "ti")          # degenerate frame
  expect_error(medsim_analyze(res), "ground truth has no parameter columns")
  expect_error(medsim_analyze_coverage(res, ci_levels = 0.95),
               "ground truth has no parameter columns")
  # the interval branch is dispatched after the same guard
  est <- medsim_estimand("interval", params = "indirect")
  expect_error(medsim_analyze_coverage(res, estimand = est),
               "ground truth has no parameter columns")
})

test_that("case 8 negative: a subset analysis still only warns per missing parameter", {
  out <- withr::local_tempdir()
  two_ci <- function(data, params) {
    c(ti_method(data, params), list(other_ci_lower = 0, other_ci_upper = 1))
  }
  res <- suppressMessages(medsim_run(two_ci, list(ti_sc()), ti_cfg(out),
                                     compute_truth = ti_good_truth,
                                     parallel = FALSE, verbose = FALSE))
  expect_warning(cov <- medsim_analyze_coverage(res, ci_levels = 0.95), "No ground truth")
  expect_identical(cov$coverage$parameter, "indirect")
})

# ---- D3 + D4 -----------------------------------------------------------------

test_that("case 11: a failed cache rename stops loudly, leaves the target and no tmp", {
  out <- withr::local_tempdir()
  target <- file.path(out, "truth_scenario_1.rds")
  dir.create(target)                                 # a directory: rename must fail
  writeLines("keep", file.path(target, "inside"))
  expect_error(suppressWarnings(medsim_cache_save(list(a = 1), target)),
               "failed to atomically rename")
  expect_true(dir.exists(target))
  expect_length(list.files(out, pattern = "\\.tmp$", all.files = TRUE), 0L)
  # negative: a normal save writes the target and leaves no tmp
  ok <- file.path(out, "ok.rds")
  suppressMessages(medsim_cache_save(list(a = 1), ok))
  expect_true(file.exists(ok))
  expect_length(list.files(out, pattern = "\\.tmp$", all.files = TRUE), 0L)
})

test_that("case 18 (R6): a save never touches another writer's tmp file", {
  out <- withr::local_tempdir()
  target <- file.path(out, "truth_scenario_1.rds")
  # the fixed tmp path a naive atomic write would share across writers
  other <- file.path(out, ".truth_scenario_1.rds.tmp")
  writeLines("another writer, mid-write", other)
  suppressMessages(medsim_cache_save(list(a = 1), target))
  expect_identical(readLines(other), "another writer, mid-write")
  expect_equal(readRDS(target)$data, list(a = 1))
})

test_that("case 12 (D4): ?medsim_config says seed drives the truth pass", {
  # Source tree under devtools::test(); installed help under R CMD check,
  # where tests run from medsim.Rcheck/ and there is no man/ to read.
  rd_file <- test_path("..", "..", "man", "medsim_config.Rd")
  rd <- if (file.exists(rd_file)) {
    paste(readLines(rd_file), collapse = " ")
  } else {
    db <- tools::Rd_db("medsim")
    skip_if(is.null(db[["medsim_config.Rd"]]), "installed Rd not available")
    paste(as.character(db[["medsim_config.Rd"]]), collapse = " ")
  }
  expect_match(rd, "Consulted directly by", fixed = TRUE)
  expect_match(rd, "Does not", fixed = TRUE)
})

# ---- D1 + D2' + R5 -----------------------------------------------------------

ti_chunk <- function(out, id, n_chunks = 2L, truth = ti_good_truth, every = FALSE,
                     scenarios = list(ti_sc()), on_failure = "stop") {
  cfg <- ti_cfg(out, reps = 4L)
  cfg$n_chunks <- n_chunks
  cfg$chunk_id <- id
  if (every) cfg$compute_truth_every_chunk <- TRUE
  suppressMessages(medsim_run_chunk(scenarios, ti_method, cfg, verbose = FALSE,
                                    compute_truth = truth,
                                    truth_on_failure = on_failure))
}

test_that("case 10 (D1): only chunk 1 computes truth unless every chunk is opted in", {
  out <- withr::local_tempdir()
  expect_false(is.null(readRDS(ti_chunk(out, 1L))$truth))
  expect_null(readRDS(ti_chunk(out, 2L))$truth)
  out2 <- withr::local_tempdir()
  expect_false(is.null(readRDS(ti_chunk(out2, 2L, every = TRUE))$truth))
  expect_true(attr(readRDS(ti_chunk(out2, 1L)), "provenance")$truth_requested)
})

test_that("case 17 (R5): truth carried only by a later chunk survives the combine", {
  out <- withr::local_tempdir()
  ti_chunk(out, 1L, truth = NULL)                    # chunk 1 has no truth
  ti_chunk(out, 2L, every = TRUE)                    # chunk 2 does
  combined <- suppressWarnings(
    medsim_combine_chunks(out, expected_chunks = 2L, on_violation = "ignore",
                          verbose = FALSE))
  expect_equal(combined$truth$indirect, 0.1)
  expect_equal(nrow(combined$truth), 1L)
})

test_that("cases 6 + 17: lost or missing truth raises truth_coverage at combine", {
  # (c) requested but lost. Under the default "stop", chunk 1 aborts before any
  # replication and writes no file; chunk 2 still records truth_requested.
  out <- withr::local_tempdir()
  expect_error(ti_chunk(out, 1L, truth = function(data, params) stop("x")),
               class = "medsim_truth_failure_error")
  expect_false(file.exists(file.path(out, .medsim_chunk_filename(1L))))
  ti_chunk(out, 2L)
  expect_error(suppressWarnings(medsim_combine_chunks(out, verbose = FALSE)),
               "truth_coverage/missing")

  # (b) one scenario's truth failed under "warn"
  out2 <- withr::local_tempdir()
  half <- function(data, params) {
    if (params$indirect == 0) stop("fails for null")
    list(indirect = params$indirect)
  }
  scs <- list(ti_sc("ok", 0.1), ti_sc("bad", 0))
  ti_capture_warnings(ti_chunk(out2, 1L, n_chunks = 1L, truth = half, scenarios = scs,
                               on_failure = "warn"))
  got <- ti_capture_warnings(
    medsim_combine_chunks(out2, on_violation = "warn", verbose = FALSE))
  expect_true(any(grepl("truth_coverage/missing: 1 scenario(s) have results but no truth row (bad)",
                        got$warnings, fixed = TRUE)))
})

test_that("cases 7 + 19: degenerate truth is flagged; no-truth runs and data.frames stay silent", {
  out <- withr::local_tempdir()
  ti_chunk(out, 1L, n_chunks = 1L)
  combined <- suppressWarnings(medsim_combine_chunks(out, verbose = FALSE))

  bad <- combined
  bad$truth <- data.frame(scenario = "ti")
  v <- suppressWarnings(medsim_audit_results(bad, on_violation = "ignore"))
  expect_true(any(grepl("truth_coverage/degenerate",
                        vapply(v, `[[`, character(1), "message"))))
  # routing: "warn" downgrades, "stop" raises
  expect_warning(medsim_audit_results(bad, on_violation = "warn"), "truth_coverage")
  expect_error(suppressWarnings(medsim_audit_results(bad)), "truth_coverage")

  # negative: a run that never requested truth is silent
  out2 <- withr::local_tempdir()
  ti_chunk(out2, 1L, n_chunks = 1L, truth = NULL)
  none <- suppressWarnings(medsim_combine_chunks(out2, verbose = FALSE))
  expect_null(none$truth)
  expect_false("truth_coverage" %in%
                 ti_types(suppressWarnings(medsim_audit_results(none, on_violation = "ignore"))))
  # a bare data.frame has no $truth to check, and must not error
  expect_false("truth_coverage" %in%
                 ti_types(suppressWarnings(medsim_audit_results(combined$results,
                                                                on_violation = "ignore"))))
})

test_that("case 13: a clean run with successful truth is unchanged", {
  out <- withr::local_tempdir()
  res <- ti_run(list(ti_sc("a", 0.1), ti_sc("b", 0.2)), ti_good_truth, out)
  expect_equal(res$truth, data.frame(scenario = c("a", "b"), indirect = c(0.1, 0.2)))
  cov <- medsim_analyze_coverage(res, ci_levels = 0.95)
  expect_equal(cov$coverage$n_valid, 8L)
})
