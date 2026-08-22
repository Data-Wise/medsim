# Verification suite for #43 PR-B (runner, chunk write, accounting).
# Cases numbered per internal/GRILL-medsim-43-parallel-fail-loud-2026-07-31.md
# section 5. Case 5 removed (fail-fast dropped, Q2); cases 8/9/12 live in
# test-cluster.R (PR-A, hand-built fixtures).

pfl_gen <- function(n = 100) data.frame(X = rnorm(n), Y = rnorm(n))

pfl_scenario <- function(name = "pfl", params = list()) {
  medsim_scenario(name = name, data_generator = pfl_gen, params = params)
}

pfl_config <- function(out_dir, n_reps = 4L) {
  cfg <- medsim_config("test", output_dir = out_dir, n_cores = 1L)
  cfg$n_replications <- n_reps
  cfg
}

reserved_method <- function(data, params) list(indirect = 0.1, error = 0.05)

# ---- Case 1: reserved-error under real parallel() --------------------------

test_that("reserved-error under real parallel raises medsim_run_failure, untruncated (#43 case 1)", {
  skip_on_cran()
  skip_on_os("windows")
  out <- withr::local_tempdir()
  cfg <- pfl_config(out, n_reps = 4L)
  cfg$n_cores <- 2L

  err <- tryCatch(
    medsim_run(reserved_method, list(pfl_scenario()), cfg,
              parallel = TRUE, verbose = FALSE),
    medsim_run_failure = function(e) e
  )
  expect_s3_class(err, "medsim_run_failure")
  expect_true(grepl("reserved", conditionMessage(err)))
  expect_gt(nchar(conditionMessage(err)), 60L)  # untruncated (B4)
})

# ---- Case 2: intermittent DGP failure ---------------------------------------

test_that("intermittent DGP failure produces a prefixed row without poisoning converged/failure_rate (#43 case 2)", {
  out <- withr::local_tempdir()
  cfg <- pfl_config(out, n_reps = 4L)

  call_n <- 0L
  flaky_gen <- function(n = 100) {
    call_n <<- call_n + 1L
    if (call_n == 2L) stop("singular draw")
    data.frame(X = rnorm(n))
  }
  sc <- list(medsim_scenario(name = "flaky", data_generator = flaky_gen,
                            params = list()))
  ok_method <- function(data, params) list(indirect = mean(data$X),
                                           converged = TRUE)

  res <- medsim_run(ok_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  df <- res$results
  expect_equal(nrow(df), 4L)

  is_dgp <- .medsim_is_dgp_failure(df$error)
  expect_equal(sum(is_dgp), 1L)
  expect_true(startsWith(df$error[is_dgp], "data_generator error: "))
  # DGP-failure row never had method() run -- converged stays NA, not 0
  # (forcing 0 would poison converged_mean, #43 review round 4).
  expect_true(all(is.na(df$converged[is_dgp])))
  expect_true(all(df$converged[!is_dgp]))
})

# ---- Case 3: all tasks failed -----------------------------------------------

test_that("all tasks failing yields a medsim_run_failure with NULL/empty results, never silent (#43 case 3)", {
  out <- withr::local_tempdir()
  cfg <- pfl_config(out, n_reps = 3L)

  err <- tryCatch(
    medsim_run(reserved_method, list(pfl_scenario()), cfg,
              parallel = FALSE, verbose = FALSE),
    medsim_run_failure = function(e) e
  )
  expect_s3_class(err, "medsim_run_failure")
  expect_true(is.null(err$results$results) || nrow(err$results$results) == 0L)
  expect_equal(length(err$errors), 3L)
})

# ---- Case 4: failure in scenario 2 of 3 -------------------------------------

test_that("failure in scenario 2 of 3 keeps scenario 1 complete and never starts scenario 3 (#43 case 4)", {
  out <- withr::local_tempdir()
  cfg <- pfl_config(out, n_reps = 3L)

  # A plain stop() inside method() is a NORMAL, handled failure (becomes a
  # failure row, run continues) -- not a task-level error. Use the reserved-
  # `error`-field contract violation instead: method() returns SUCCESSFULLY,
  # so the escape happens in runner.R's post-method check, outside the
  # method-level tryCatch, which is what actually reaches this sweep.
  call_n <- 0L
  flaky_method <- function(data, params) {
    if (isTRUE(params$fail)) {
      call_n <<- call_n + 1L
      if (call_n == 2L) return(list(indirect = 0.1, error = 0.05))
    }
    list(indirect = mean(data$X))
  }
  sc <- list(
    pfl_scenario("S1", params = list(fail = FALSE)),
    pfl_scenario("S2", params = list(fail = TRUE)),
    pfl_scenario("S3", params = list(fail = FALSE))
  )

  err <- tryCatch(
    medsim_run(flaky_method, sc, cfg, parallel = FALSE, verbose = FALSE),
    medsim_run_failure = function(e) e
  )
  expect_s3_class(err, "medsim_run_failure")
  df <- err$results$results
  expect_equal(sort(unique(df$scenario)), c("S1", "S2"))
  expect_equal(sum(df$scenario == "S1"), 3L)
  expect_equal(sum(df$scenario == "S2"), 2L)  # rep 1 and 3 survived rep 2's failure
})

# ---- Case 6: serial/parallel parity of class and payload shape -------------

test_that("serial and parallel failures produce the same condition class and payload shape (#43 case 6)", {
  skip_on_cran()
  skip_on_os("windows")
  out_s <- withr::local_tempdir()
  out_p <- withr::local_tempdir()

  cfg_s <- pfl_config(out_s, n_reps = 4L)
  cfg_p <- pfl_config(out_p, n_reps = 4L)
  cfg_p$n_cores <- 2L

  err_s <- tryCatch(
    medsim_run(reserved_method, list(pfl_scenario()), cfg_s,
              parallel = FALSE, verbose = FALSE),
    medsim_run_failure = function(e) e
  )
  err_p <- tryCatch(
    medsim_run(reserved_method, list(pfl_scenario()), cfg_p,
              parallel = TRUE, verbose = FALSE),
    medsim_run_failure = function(e) e
  )

  expect_identical(class(err_s), class(err_p))
  expect_true(all(c("scenario", "global_rep_id", "message") %in%
                    names(err_s$errors[[1L]])))
  expect_true(all(c("scenario", "global_rep_id", "message") %in%
                    names(err_p$errors[[1L]])))
  expect_s3_class(err_s$results, "medsim_results")
  expect_s3_class(err_p$results, "medsim_results")
})

# ---- Case 7: chunk write rescues partial results, re-stops, combine flags --

test_that("medsim_run_chunk writes a partial=TRUE chunk on task failure, re-stops, and combine flags it (#43 case 7)", {
  out <- withr::local_tempdir()
  cfg <- medsim_config("test", output_dir = out, n_cores = 1L)
  cfg$n_replications <- 4L
  cfg$n_chunks <- 1L
  cfg$chunk_id <- 1L

  call_n <- 0L
  flaky_method <- function(data, params) {
    call_n <<- call_n + 1L
    if (call_n == 3L) return(list(indirect = 0.1, error = 0.05))
    list(indirect = mean(data$X))
  }
  sc <- list(pfl_scenario())

  expect_error(
    medsim_run_chunk(sc, flaky_method, cfg, verbose = FALSE),
    class = "medsim_run_failure"
  )

  chunk_path <- file.path(out, .medsim_chunk_filename(1L))
  expect_true(file.exists(chunk_path))
  chunk <- readRDS(chunk_path)
  expect_true(isTRUE(attr(chunk, "provenance")$partial))
  # lapply calls every task regardless of an earlier failure -- only rep 3
  # (call_n == 3) actually fails; reps 1, 2, 4 all survive independently.
  expect_equal(nrow(chunk$results), 3L)

  expect_warning(
    combined <- medsim_combine_chunks(out, on_violation = "warn", verbose = FALSE),
    "partial_chunk"
  )
  expect_equal(nrow(combined$results), 3L)

  # Manual re-run overwrites the partial file (not a --requeue path, but the
  # atomic write must not refuse a second attempt).
  call_n <<- 0L
  ok_method <- function(data, params) list(indirect = mean(data$X))
  medsim_run_chunk(sc, ok_method, cfg, verbose = FALSE)
  chunk2 <- readRDS(chunk_path)
  expect_false(isTRUE(attr(chunk2, "provenance")$partial))
  expect_equal(nrow(chunk2$results), 4L)
})

# ---- Case 10: DGP failures still detected at combine (B1 regression guard) -

test_that("DGP failures are still detected at combine via the dgp_failure gate (#43 case 10, B1 regression guard)", {
  out <- withr::local_tempdir()
  cfg <- medsim_config("test", output_dir = out, n_cores = 1L)
  cfg$n_replications <- 4L
  cfg$n_chunks <- 1L
  cfg$chunk_id <- 1L

  call_n <- 0L
  flaky_gen <- function(n = 100) {
    call_n <<- call_n + 1L
    if (call_n == 2L) stop("singular")
    data.frame(X = rnorm(n))
  }
  sc <- list(medsim_scenario(name = "S1", data_generator = flaky_gen,
                            params = list()))
  ok_method <- function(data, params) list(indirect = mean(data$X))

  medsim_run_chunk(sc, ok_method, cfg, verbose = FALSE)

  # A contiguous 1..4 id block with no gaps -- contiguity alone (rep_gap)
  # finds NOTHING here; dgp_failure is the only detector that can.
  expect_warning(
    combined <- medsim_combine_chunks(out, on_violation = "warn", verbose = FALSE),
    "dgp_failure"
  )
  expect_equal(nrow(combined$results), 4L)
})

# ---- Case 11: .medsim_rbind_reps defense-in-depth ---------------------------

test_that(".medsim_rbind_reps stops on a medsim_error input (#43 case 11, §3 defense in depth)", {
  good <- data.frame(scenario = "s1", replication = 1L, indirect = 0.1)
  bad <- structure(list(message = "boom", task = 1L),
                   class = c("medsim_error", "error", "condition"))
  expect_error(.medsim_rbind_reps(list(good, bad)), "medsim_error")
})

# ---- Case 13: negative control ----------------------------------------------

test_that("a clean run gives identical values under real parallel and serial execution (#43 case 13, negative control)", {
  skip_on_cran()
  skip_on_os("windows")
  out_s <- withr::local_tempdir()
  out_p <- withr::local_tempdir()
  ok_method <- function(data, params) list(indirect = mean(data$X * data$Y))

  cfg_s <- pfl_config(out_s, n_reps = 4L)
  cfg_p <- pfl_config(out_p, n_reps = 4L)
  cfg_p$n_cores <- 2L

  res_s <- medsim_run(ok_method, list(pfl_scenario()), cfg_s,
                      parallel = FALSE, verbose = FALSE)
  res_p <- medsim_run(ok_method, list(pfl_scenario()), cfg_p,
                      parallel = TRUE, verbose = FALSE)

  cols <- c("scenario", "replication", "indirect")
  expect_equal(
    res_s$results[order(res_s$results$replication), cols],
    res_p$results[order(res_p$results$replication), cols]
  )
})

# ---- Supporting guards: medsim_check_results / medsim_summarize_results ----

test_that("medsim_check_results handles an empty results list without erroring (#43 §2)", {
  expect_message(out <- medsim_check_results(list()), "No tasks")
  expect_equal(out, integer(0))
})

test_that("medsim_summarize_results reports NA (not Inf) min/max for an all-NA cell (#43 §1)", {
  df <- data.frame(
    scenario = c("s1", "s1"),
    estimate = c(NA_real_, NA_real_)
  )
  summ <- medsim_summarize_results(df)
  expect_true(is.na(summ$estimate_min))
  expect_true(is.na(summ$estimate_max))
  expect_false(any(is.infinite(c(summ$estimate_min, summ$estimate_max))))
})
