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

# Config for the cases that must exercise the REAL parallel path (#50 PM2).
# medsim_run_parallel() falls back to the sequential path on any of three
# conditions, and all three are live in a check environment:
#   1. _R_CHECK_LIMIT_CORES_ set  -- R CMD check --as-cran sets it
#   2. n_cores == 1               -- pfl_config()'s default
#   3. length(tasks) < 4          -- tasks are per-replication (runner.R:228)
# The fallback is correctness-preserving, so a case that silently takes it
# still passes every assertion while covering none of the parallel code.
# n_reps = 6 keeps condition 3 off the 4-task boundary that 4 sat exactly on.
pfl_parallel_config <- function(out_dir, n_reps = 6L) {
  withr::local_envvar(.local_envir = parent.frame(),
                      "_R_CHECK_LIMIT_CORES_" = NA)
  cfg <- pfl_config(out_dir, n_reps = n_reps)
  cfg$n_cores <- 2L
  cfg
}

# ---- Case 1: reserved-error under real parallel() --------------------------

test_that("reserved-error under real parallel raises medsim_run_failure, untruncated (#43 case 1)", {
  skip_on_cran()
  skip_on_os("windows")
  out <- withr::local_tempdir()
  cfg <- pfl_parallel_config(out)

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

  cfg_s <- pfl_config(out_s, n_reps = 6L)
  cfg_p <- pfl_parallel_config(out_p)

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

# ---- Concurrent attempts at one chunk: unique tmp + acceptance rule --------

chunk_cfg <- function(out) {
  cfg <- medsim_config("test", output_dir = out, n_cores = 1L)
  cfg$n_replications <- 4L
  cfg$n_chunks <- 1L
  cfg$chunk_id <- 1L
  cfg
}
good_method <- function(data, params) list(indirect = mean(data$X))
make_failing_method <- function() {
  call_n <- 0L
  function(data, params) {
    call_n <<- call_n + 1L
    if (call_n == 3L) return(list(indirect = 0.1, error = 0.05))
    list(indirect = mean(data$X))
  }
}

test_that("medsim_run_chunk does not touch another attempt's in-flight tmp file", {
  out <- withr::local_tempdir()
  # Planted defect: the tmp path every attempt shared before this fix,
  # standing in for an overlapping attempt that is still writing.
  other_tmp <- file.path(out, paste0(".", .medsim_chunk_filename(1L), ".tmp"))
  writeLines("in-flight write of another attempt", other_tmp)

  path <- medsim_run_chunk(list(pfl_scenario()), good_method, chunk_cfg(out),
                           verbose = FALSE)

  expect_true(file.exists(other_tmp))
  expect_identical(readLines(other_tmp), "in-flight write of another attempt")
  expect_s3_class(readRDS(path), "medsim_results")
  # this attempt's own tmp file is gone; only the planted one remains
  expect_identical(list.files(out, pattern = "\\.tmp$", all.files = TRUE),
                   basename(other_tmp))
})

test_that("a partial attempt never replaces a complete chunk; a complete one replaces a partial", {
  out <- withr::local_tempdir()
  sc <- list(pfl_scenario())
  chunk_path <- file.path(out, .medsim_chunk_filename(1L))

  # complete first, then a failing retry finishes last
  medsim_run_chunk(sc, good_method, chunk_cfg(out), verbose = FALSE)
  complete <- readRDS(chunk_path)
  expect_warning(
    expect_error(
      medsim_run_chunk(sc, make_failing_method(), chunk_cfg(out), verbose = FALSE),
      class = "medsim_run_failure"
    ),
    "already holds a complete result"
  )
  kept <- readRDS(chunk_path)
  expect_false(isTRUE(attr(kept, "provenance")$partial))
  expect_identical(kept$results, complete$results)
  expect_length(list.files(out, pattern = "\\.tmp$", all.files = TRUE), 0L)

  # partial first, then a complete retry replaces it
  out2 <- withr::local_tempdir()
  path2 <- file.path(out2, .medsim_chunk_filename(1L))
  expect_error(
    medsim_run_chunk(sc, make_failing_method(), chunk_cfg(out2), verbose = FALSE),
    class = "medsim_run_failure"
  )
  expect_true(isTRUE(attr(readRDS(path2), "provenance")$partial))
  medsim_run_chunk(sc, good_method, chunk_cfg(out2), verbose = FALSE)
  expect_false(isTRUE(attr(readRDS(path2), "provenance")$partial))
  expect_equal(nrow(readRDS(path2)$results), 4L)
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
  ok_method <- function(data, params) {
    list(indirect = mean(data$X * data$Y), worker_pid = Sys.getpid())
  }

  cfg_s <- pfl_config(out_s, n_reps = 6L)
  cfg_p <- pfl_parallel_config(out_p)

  res_s <- medsim_run(ok_method, list(pfl_scenario()), cfg_s,
                      parallel = FALSE, verbose = FALSE)
  res_p <- medsim_run(ok_method, list(pfl_scenario()), cfg_p,
                      parallel = TRUE, verbose = FALSE)

  # Self-certifying (#50 PM2): before the env-var fix this compared the serial
  # path to ITSELF -- a negative control that passed for exactly the wrong
  # reason. Assert each arm really ran where it claims to. `worker_pid` is not
  # in `cols`, so the parity comparison below is unaffected.
  expect_true(all(res_s$results$worker_pid == Sys.getpid()))
  expect_true(all(res_p$results$worker_pid != Sys.getpid()))

  cols <- c("scenario", "replication", "indirect")
  expect_equal(
    res_s$results[order(res_s$results$replication), cols],
    res_p$results[order(res_p$results$replication), cols]
  )
})

# ---- Case 20: all-failed chunk still combines with a violation (#50 PM1) ---

test_that("a trailing all-reps-failed chunk writes a flagged file and fires partial_chunk (#50 PM1)", {
  skip_on_cran()
  skip_on_os("windows")
  out <- withr::local_tempdir()

  # Real parallel on both chunks -- PM2's fix is what makes this meaningful;
  # under the serial fallback this would not exercise the parallel failure
  # path the reserved-`error` class actually travels.
  #
  # NOTE: cleared HERE, not inside mk_cfg(). pfl_parallel_config() scopes its
  # withr cleanup to `parent.frame()`, so calling it from a nested helper
  # would restore _R_CHECK_LIMIT_CORES_ the instant that helper returns --
  # re-arming the very fallback this case needs off.
  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = NA)

  # 12 reps over 2 chunks = 6 tasks PER CHUNK. medsim_run_chunk() re-splits
  # n_replications across the array, so the 4-task parallel floor applies to
  # the per-chunk count, not the total -- 6 total would give 3 per chunk and
  # silently fall back to serial.
  mk_cfg <- function(chunk_id) {
    cfg <- pfl_parallel_config(out, n_reps = 12L)
    cfg$chunk_id <- chunk_id
    cfg$n_chunks <- 2L
    cfg
  }

  ok_method <- function(data, params) {
    list(indirect = mean(data$X), worker_pid = Sys.getpid())
  }

  # Chunk 1 succeeds -- and must do so on real workers, or this case degrades
  # into a serial-path test without saying so.
  medsim_run_chunk(list(pfl_scenario()), ok_method, mk_cfg(1L), verbose = FALSE)
  ch1 <- readRDS(file.path(out, .medsim_chunk_filename(1L)))
  expect_true(all(ch1$results$worker_pid != Sys.getpid()))

  # Chunk 2 fails EVERY rep (reserved `error` field) -- nothing survives.
  err <- tryCatch(
    medsim_run_chunk(list(pfl_scenario()), reserved_method, mk_cfg(2L),
                     verbose = FALSE),
    medsim_run_failure = function(e) e
  )
  expect_s3_class(err, "medsim_run_failure")

  # PM1: the file must exist even though zero reps survived -- it is the only
  # carrier of `partial = TRUE`.
  expect_true(file.exists(file.path(out, .medsim_chunk_filename(2L))))
  ch2 <- readRDS(file.path(out, .medsim_chunk_filename(2L)))
  expect_true(isTRUE(attr(ch2, "provenance")$partial))

  # ...and the combine must therefore refuse to call this a clean run.
  # on_violation = "stop" carries the typed violation list on the condition,
  # so this asserts the exact type rather than regex-matching a message.
  cv <- tryCatch(
    medsim_combine_chunks(out, on_violation = "stop"),
    medsim_combine_violation = function(e) e
  )
  expect_s3_class(cv, "medsim_combine_violation")
  expect_true("partial_chunk" %in%
                vapply(cv$violations, `[[`, character(1), "type"))

  # The rescue path still returns chunk 1's completed work.
  expect_equal(sort(unique(cv$results$results$scenario)), "pfl")
})

# ---- Case 21: EVERY chunk failed -- diagnose it correctly (#50 review) -----

test_that("an all-chunks-failed combine reports the real cause, not a false schema warning (#50)", {
  skip_on_cran()
  out <- withr::local_tempdir()

  mk_cfg <- function(chunk_id) {
    cfg <- pfl_config(out, n_reps = 8L)
    cfg$chunk_id <- chunk_id
    cfg$n_chunks <- 2L
    cfg
  }

  # Both chunks fail every rep -- only reachable since PM1 made a zero-row
  # chunk write a file at all.
  for (k in 1:2) {
    expect_error(
      medsim_run_chunk(list(pfl_scenario()), reserved_method, mk_cfg(k),
                       verbose = FALSE),
      class = "medsim_run_failure")
  }
  expect_equal(length(list.files(out, pattern = "^chunk_")), 2L)

  warns <- character(0)
  combined <- withCallingHandlers(
    medsim_combine_chunks(out, on_violation = "warn"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  # The real cause, stated correctly: not "holds only its completed reps".
  expect_true(any(grepl("partial_chunk", warns, fixed = TRUE)))
  expect_true(any(grepl("hold no results at all", warns, fixed = TRUE)))

  # ...and NOT a version-mismatch red herring. attr(NULL, "medsim_schema") is
  # NULL, so the audit used to claim these chunks came from an older medsim
  # and tell the user to re-run them -- actively wrong advice on a run whose
  # actual problem is that every replication failed.
  expect_false(any(grepl("older medsim", warns, fixed = TRUE)))
  expect_false(any(grepl("no non-missing arguments to max", warns, fixed = TRUE)))

  # -Inf must never be baked into the returned config.
  expect_equal(combined$config$n_replications, 0L)
  expect_null(combined$results)
})

# ---- Guard: real parallelism actually engages (#50 PM2) --------------------

test_that("the real-parallel cases run on worker processes, not the fallback (#50 PM2)", {
  skip_on_cran()
  skip_on_os("windows")
  out <- withr::local_tempdir()
  cfg <- pfl_parallel_config(out)

  pid_method <- function(data, params) {
    list(indirect = mean(data$X), worker_pid = Sys.getpid())
  }

  res <- medsim_run(pid_method, list(pfl_scenario()), cfg,
                    parallel = TRUE, verbose = FALSE)

  # Positive control for the whole real-parallel group. Cases 1/6/13 share
  # pfl_parallel_config(), so if any fallback condition creeps back in, this
  # fails loudly instead of the group quietly covering serial code.
  expect_gte(cfg$n_replications, 4L)          # the length(tasks) < 4 floor
  expect_equal(cfg$n_cores, 2L)               # the n_cores == 1 floor
  expect_true(all(res$results$worker_pid != Sys.getpid()))

  # ...and confirm more than one worker actually shared the load.
  expect_gt(length(unique(res$results$worker_pid)), 1L)
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

# ---- medsim_audit_results() reports partial chunks like combine does -------

test_that("medsim_audit_results flags a partial chunk in a recovered combined object", {
  out <- withr::local_tempdir()
  sc <- list(pfl_scenario())
  cfg <- chunk_cfg(out)
  cfg$n_replications <- 8L
  cfg$n_chunks <- 2L
  cfg$chunk_id <- 1L
  medsim_run_chunk(sc, good_method, cfg, verbose = FALSE)
  cfg$chunk_id <- 2L
  # chunk 2 fails on its 3rd rep (global rep 7): it keeps reps 5, 6 and 8
  expect_error(medsim_run_chunk(sc, make_failing_method(), cfg, verbose = FALSE),
               class = "medsim_run_failure")

  combine_warns <- character()
  combined <- withCallingHandlers(
    medsim_combine_chunks(out, on_violation = "warn", verbose = FALSE),
    warning = function(w) {
      combine_warns <<- c(combine_warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  v <- suppressWarnings(medsim_audit_results(combined, on_violation = "ignore"))
  types <- vapply(v, `[[`, character(1), "type")
  expect_true("partial_chunk" %in% types)
  msg <- v[[which(types == "partial_chunk")]]$message
  expect_match(msg, "1 of 2 chunk file(s) are flagged partial", fixed = TRUE)
  expect_match(msg, "hold only their completed reps", fixed = TRUE)
  # same message as combine reported for the same chunks
  expect_true(any(grepl(msg, combine_warns, fixed = TRUE)))
  expect_error(suppressWarnings(medsim_audit_results(combined)),
               "partial_chunk")

  # A bare data.frame has no provenance: the check is skipped, not invented
  v_df <- suppressWarnings(medsim_audit_results(combined$results,
                                                on_violation = "ignore"))
  expect_false("partial_chunk" %in% vapply(v_df, `[[`, character(1), "type"))
})

test_that("partial-chunk detail degrades gracefully for provenance without n_rows", {
  provs <- list(list(partial = FALSE), list(partial = TRUE))
  v <- .medsim_partial_chunk_violation(provs)
  expect_length(v, 1L)
  expect_match(v[[1L]]$message, "hold fewer reps than assigned", fixed = TRUE)
  expect_length(.medsim_partial_chunk_violation(list(list(partial = FALSE))), 0L)
  expect_length(.medsim_partial_chunk_violation(NULL), 0L)
  # with n_rows stamped, an all-failed flagged chunk is described as empty
  v0 <- .medsim_partial_chunk_violation(list(list(partial = TRUE, n_rows = 0L)))
  expect_match(v0[[1L]]$message, "hold no results at all", fixed = TRUE)
})

