# Regression tests for the pre-integration adversarial review's findings
# (2026-07-31): 4 blockers + 3 non-blocking, all CONFIRMED empirically by the
# reviewer before these fixes. Each test reproduces the reviewer's transcript.

.rf_scenario <- function(name = "rf_s1") {
  medsim_scenario(
    name = name,
    data_generator = function(n = 20) data.frame(x = stats::rnorm(n)),
    params = list()
  )
}

.rf_frame <- function(scenarios = "s1", nsim = 40L,
                      estimate = function(sc, ids) rnorm(length(ids))) {
  rows <- do.call(rbind, lapply(scenarios, function(sc) {
    ids <- seq_len(nsim)
    data.frame(scenario = sc, replication = ids, elapsed = 0.001,
               indirect = estimate(sc, ids), error = NA_character_,
               stringsAsFactors = FALSE)
  }))
  attr(rows, "medsim_schema") <- 2L
  attr(rows, "medsim_meta_cols") <- c("scenario", "replication", "elapsed",
                                      "error")
  rows
}

# ---- Blocker 1: A.2 total-collapse blind spot -----------------------------

test_that("R-B1: TOTAL collapse (1 distinct value) now fires — was invisible", {
  df <- .rf_frame(nsim = 1000L, estimate = function(sc, ids) rep(0.5, length(ids)))
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("collapse" %in% vapply(v, `[[`, "", "type"))
  # 2-distinct form too (the reviewer's second reproduction)
  df2 <- .rf_frame(nsim = 1000L,
                   estimate = function(sc, ids) rep_len(c(0.4, 0.6), length(ids)))
  v2 <- suppressWarnings(medsim_audit_results(df2, on_violation = "warn"))
  expect_true("collapse" %in% vapply(v2, `[[`, "", "type"))
})

test_that("R-B1: contract flags stay excluded BY NAME; custom flags via collapse_exclude", {
  df <- .rf_frame()
  df$branch_switch <- rep_len(c(0, 1), nrow(df))
  df$converged <- 1
  expect_silent(medsim_audit_results(df))          # named contract flags
  df$my_flag <- rep_len(c(0, 1), nrow(df))          # unknown discrete field
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("collapse" %in% vapply(v, `[[`, "", "type"))  # flags by default
  expect_silent(                                            # escape hatch
    medsim_audit_results(df, collapse_exclude = c("converged", "branch_switch",
                                                  "my_flag")))
})

# ---- Blocker 2: reserved `error` field name -------------------------------

test_that("R-B2: a method returning an `error` field stops loudly, not corrupts", {
  meth <- function(data, params) list(indirect = mean(data$x), error = 0.05,
                                      converged = TRUE)
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                       n_cores = 1L)
  cfg$n_replications <- 3L
  expect_error(
    suppressWarnings(medsim_run(meth, list(.rf_scenario()), cfg,
                                parallel = FALSE, verbose = FALSE)),
    "reserved"
  )
})

# ---- Blocker 3: mixed legacy+v2 chunk dirs --------------------------------

test_that("R-B3: mixed-schema combine warns + skips id audits in BOTH orderings", {
  meth <- function(data, params) list(indirect = mean(data$x))
  for (legacy_chunk in c(1L, 2L)) {   # legacy first AND legacy last
    out <- withr::local_tempdir()
    for (k in 1:2) {
      cfg <- medsim_config("test", chunk_id = k, n_chunks = 2L,
                           n_replications = 10L, n_cores = 1L,
                           output_dir = out)
      medsim_run_chunk(list(.rf_scenario()), meth, cfg, verbose = FALSE,
                       code_sha = "s")
    }
    # Strip one chunk down to legacy: local ids, no schema attrs.
    f <- file.path(out, sprintf("chunk_%04d.rds", legacy_chunk))
    ch <- readRDS(f)
    attr(ch$results, "medsim_schema") <- NULL
    attr(ch$results, "medsim_meta_cols") <- NULL
    ch$results$replication <- seq_len(nrow(ch$results))  # colliding local ids
    saveRDS(ch, f)

    # Must WARN (legacy skip), must NOT stop with a misdiagnosed dup_rep_id.
    expect_warning(
      cmb <- medsim_combine_chunks(out, verbose = FALSE),
      "schema-v2"
    )
    expect_s3_class(cmb, "medsim_results")
    expect_null(attr(cmb$results, "medsim_schema", exact = TRUE))
  }
})

# ---- Blocker 4: all-failed run --------------------------------------------

test_that("R-B4: an ALL-failed chunked run fires all_failed, not silence", {
  meth_bad <- function(data, params) stop("always fails")
  out <- withr::local_tempdir()
  for (k in 1:2) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = 2L,
                         n_replications = 30L, n_cores = 1L,
                         output_dir = out)
    suppressWarnings(medsim_run_chunk(list(.rf_scenario()), meth_bad, cfg,
                                      verbose = FALSE, code_sha = "s"))
  }
  expect_error(medsim_combine_chunks(out, verbose = FALSE),
               class = "medsim_combine_violation")
  v <- tryCatch(medsim_combine_chunks(out, verbose = FALSE),
                medsim_combine_violation = function(e) e$violations)
  expect_true("all_failed" %in% vapply(v, `[[`, "", "type"))
})

# ---- Non-blocking 5-7 ------------------------------------------------------

test_that("R-F5: pilot n=200L vs n=200 (double) does NOT false-positive", {
  meth <- function(data, params) list(indirect = mean(data$x))
  out <- withr::local_tempdir()
  for (k in 1:2) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = 2L,
                         n_replications = 8L, n_cores = 1L, output_dir = out)
    cfg$n <- 200        # double
    medsim_run_chunk(list(.rf_scenario()), meth, cfg, verbose = FALSE,
                     code_sha = "s")
  }
  pcfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                        n_cores = 1L)
  pcfg$n_replications <- 4L
  pcfg$n <- 200L        # integer
  pilot <- medsim_run(meth, list(.rf_scenario()), pcfg,
                      parallel = FALSE, verbose = FALSE)
  expect_silent(
    medsim_combine_chunks(out, pilot_reference = pilot, verbose = FALSE))
})

test_that("R-F6: NA-vs-value asymmetry against the pilot IS a mismatch", {
  meth <- function(data, params) list(indirect = mean(data$x))
  out <- withr::local_tempdir()
  for (k in 1:2) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = 2L,
                         n_replications = 8L, n_cores = 1L, output_dir = out)
    medsim_run_chunk(list(.rf_scenario()), meth, cfg, verbose = FALSE,
                     code_sha = "s")
  }
  pcfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                        n_cores = 1L)
  pcfg$n_replications <- 4L
  pilot <- medsim_run(meth, list(.rf_scenario()), pcfg,
                      parallel = FALSE, verbose = FALSE)
  pilot$results$indirect[2L] <- NA_real_   # failed in pilot, value in full run
  v <- tryCatch(
    medsim_combine_chunks(out, pilot_reference = pilot, verbose = FALSE),
    medsim_combine_violation = function(e) e$violations)
  expect_true("pilot_mismatch" %in% vapply(v, `[[`, "", "type"))
})

test_that("R-F7: all-provenance-less chunks warn about the skipped SHA assertion", {
  meth <- function(data, params) list(indirect = mean(data$x))
  out <- withr::local_tempdir()
  for (k in 1:2) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = 2L,
                         n_replications = 10L, n_cores = 1L, output_dir = out)
    medsim_run_chunk(list(.rf_scenario()), meth, cfg, verbose = FALSE)
  }
  for (k in 1:2) {
    f <- file.path(out, sprintf("chunk_%04d.rds", k))
    ch <- readRDS(f); attr(ch, "provenance") <- NULL; saveRDS(ch, f)
  }
  expect_warning(medsim_combine_chunks(out, verbose = FALSE),
                 "no chunk carries a code SHA")
})

# ---- Nit: template creates logs/ ------------------------------------------

test_that("R-F9a: template mkdirs logs/ before SLURM needs it", {
  cfg <- medsim_config("cluster", n_chunks = 4L, output_dir = tempfile())
  out <- tempfile(fileext = ".sh")
  medsim_write_submit_script(cfg, output_file = out)
  expect_true(any(grepl("^mkdir -p logs$", readLines(out))))
})
