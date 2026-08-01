# Gate A of SPEC-medsim-chunked-run-gates-2026-07-31 (#34): combine-step
# seed-provenance audit. Each test is a planted defect (or its matched
# negative control) from the spec's acceptance matrix.

.audit_frame <- function(scenarios = "s1", nsim = 40L,
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

# ---- A.1 contiguity -------------------------------------------------------

test_that("A.1: clean contiguous grid is silent", {
  df <- .audit_frame(c("s1", "s2"))
  expect_silent(v <- medsim_audit_results(df, on_violation = "stop"))
  expect_length(v, 0L)
})

test_that("A.1: duplicated replication id fires dup_rep_id", {
  df <- .audit_frame()
  df$replication[2L] <- 1L  # two chunks claimed rep 1; rep 2 now also a gap
  expect_error(medsim_audit_results(df), class = "medsim_combine_violation")
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("dup_rep_id" %in% vapply(v, `[[`, "", "type"))
})

test_that("A.1: a gap in replication ids fires rep_gap", {
  df <- .audit_frame()
  df <- df[df$replication != 17L, ]  # deleted-chunk signature
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("rep_gap" %in% vapply(v, `[[`, "", "type"))
})

test_that("A.1: scenarios with different totals fire ragged_cells", {
  df <- .audit_frame(c("s1", "s2"))
  df <- df[!(df$scenario == "s2" & df$replication > 30L), ]
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  types <- vapply(v, `[[`, "", "type")
  expect_true("ragged_cells" %in% types)
  expect_false("rep_gap" %in% types)  # 1..30 is contiguous -- not a gap
})

test_that("A.1: explicit nsim pin fires on mismatch, silent on match", {
  df <- .audit_frame(nsim = 40L)
  expect_silent(medsim_audit_results(df, nsim = 40L))
  v <- suppressWarnings(
    medsim_audit_results(df, nsim = 50L, on_violation = "warn"))
  expect_true("nsim_mismatch" %in% vapply(v, `[[`, "", "type"))
})

# ---- A.2 collapse signature ----------------------------------------------

test_that("A.2: the 0.3.1 collapse signature fires on a continuous column", {
  # ~17 distinct values recycled over 1000 reps -- the exact historical bug.
  df <- .audit_frame(nsim = 1000L,
                     estimate = function(sc, ids) rep_len(rnorm(17L), length(ids)))
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("collapse" %in% vapply(v, `[[`, "", "type"))
})

test_that("A.2: discrete 0/1 contract fields do NOT fire (negative control)", {
  df <- .audit_frame()
  df$branch_switch <- rep_len(c(0, 1), nrow(df))   # 2 distinct values only
  df$converged <- 1                                # 1 distinct value
  expect_silent(medsim_audit_results(df))
})

test_that("A.2: small cells are skipped (too noisy to diagnose)", {
  df <- .audit_frame(nsim = 20L,  # < collapse_min_cell = 30
                     estimate = function(sc, ids) rep_len(rnorm(2L), length(ids)))
  expect_silent(medsim_audit_results(df))
})

test_that("A.2: an all-failed cell is cell_failed, never collapse", {
  df <- .audit_frame()
  df$indirect <- NA_real_
  df$error <- "boom"
  # indirect stays a continuous column via a second healthy scenario.
  df2 <- .audit_frame(scenarios = "s2")
  both <- rbind(df, df2)
  attr(both, "medsim_schema") <- 2L
  attr(both, "medsim_meta_cols") <- attr(df, "medsim_meta_cols", exact = TRUE)
  v <- suppressWarnings(medsim_audit_results(both, on_violation = "warn"))
  types <- vapply(v, `[[`, "", "type")
  expect_true("cell_failed" %in% types)
  expect_false("collapse" %in% types)
})

test_that("A.2: thresholds are configurable", {
  df <- .audit_frame(nsim = 100L,
                     estimate = function(sc, ids) rep_len(rnorm(80L), length(ids)))
  # 80/100 distinct: fails at threshold 0.9, passes at 0.5.
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("collapse" %in% vapply(v, `[[`, "", "type"))
  expect_silent(medsim_audit_results(df, collapse_threshold = 0.5))
})

# ---- A.3 cross-scenario seed collisions -----------------------------------

test_that("A.3: two scenario names in the same hash bucket fire seed_collision", {
  # Brute-force a genuine .medsim_det_seed collision (same base for rep 0).
  base_of <- function(nm) .medsim_det_seed(nm, 0L)
  target <- base_of("s1")
  collider <- NULL
  for (i in seq_len(200000L)) {
    cand <- paste0("c", i)
    if (cand != "s1" && identical(base_of(cand), target)) {
      collider <- cand
      break
    }
  }
  skip_if(is.null(collider), "no hash collision found in search budget")
  df <- .audit_frame(c("s1", collider))
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("seed_collision" %in% vapply(v, `[[`, "", "type"))
})

test_that("A.3: distinct buckets are silent (negative control)", {
  df <- .audit_frame(c("scenario_a", "scenario_b", "scenario_c"))
  expect_silent(medsim_audit_results(df))
})

# ---- legacy + condition contract ------------------------------------------

test_that("legacy schema-absent frames warn + skip A.1/A.3, never stop", {
  df <- .audit_frame()
  attr(df, "medsim_schema") <- NULL
  df$replication <- rep_len(1:10, nrow(df))  # colliding local ids (legacy)
  expect_warning(
    v <- medsim_audit_results(df, on_violation = "stop"),
    "schema-v2"
  )
  expect_false("dup_rep_id" %in% vapply(v, `[[`, "", "type"))
})

test_that("stop condition carries the audited object", {
  df <- .audit_frame()
  df <- df[df$replication != 5L, ]
  recovered <- tryCatch(
    medsim_audit_results(df),
    medsim_combine_violation = function(e) list(res = e$results,
                                                v = e$violations))
  expect_identical(nrow(recovered$res), nrow(df))
  expect_true(length(recovered$v) >= 1L)
})

test_that("on_violation = 'ignore' is fully silent", {
  df <- .audit_frame()
  df <- df[df$replication != 5L, ]
  expect_silent(medsim_audit_results(df, on_violation = "ignore"))
})

# ---- end-to-end through medsim_combine_chunks -----------------------------

test_that("combine runs the audit end-to-end: planted gap stops, clean run passes", {
  sc <- medsim_scenario(
    name = "audit_e2e",
    data_generator = function(n = 30) data.frame(x = stats::rnorm(n)),
    params = list()
  )
  meth <- function(data, params) list(indirect = mean(data$x))
  out <- withr::local_tempdir()
  for (k in 1:4) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = 4L,
                         n_replications = 40L, n_cores = 1L, output_dir = out)
    medsim_run_chunk(list(sc), meth, cfg, verbose = FALSE)
  }
  # Clean: silent under the default stop posture.
  cmb <- medsim_combine_chunks(out, verbose = FALSE)
  expect_s3_class(cmb, "medsim_results")
  # Planted defect: delete a middle chunk -> rep gap -> data-carrying stop.
  file.remove(file.path(out, "chunk_0003.rds"))
  expect_error(medsim_combine_chunks(out, verbose = FALSE),
               class = "medsim_combine_violation")
})
