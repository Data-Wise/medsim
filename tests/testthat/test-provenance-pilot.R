# Gates C (provenance/SHA) and D (pilot-subset positive control) of
# SPEC-medsim-chunked-run-gates-2026-07-31 (#34). Planted defects + matched
# negative controls per the spec's acceptance matrix.

.cd_scenario <- function(mu = 0) {
  force(mu)
  medsim_scenario(
    name = "cd_s1",
    data_generator = function(n = 25) data.frame(x = stats::rnorm(n, mu)),
    params = list(mu = mu)
  )
}

.cd_method <- function(data, params) list(indirect = mean(data$x))

.cd_run_chunks <- function(out, nsim = 20L, n_chunks = 4L, sc = .cd_scenario(),
                           code_sha = NULL) {
  for (k in seq_len(n_chunks)) {
    cfg <- medsim_config("test", chunk_id = k, n_chunks = n_chunks,
                         n_replications = nsim, n_cores = 1L,
                         output_dir = out)
    medsim_run_chunk(list(sc), .cd_method, cfg, verbose = FALSE,
                     code_sha = code_sha)
  }
  out
}

# ---- Gate C: provenance ---------------------------------------------------

test_that("C: chunk files carry a full provenance header", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, n_chunks = 2L)
  ch <- readRDS(file.path(out, "chunk_0001.rds"))
  p <- attr(ch, "provenance", exact = TRUE)
  expect_type(p, "list")
  expect_identical(p$r_version, as.character(getRversion()))
  expect_identical(p$medsim_version,
                   as.character(utils::packageVersion("medsim")))
  expect_true(nzchar(p$hostname))
  expect_true(is.numeric(p$sec_per_rep) && p$sec_per_rep >= 0)
  # Auto-detected SHA: 40-hex in a git tree, pkg: tag outside one.
  expect_match(p$code_sha, "^([0-9a-f]{40}|pkg:medsim-.+)$")
})

test_that("C: explicit code_sha overrides auto-detection", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, n_chunks = 2L, code_sha = "deadbeefcafe")
  ch <- readRDS(file.path(out, "chunk_0002.rds"))
  expect_identical(attr(ch, "provenance", exact = TRUE)$code_sha,
                   "deadbeefcafe")
})

test_that("C: mixed SHAs across chunks fire sha_mismatch (planted defect)", {
  # A mid-run code edit + partial resubmit: chunk 2 rewritten under new code.
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "sha_state_AAAA")
  ch2 <- readRDS(file.path(out, "chunk_0002.rds"))
  prov <- attr(ch2, "provenance", exact = TRUE)
  prov$code_sha <- "sha_state_BBBB"
  attr(ch2, "provenance") <- prov
  saveRDS(ch2, file.path(out, "chunk_0002.rds"))

  expect_error(medsim_combine_chunks(out, verbose = FALSE),
               class = "medsim_combine_violation")
  v <- tryCatch(medsim_combine_chunks(out, verbose = FALSE),
                medsim_combine_violation = function(e) e$violations)
  expect_true("sha_mismatch" %in% vapply(v, `[[`, "", "type"))
})

test_that("C: single SHA across chunks is silent (negative control)", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "sha_state_AAAA")
  expect_silent(cmb <- medsim_combine_chunks(out, verbose = FALSE))
  expect_length(cmb$chunk_provenance, 4L)
})

test_that("C: provenance-less legacy chunks warn, never stop", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out)
  ch1 <- readRDS(file.path(out, "chunk_0001.rds"))
  attr(ch1, "provenance") <- NULL
  saveRDS(ch1, file.path(out, "chunk_0001.rds"))
  expect_warning(cmb <- medsim_combine_chunks(out, verbose = FALSE),
                 "lack provenance")
  expect_s3_class(cmb, "medsim_results")
})

# ---- Gate D: pilot-subset positive control --------------------------------

.cd_pilot <- function(nsim = 8L, sc = .cd_scenario()) {
  # A standalone pilot: reps 1..nsim of the same scenario at the same n --
  # draw-identical to the full run's first nsim reps by the seeding contract.
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                       n_cores = 1L)
  cfg$n_replications <- nsim
  medsim_run(.cd_method, list(sc), cfg, parallel = FALSE, verbose = FALSE)
}

test_that("D: pilot-identical full run passes (elapsed differing must NOT fire)", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "s")
  pilot <- .cd_pilot()
  # elapsed always differs between runs; only estimate columns are compared.
  expect_silent(
    cmb <- medsim_combine_chunks(out, pilot_reference = pilot,
                                 verbose = FALSE))
  expect_s3_class(cmb, "medsim_results")
})

test_that("D: a perturbed estimate fires pilot_mismatch (planted defect)", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "s")
  pilot <- .cd_pilot()
  pilot$results$indirect[3L] <- pilot$results$indirect[3L] + 1e-3
  v <- tryCatch(
    medsim_combine_chunks(out, pilot_reference = pilot, verbose = FALSE),
    medsim_combine_violation = function(e) e$violations)
  expect_true("pilot_mismatch" %in% vapply(v, `[[`, "", "type"))
})

test_that("D: pilot at a different n fails as pilot_config_differs, not mismatch", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "s")
  pilot <- .cd_pilot()
  pilot$config$n <- 64000L  # full run's config has no n set -> differs
  v <- tryCatch(
    medsim_combine_chunks(out, pilot_reference = pilot, verbose = FALSE),
    medsim_combine_violation = function(e) e$violations)
  types <- vapply(v, `[[`, "", "type")
  expect_true("pilot_config_differs" %in% types)
  expect_false("pilot_mismatch" %in% types)  # identity gate short-circuits
})

test_that("D: a changed generator fails the fingerprint identity gate", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "s")
  pilot <- .cd_pilot(sc = .cd_scenario(mu = 5))  # same name, different DGM
  v <- tryCatch(
    medsim_combine_chunks(out, pilot_reference = pilot, verbose = FALSE),
    medsim_combine_violation = function(e) e$violations)
  expect_true("pilot_config_differs" %in% vapply(v, `[[`, "", "type"))
})

test_that("D: pilot_tol is honored (loose tol accepts the perturbation)", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "s")
  pilot <- .cd_pilot()
  pilot$results$indirect[3L] <- pilot$results$indirect[3L] + 1e-3
  expect_silent(
    medsim_combine_chunks(out, pilot_reference = pilot, pilot_tol = 1e-2,
                          verbose = FALSE))
})

test_that("D: pilot_reference accepts a file path", {
  out <- withr::local_tempdir()
  .cd_run_chunks(out, code_sha = "s")
  pilot <- .cd_pilot()
  pfile <- tempfile(fileext = ".rds")
  saveRDS(pilot, pfile)
  expect_silent(
    medsim_combine_chunks(out, pilot_reference = pfile, verbose = FALSE))
})
