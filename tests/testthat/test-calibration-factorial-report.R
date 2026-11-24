.factorial_report <- local({
  report <- new.env(parent = baseenv())
  directory <- system.file("mbco-calibration", package = "medsim", mustWork = TRUE)
  sys.source(file.path(directory, "report.R"), envir = report)
  report$driver <- report$report_driver(directory)
  report
})

.factorial_rows <- function() {
  data.frame(
    scenario = "fixture", replication = 1:5,
    own_li = c(.01, .05, .2, .01, .01),
    own_chan_meng = c(.2, .01, .01, .01, .01),
    fixed_li = c(.2, .01, .01, .2, .01),
    fixed_chan_meng = c(.01, .2, NA, .01, .01),
    complete_by_design = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    effective_k = c(20L, 20L, 20L, 19L, 1L)
  )
}

test_that("factorial contrasts use the same valid dataset-level pairs", {
  report <- .factorial_report
  rows <- .factorial_rows()
  counts <- report$factorial_counts(rows, report$driver$src)
  actual <- report$factorial_summary(counts)
  expected <- cbind(c(-1, 1, 0), c(1, -1, 0), c(-1, 1, 0),
                    c(1, -1, 0), c(2, -2, 0))
  expect_equal(actual$estimate, colMeans(expected))
  expect_equal(actual$mcse, apply(expected, 2L, stats::sd) / sqrt(3))
  expect_equal(actual$comparison_usable, c(4, 3, 4, 3, 3))
  expect_equal(actual$usable_four, rep(3, 5))
  expect_equal(unlist(actual[5, c("minus_two", "zero", "plus_two")], use.names = FALSE),
               c(1, 1, 1))
})

test_that("estimates and MCSE distinguish zero and one four-arm usable draw", {
  report <- .factorial_report
  rows <- .factorial_rows()
  summarize <- function(i) {
    report$factorial_summary(report$factorial_counts(rows[i, ], report$driver$src))
  }
  expect_true(all(is.na(summarize(3:4)$estimate)))
  expect_true(all(is.na(summarize(3:4)$mcse)))
  expect_equal(summarize(1)$estimate, c(-1, 1, -1, 1, 2))
  expect_true(all(is.na(summarize(1)$mcse)))
})

test_that("the report verifies relocated selected payloads before publication", {
  report <- .factorial_report
  driver <- report$driver
  root <- withr::local_tempdir()
  group <- "100-power"
  chunk_dir <- file.path(root, "attempts", group, "1", "1")
  dir.create(chunk_dir, recursive = TRUE)
  path <- file.path(chunk_dir, "chunk_0001.rds")
  outfile <- file.path(root, "factorial.csv")
  plan <- list(
    grid = data.frame(scenario = "fixture", group = group, attempts = 5,
                      is_null = FALSE, n = 100L),
    groups = data.frame(group = group, chunks = 1, attempts = 5),
    runtime = list(medsim_sha = strrep("a", 40L))
  )
  manifest <- file.path(root, "manifest.rds")
  decision <- file.path(root, "decision.rds")
  saveRDS(plan, manifest)
  rows <- .factorial_rows()
  rows$branch_mix <- 0L
  rows$realized_missing <- 0
  rows$failure_reason <- NA_character_
  rows$error <- NA_character_
  summary <- driver$src$calibration_summarize(rows, plan$grid)
  prepare <- function(rows, partial = FALSE) {
    payload <- structure(list(results = rows),
      provenance = list(code_sha = plan$runtime$medsim_sha, partial = partial))
    saveRDS(payload, path)
    start <- list(manifest = driver$sha256(manifest), group = group,
                  chunk = 1L, attempt = 1L)
    hash <- driver$sha256(path)
    saveRDS(start, file.path(chunk_dir, "started.rds"))
    saveRDS(c(start, list(status = "complete", output_hash = hash)),
            file.path(chunk_dir, "receipt.rds"))
    selected <- stats::setNames(hash, paste0("/original/attempts/", group, "/1/1/chunk_0001.rds"))
    saveRDS(list(manifest_hash = start$manifest, chunks = stats::setNames(list(selected), group),
                 cells = list(fixture = summary)), decision)
    driver$sha256(decision)
  }
  hash <- prepare(transform(rows, replication = c(1L, 1L, 3:5)))
  expect_error(report$report_run(root, outfile, hash, driver), "replicate keys")
  expect_false(file.exists(outfile))
  hash <- prepare(rows, partial = TRUE)
  expect_error(report$report_run(root, outfile, hash, driver), "provenance")
  expect_false(file.exists(outfile))
  hash <- prepare(rows)
  cat("altered", file = path, append = TRUE)
  expect_error(report$report_run(root, outfile, hash, driver), "payload hash")
  expect_false(file.exists(outfile))
  hash <- prepare(rows)
  expect_error(report$report_run(root, outfile, strrep("0", 64L), driver), "Decision.*hash")
  expect_silent(report$report_run(root, outfile, hash, driver))
  result <- utils::read.csv(outfile)
  expect_equal(nrow(result), 5)
  expect_true(all(is.na(result$u_paired)))
  expect_true(all(result$decision_sha256 == hash))
  expect_error(report$report_run(root, outfile, hash, driver), "already exists")
})
