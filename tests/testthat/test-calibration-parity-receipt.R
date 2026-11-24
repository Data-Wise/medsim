test_that("the finalizer requires complete, consistent shared-input parity evidence", {
  directory <- system.file("mbco-calibration", package = "medsim", mustWork = TRUE)
  driver <- new.env(parent = baseenv())
  driver$commandArgs <- function(...) paste0("--file=", file.path(directory, "run.R"))
  sys.source(file.path(directory, "run.R"), envir = driver)
  root <- withr::local_tempdir()
  name <- if (.Platform$OS.type == "windows") "hash input" else "hash \\ input"
  input <- file.path(root, name)
  writeBin(as.raw(c(0x61, 0x0d, 0x0a, 0x1a, 0x00, 0xff)), input)
  expected <- "5f3a00835fec1ac5375bdc710a18b3008d4fcf3d7799146f7e8737442d4cb971"
  expect_identical(driver$sha256(input), expected)

  pin <- list(medsim_sha = "medsim", medsim_code = "code", checker = "checker",
              harness = "harness", r = "R version", rng = c("kind", "normal", "sample"),
              packages = c(medsim = "0.6.0", mice = "3.0.0"))
  values <- c(D4 = 1, p = .2, r4 = 0, nu = Inf, stacked_branch = 1)
  recorded <- function(mode) {
    list(medsim = values, medsim_branch_mix = 0L,
         missingmed = structure(values, ariv = mode, stacked_branch = "a", branch_mix = 0L, k = 1L))
  }
  pair <- list(own = recorded("own"), fixed = recorded("fixed"))
  receipt <- list(passed = TRUE, medsim_sha = pin$medsim_sha, medsim_code = pin$medsim_code,
                  checker_hash = pin$checker, harness_hash = pin$harness,
                  missingmed_sha = driver$src$.calibration_missingmed_sha,
                  fixture_blob = driver$src$.calibration_fixture_blob,
                  fixture_sha256 = driver$parity_fixture_sha,
                  missingmed_code = strrep("0", 64L), command = "parity",
                  r = pin$r, rng = pin$rng, packages = c(pin$packages, missingmed = "0.1.0"),
                  comparisons = list(seed26 = pair, seed9 = pair, seed28 = pair))
  expect_true(driver$validate_parity(receipt, pin))

  driver$read_plan <- function(root) list(runtime = pin)
  parity <- file.path(root, "parity.rds")
  expect_rejected <- function(x) {
    saveRDS(x, parity)
    expect_error(driver$finalize(root, parity), "complete, passing shared-input parity")
    expect_false(file.exists(file.path(root, "decision.rds")))
  }
  expect_rejected(receipt[1:7])
  expect_rejected(within(receipt, rng <- "wrong"))
  expect_rejected(within(receipt, packages["mice"] <- "wrong"))
  expect_rejected(within(receipt, comparisons$seed9$fixed <- NULL))
  expect_rejected(within(receipt, comparisons$seed28$own$medsim["p"] <- .01))
  expect_rejected(within(receipt, comparisons$seed26$fixed$medsim_branch_mix <- 1L))
  expect_rejected(within(receipt, comparisons$seed26$fixed <- comparisons$seed26$own))
})
