#!/usr/bin/env Rscript

check_script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))
driver <- new.env(parent = baseenv())
sys.source(file.path(dirname(check_script), "run.R"), envir = driver)
h <- driver$src

fixture <- function(path) {
  blob <- system2("git", c("hash-object", shQuote(path)), stdout = TRUE)
  stopifnot(identical(blob, h$.calibration_fixture_blob))
  readRDS(path)$cases
}

smoke <- function() {
  grid <- h$calibration_grid()
  stopifnot(
    nrow(grid) == 72L, sum(grid$is_null) == 54L,
    sum(grid$attempts) == 5580000, !anyDuplicated(grid$scenario),
    identical(sort(as.integer(table(grid$group))), rep(c(6L, 18L), each = 3L)),
    h$calibration_check_seeds(grid)
  )

  dir <- tempfile("medsim62-smoke-")
  on.exit(unlink(dir, recursive = TRUE))
  cell <- grid[grid$scenario == "mbco62_100-null_a_null_30_skewed", ]
  cell$attempts <- 2L
  settings <- medsim::medsim_config("test",
    n = cell$n, n_replications = cell$attempts,
    n_chunks = 1L, chunk_id = 1L, n_cores = 1L,
    output_dir = dir
  )
  medsim::medsim_run_chunk(h$calibration_scenarios(cell), h$calibration_fit, settings)
  rows <- medsim::medsim_combine_chunks(dir, expected_chunks = 1L, nsim = 2L)$results
  columns <- c(
    "scenario", "replication", "indirect", "indirect_ci_lower",
    "indirect_ci_upper", "indirect_p", "branch_switch", "converged",
    "indirect_p_fixed", "branch_mix", "p_branch_a", "stacked_branch",
    "r4", "r4_fixed", h$.calibration_arms, "effective_k",
    "complete_by_design", "realized_missing", "failure_reason"
  )
  stopifnot(
    nrow(rows) == 2L, all(columns %in% names(rows)),
    identical(rows$indirect_p, rows$fixed_chan_meng),
    identical(rows$indirect_p_fixed, rows$fixed_chan_meng),
    all(vapply(rows[h$.calibration_arms], function(p) {
      all(h$calibration_valid_p(p))
    }, logical(1))),
    all(rows$effective_k == 20L), all(rows$realized_missing > 0),
    all(is.na(rows$failure_reason))
  )
  h$calibration_summarize(rows, cell)

  # Exercise failure coding with copies of the same two rows.
  fallback <- rows
  fallback$effective_k[1L] <- 1L
  fallback$failure_reason[1L] <- "imputation_fallback"
  fallback$fixed_chan_meng[2L] <- Inf
  fallback$own_li[2L] <- .04
  result <- h$calibration_summarize(fallback, cell)
  stopifnot(
    result$decision[["new"]] == 2L, result$decision[["new_only"]] == 1L,
    result$decision[["legacy_only"]] == 0L,
    result$decision[["complete_pairs"]] == 0L,
    identical(result$arms$unusable, c(1L, 1L, 1L, 2L)),
    sum(result$unusable_reasons$Freq) == 5L
  )
  complete <- rows
  complete$complete_by_design[] <- TRUE
  complete$effective_k[] <- 1L
  complete[h$.calibration_arms] <- .01
  decision <- h$calibration_summarize(complete, cell)$decision
  stopifnot(
    decision[["new"]] == 2L, decision[["new_only"]] == 0L,
    decision[["legacy_only"]] == 0L, decision[["branch_mix_denominator"]] == 0L
  )
  stopifnot(
    h$calibration_limits(0, 0, 0, 100000)[["passed"]] == 1,
    h$calibration_limits(100000, 100000, 0, 100000)[["u_size"]] == 1,
    h$calibration_limits(6234, 424, 0, 100000)[["passed"]] == 1,
    h$calibration_limits(6235, 424, 0, 100000)[["passed"]] == 0,
    h$calibration_limits(6234, 425, 0, 100000)[["passed"]] == 0,
    !h$calibration_pass(.005, .064), !h$calibration_pass(.004, .065)
  )

  # Reuse the real smoke rows and simulate only receipt and file states.
  obj <- readRDS(file.path(dir, "chunk_0001.rds"))
  provenance <- attr(obj, "provenance")
  provenance$code_sha <- "smoke"
  plan <- list(hash = "smoke", runtime = list(medsim_sha = "smoke"))
  emit <- function(attempt, rows, partial = FALSE, root = dir, group = cell$group,
                   manifest = plan$hash) {
    out <- file.path(root, "attempts", group, "1", attempt)
    driver$new_directory(out)
    start <- list(
      manifest = manifest, group = group, chunk = 1L,
      attempt = as.integer(attempt)
    )
    driver$save_atomic(start, file.path(out, "started.rds"))
    obj$results <- rows
    provenance$partial <- partial
    attr(obj, "provenance") <- provenance
    path <- file.path(out, "chunk_0001.rds")
    driver$save_atomic(obj, path)
    driver$save_atomic(
      c(start, list(
        status = if (partial) "error" else "complete",
        output_hash = driver$sha256(path)
      )),
      file.path(out, "receipt.rds")
    )
    path
  }
  audit <- function() driver$read_attempts(dir, plan, cell$group, 1L, cell$scenario, 1:2)
  testthat::expect_error(audit(), "No complete audited attempt")
  reference <- emit(1L, rows)
  partial <- rows[1L, ]
  partial$elapsed <- partial$elapsed + 1
  emit(2L, partial, partial = TRUE)
  stopifnot(identical(audit(), reference))
  duplicate <- emit(3L, rows[c(1L, 1L), ])
  testthat::expect_error(audit(), "Invalid attempt keys")
  unlink(dirname(duplicate), recursive = TRUE)
  partial$fixed_chan_meng <- 1 - partial$fixed_chan_meng
  mismatch <- emit(4L, partial, partial = TRUE)
  testthat::expect_error(audit(), "Retry disagreement in fixed_chan_meng")
  unlink(dirname(mismatch), recursive = TRUE)
  partial$error <- "data_generator error: smoke"
  emit(5L, partial, partial = TRUE)
  testthat::expect_error(audit(), "DGP failure")

  # Exercise the full six-group finalizer using the smoke rows as synthetic files.
  # The reduced grid and fake provenance are confined to this local check.
  reduced <- grid[!duplicated(grid$group), ]
  reduced$attempts <- reduced$last_id <- 2L
  pin <- list(medsim_sha = "smoke", medsim_code = "smoke", harness = "smoke", checker = "smoke")
  driver$runtime <- function(sha) pin
  driver$src$calibration_grid <- function() reduced
  root <- file.path(dir, "mock-release")
  driver$new_directory(root)
  manifest <- file.path(root, "manifest.rds")
  groups <- driver$combine_groups(reduced, "1", "1")
  driver$save_atomic(list(grid = reduced, groups = groups, runtime = pin), manifest)
  hash <- driver$sha256(manifest)
  parity <- file.path(root, "parity.rds")
  driver$save_atomic(list(
    passed = TRUE, medsim_sha = "smoke", medsim_code = "smoke",
    checker_hash = "smoke", harness_hash = "smoke",
    missingmed_sha = h$.calibration_missingmed_sha,
    fixture_blob = h$.calibration_fixture_blob
  ), parity)
  for (i in seq_len(nrow(reduced))) {
    synthetic <- rows
    synthetic$scenario <- reduced$scenario[i]
    emit(1L, synthetic, root = root, group = reduced$group[i], manifest = hash)
  }
  wrong <- emit(2L, rows, root = root, group = tail(reduced$group, 1L), manifest = hash)
  testthat::expect_error(driver$finalize(root, parity), "Invalid attempt keys")
  stopifnot(!file.exists(file.path(root, "decision.rds")))
  unlink(dirname(wrong), recursive = TRUE)
  stopifnot(identical(capture.output(driver$finalize(root, parity)), "HOLD"))
  report <- readRDS(file.path(root, "decision.rds"))
  stopifnot(
    isFALSE(report$passed), identical(names(report$cells), reduced$scenario),
    identical(names(report$chunks), groups$group), identical(report$manifest_hash, hash)
  )
  testthat::expect_error(driver$finalize(root, parity), "already has a final decision")
  cat("PASS smoke: 2 replicates, stock combine, 6 groups, 5580000 seeds, retry and gate checks\n")
}

adapter <- function(path) {
  case <- fixture(path)$seed9
  complete <- case$implist[[1L]]
  data <- complete
  data$M[1L] <- NA_real_
  model <- "M ~ a*X + C1\nY ~ b*M + cp*X + C1\nab := a*b"
  compare <- function(x, y) stopifnot(identical(x, y))
  set.seed(62)
  ordinary <- medsim::medsim_method_mbco_mi(model)(complete, list())
  set.seed(62)
  cc <- h$calibration_fit(complete, list())
  stopifnot(cc$complete_by_design, cc$effective_k == 1L)
  compare(unlist(cc[h$.calibration_arms], use.names = FALSE), rep(ordinary$indirect_p, 4L))

  implist <- rep(case$implist, 4L)
  run <- function(implist) {
    testthat::local_mocked_bindings(
      .medsim_md_implist = function(...) implist,
      .package = "medsim"
    )
    set.seed(62)
    h$calibration_fit(data, list())
  }
  fallback <- run(list(complete))
  stopifnot(
    !fallback$complete_by_design, fallback$effective_k == 1L,
    fallback$failure_reason == "imputation_fallback",
    all(is.na(unlist(fallback[h$.calibration_arms])))
  )

  testthat::local_mocked_bindings(
    .medsim_md_implist = function(...) implist,
    .package = "medsim"
  )
  set.seed(62)
  paired <- h$calibration_fit(data, list())
  stopifnot(paired$branch_mix == 1L, paired$effective_k == 20L)
  covs <- medsim:::.medsim_md_covariates(data)
  fit <- medsim:::.medsim_mbco_mi_fit(implist, covs, h$.calibration_df)
  check_arms <- function(out) {
    for (df in h$.calibration_df) {
      for (ariv in c("own", "fixed")) {
        set.seed(62)
        one <- medsim::medsim_method_mbco_mi(model, ariv = ariv, df = df)(data, list())
        arm <- paste(ariv, df, sep = "_")
        if (!identical(out[[arm]], one$indirect_p)) stop("Adapter p-value differs for ", arm)
        compare(out[[paste0("fixed_", df)]], one$indirect_p_fixed)
        fields <- c(
          names(fit$effect), "branch_switch", "branch_mix", "p_branch_a",
          "stacked_branch", "r4", "r4_fixed"
        )
        compare(out[fields], one[fields])
        compare(
          fit$tests$li[[ariv]][c("D4", "r4")],
          fit$tests$chan_meng[[ariv]][c("D4", "r4")]
        )
      }
    }
  }
  stopifnot(
    paired$own_li != paired$fixed_li, paired$own_chan_meng != paired$fixed_chan_meng,
    paired$own_li != paired$own_chan_meng, paired$fixed_li != paired$fixed_chan_meng
  )
  check_arms(paired)
  broken <- paired
  broken[c("own_li", "own_chan_meng")] <- broken[c("fixed_li", "fixed_chan_meng")]
  testthat::expect_error(check_arms(broken), "Adapter p-value differs for own_li")
  cat("PASS adapter: 4 arms, shared estimates and diagnostics, complete data, fallback, planted ARIV defect\n")
}

parity <- function(path, receipt_path = NULL) {
  cases <- fixture(path)
  medsim_sha <- utils::packageDescription("medsim")$RemoteSha
  missingmed_sha <- utils::packageDescription("missingmed")$RemoteSha
  stopifnot(identical(missingmed_sha, h$.calibration_missingmed_sha), length(cases) == 3L)
  disagreements <- character()
  check <- function(ok, name, ariv, field, x, y) {
    if (!isTRUE(ok)) {
      disagreements <<- c(disagreements, paste(
        name, ariv, field,
        format(x, digits = 17), format(y, digits = 17)
      ))
    }
  }
  comparisons <- list()
  for (name in names(cases)) {
    case <- cases[[name]]
    covs <- setdiff(all.vars(case$formula_m), c("M", "X"))
    stopifnot(
      identical(all.vars(case$formula_m), c("M", "X", covs)),
      identical(all.vars(case$formula_y), c("Y", "X", "M", covs))
    )
    local <- medsim:::.medsim_mbco_mi_fit(case$implist, covs, "chan_meng")
    comparisons[[name]] <- list()
    for (ariv in c("own", "fixed")) {
      x <- local$tests$chan_meng[[ariv]]
      y <- missingmed::mbco_d4(
        case$implist, case$formula_y, case$formula_m,
        family_y = stats::gaussian(),
        family_m = stats::gaussian(), treatment = "X", mediator = "M", ariv = ariv
      )
      for (field in c("D4", "r4", "nu", "p")) {
        a <- x[[field]]
        b <- y[[field]]
        agree <- if (is.finite(a) && is.finite(b)) {
          abs(a - b) <= 1e-10 * max(1, abs(a), abs(b))
        } else {
          field == "nu" && identical(a, Inf) && identical(b, Inf)
        }
        check(agree, name, ariv, field, a, b)
      }
      branch <- if (local$stacked_branch == 1) "a" else "b"
      check(branch == y@stacked_branch, name, ariv, "stacked branch", branch, y@stacked_branch)
      check(
        local$branch_mix == y@branch_mix, name, ariv, "branch mix", local$branch_mix,
        y@branch_mix
      )
      # medsim's linear D4 call has one constraint on either union-null branch.
      check(y@k == 1L, name, ariv, "dimension", 1L, y@k)
      check((x[["p"]] < .05) == (y[["p"]] < .05), name, ariv, "reject", x[["p"]], y[["p"]])
      comparisons[[name]][[ariv]] <- list(medsim = x, missingmed = y)
    }
  }
  if (length(disagreements)) {
    stop("Parity discrepancies (case, ARIV, field, medsim, missingmed):\n",
      paste(disagreements, collapse = "\n"),
      call. = FALSE
    )
  }
  if (!is.null(receipt_path)) {
    pin <- driver$runtime(medsim_sha)
    if (file.exists(receipt_path)) stop("Parity receipt already exists.")
    driver$save_atomic(list(
      passed = TRUE, medsim_sha = medsim_sha, medsim_code = pin$medsim_code,
      harness_hash = pin$harness, checker_hash = pin$checker,
      missingmed_sha = missingmed_sha, fixture_blob = h$.calibration_fixture_blob,
      fixture_sha256 = driver$sha256(path), r = R.version.string,
      packages = driver$dependency_versions(c("medsim", "missingmed")),
      missingmed_code = driver$sha256(system.file("R", "missingmed.rdb", package = "missingmed")),
      rng = RNGkind(), command = commandArgs(), comparisons = comparisons
    ), receipt_path)
  }
  cat("PASS missingmed parity: 3 shared-input cases x 2 ARIV rules x 8 comparisons\n")
}

args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "smoke")) {
  smoke()
} else if (length(args) == 2L && args[1L] == "adapter") {
  adapter(args[2L])
} else if (length(args) %in% 2:3 && args[1L] == "parity") {
  parity(args[2L], if (length(args) == 3L) args[3L])
} else {
  stop("Usage: check.R smoke | adapter FIXTURE | parity FIXTURE [PARITY_RECEIPT]")
}
