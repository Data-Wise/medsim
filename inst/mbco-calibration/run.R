#!/usr/bin/env Rscript

entry <- grep("^--file=", commandArgs(), value = TRUE)
directory <- dirname(normalizePath(sub("^--file=", "", entry)))
script <- file.path(directory, "run.R")
src <- new.env(parent = baseenv())
sys.source(file.path(directory, "harness.R"), envir = src)

sha256 <- function(path) {
  executable <- Sys.which("sha256sum")
  args <- "-b"
  if (!nzchar(executable)) {
    executable <- Sys.which("shasum")
    args <- c("-a", "256", args)
  }
  if (!nzchar(executable) || !file.exists(path)) stop("Cannot hash ", path)
  # Hash the bytes through stdin so the tool never prints an escaped path.
  value <- system2(executable, args, stdout = TRUE, stdin = path)
  if (!is.null(attr(value, "status")) || length(value) != 1L ||
      !grepl("^[0-9a-f]{64} ", value)) {
    stop("Invalid SHA-256 output for ", path)
  }
  substr(value, 1L, 64L)
}

read_hashed_rds <- function(path, expected = NULL, label = basename(path)) {
  snapshot <- tempfile("medsim-rds-")
  on.exit(unlink(snapshot))
  if (!file.copy(path, snapshot)) stop("Cannot snapshot ", path)
  hash <- sha256(snapshot)
  if (!is.null(expected) && !identical(hash, expected)) stop(label, " hash mismatch")
  list(hash = hash, value = readRDS(snapshot))
}

dependency_versions <- function(packages) {
  db <- utils::installed.packages()
  dependencies <- tools::package_dependencies(
    packages,
    db = db, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE
  )
  pkgs <- sort(unique(c(packages, unlist(dependencies))))
  if (!all(pkgs %in% rownames(db))) stop("Missing runtime dependencies.")
  db[pkgs, "Version"]
}

runtime <- function(sha) {
  if (length(sha) != 1L || !grepl("^[0-9a-f]{40}$", sha) ||
    !identical(utils::packageDescription("medsim")$RemoteSha, sha)) {
    stop("Install medsim from the pinned revision with its RemoteSha recorded.")
  }
  list(
    medsim_sha = sha, r = R.version.string, platform = R.version$platform,
    libraries = extSoftVersion(), rng = RNGkind(),
    packages = dependency_versions(c("medsim", "mice")),
    medsim_code = sha256(system.file("R", "medsim.rdb", package = "medsim")),
    harness = sha256(file.path(directory, "harness.R")), driver = sha256(script),
    checker = sha256(file.path(directory, "check.R"))
  )
}

positive_integer <- function(x) {
  if (!grepl("^[1-9][0-9]*$", x) || is.na(suppressWarnings(as.integer(x)))) {
    stop("Expected a positive integer, got ", x)
  }
  as.integer(x)
}

new_directory <- function(path) {
  if (!dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create new directory ", path)
  }
}

save_atomic <- function(value, path) {
  tmp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(tmp))
  saveRDS(value, tmp)
  if (!file.rename(tmp, path)) stop("Cannot save ", path)
}

combine_groups <- function(grid, null_chunks, power_chunks) {
  group <- unique(grid[c("group", "n", "is_null", "attempts")])
  group$chunks <- ifelse(group$is_null, positive_integer(null_chunks),
    positive_integer(power_chunks)
  )
  if (any(group$chunks > group$attempts)) stop("More chunks than attempts.")
  group
}

plan_run <- function(root, sha, null_chunks, power_chunks) {
  grid <- src$calibration_grid()
  src$calibration_check_seeds(grid)
  group <- combine_groups(grid, null_chunks, power_chunks)
  plan <- list(grid = grid, groups = group, runtime = runtime(sha), command = commandArgs())
  new_directory(root)
  save_atomic(plan, file.path(root, "manifest.rds"))
}

read_plan <- function(root) {
  path <- file.path(root, "manifest.rds")
  plan <- readRDS(path)
  if (!identical(plan$runtime, runtime(plan$runtime$medsim_sha)) ||
    !identical(plan$grid, src$calibration_grid()) ||
    !identical(plan$groups, combine_groups(
      plan$grid, as.character(unique(plan$groups$chunks[plan$groups$is_null])),
      as.character(unique(plan$groups$chunks[!plan$groups$is_null]))
    ))) {
    stop("Runtime or grid differs from the frozen manifest.")
  }
  plan$hash <- sha256(path)
  plan
}

run_chunk <- function(root, group, chunk, attempt, cores) {
  plan <- read_plan(root)
  config <- plan$groups[plan$groups$group == group, ]
  if (nrow(config) != 1L) stop("Unknown group ", group)
  chunk <- positive_integer(chunk)
  attempt <- positive_integer(attempt)
  if (chunk > config$chunks) stop("Chunk is outside the manifest.")
  cores <- positive_integer(cores)
  check_open <- function() {
    if (file.exists(file.path(root, "decision.rds")) ||
      dir.exists(file.path(root, "finalize.lock"))) {
      stop("The manifest is finalizing or already finalized.")
    }
  }
  check_open()
  dir <- file.path(root, "attempts", group, as.character(chunk), as.character(attempt))
  new_directory(dir)
  settings <- medsim::medsim_config(
    "cluster",
    n = config$n, n_replications = config$attempts,
    n_chunks = config$chunks, chunk_id = chunk, n_cores = cores,
    output_dir = dir
  )
  receipt <- list(
    manifest = plan$hash, group = group, chunk = chunk,
    attempt = attempt, command = commandArgs()
  )
  save_atomic(receipt, file.path(dir, "started.rds"))
  status <- "error"
  on.exit({
    output <- list.files(dir, "^chunk_.*[.]rds$", full.names = TRUE)
    receipt$status <- status
    receipt$output_hash <- if (length(output) == 1L) sha256(output) else NA_character_
    save_atomic(receipt, file.path(dir, "receipt.rds"))
  })
  check_open()
  scenarios <- src$calibration_scenarios(plan$grid[plan$grid$group == group, ])
  medsim::medsim_run_chunk(scenarios, src$calibration_fit, settings,
    code_sha = plan$runtime$medsim_sha
  )
  status <- "complete"
}

read_attempts <- function(root, plan, group, chunk, scenarios, ids) {
  dir <- file.path(root, "attempts", group, as.character(chunk))
  dirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  paths <- unlist(lapply(dirs, function(attempt) {
    outputs <- list.files(attempt, "^chunk_.*[.]rds$", full.names = TRUE)
    if (length(outputs) > 1L ||
      (length(outputs) == 1L &&
        basename(outputs) != medsim:::.medsim_chunk_filename(chunk))) {
      stop("Unexpected chunk output in ", attempt)
    }
    outputs
  }), use.names = FALSE)
  key <- function(rows) paste(rows$scenario, rows$replication, sep = ":")
  expected <- unlist(lapply(scenarios, paste, ids, sep = ":"), use.names = FALSE)
  read_output <- function(path) {
    start <- readRDS(file.path(dirname(path), "started.rds"))
    if (!identical(start$manifest, plan$hash) || !identical(start$group, group) ||
      !identical(start$chunk, chunk) ||
      !identical(as.character(start$attempt), basename(dirname(path)))) {
      stop("Unverified attempt identity in ", path)
    }
    receipt_path <- file.path(dirname(path), "receipt.rds")
    receipt <- if (file.exists(receipt_path)) readRDS(receipt_path)
    if (!is.null(receipt) && (!identical(receipt[names(start)], start) ||
      !identical(receipt$output_hash, sha256(path)))) {
      stop("Attempt output hash mismatch in ", path)
    }
    obj <- readRDS(path)
    provenance <- attr(obj, "provenance", exact = TRUE)
    if (!identical(provenance$code_sha, plan$runtime$medsim_sha)) {
      stop("Attempt code identity mismatch in ", path)
    }
    rows <- obj$results
    if (is.null(rows) || !nrow(rows)) {
      return(list(rows = rows, complete = FALSE))
    }
    keys <- key(rows)
    if (anyDuplicated(keys) || !all(keys %in% expected)) {
      stop("Invalid attempt keys in ", path)
    }
    if (any(medsim:::.medsim_is_dgp_failure(rows$error))) stop("DGP failure in ", path)
    list(rows = rows, complete = identical(receipt$status, "complete") &&
      isFALSE(provenance$partial) && length(keys) == length(expected))
  }
  selected <- NULL
  for (path in paths) {
    candidate <- read_output(path)
    if (candidate$complete) {
      selected <- path
      reference <- candidate$rows
      break
    }
  }
  if (is.null(selected)) stop("No complete audited attempt for ", group, " chunk ", chunk)
  for (path in setdiff(paths, selected)) {
    rows <- read_output(path)$rows
    if (is.null(rows) || !nrow(rows)) next
    matched <- match(key(rows), key(reference))
    fields <- setdiff(union(names(rows), names(reference)), "elapsed")
    for (field in fields) {
      x <- rows[[field]]
      y <- reference[[field]][matched]
      # Partial files may omit a column whose values are all missing.
      if (!identical(x, y) && !((is.null(x) || all(is.na(x))) &&
        (is.null(y) || all(is.na(y))))) {
        stop("Retry disagreement in ", field, " for ", path)
      }
    }
  }
  selected
}

parity_fixture_sha <- "b88d9e26e462f6f387ea711766511507f8c638939f18d95ddab67c640964e4e8"

parity_values_agree <- function(a, b, field) {
  if (!is.numeric(a) || !is.numeric(b) || length(a) != 1L || length(b) != 1L) return(FALSE)
  if (is.finite(a) && is.finite(b)) return(abs(a - b) <= 1e-10 * max(1, abs(a), abs(b)))
  field == "nu" && identical(a, Inf) && identical(b, Inf)
}

validate_parity <- function(parity, pin) {
  valid <- function(ok) {
    if (!isTRUE(ok)) stop("A complete, passing shared-input parity receipt for this runtime is required.")
  }
  valid(is.list(parity) && !anyDuplicated(names(parity)))
  valid(isTRUE(parity$passed) && identical(parity$medsim_sha, pin$medsim_sha) &&
    identical(parity$medsim_code, pin$medsim_code) &&
    identical(parity$checker_hash, pin$checker) &&
    identical(parity$harness_hash, pin$harness) &&
    identical(parity$missingmed_sha, src$.calibration_missingmed_sha) &&
    identical(parity$fixture_blob, src$.calibration_fixture_blob) &&
    identical(parity$fixture_sha256, parity_fixture_sha) &&
    is.character(parity$missingmed_code) && length(parity$missingmed_code) == 1L &&
    grepl("^[0-9a-f]{64}$", parity$missingmed_code) &&
    is.character(parity$command) && length(parity$command) > 0L &&
    all(!is.na(parity$command) & nzchar(parity$command)) &&
    identical(parity$r, pin$r) && identical(parity$rng, pin$rng) &&
    is.character(parity$packages) && !anyDuplicated(names(parity$packages)) &&
    all(!is.na(parity$packages) & nzchar(parity$packages)) &&
    all(c(names(pin$packages), "missingmed") %in% names(parity$packages)) &&
    identical(parity$packages[names(pin$packages)], pin$packages))

  cases <- c("seed26", "seed9", "seed28")
  ariv <- c("own", "fixed")
  fields <- c("D4", "r4", "nu", "p")
  valid(is.list(parity$comparisons) && length(parity$comparisons) == 3L &&
    setequal(names(parity$comparisons), cases))
  for (case in cases) {
    pair <- parity$comparisons[[case]]
    valid(is.list(pair) && length(pair) == 2L && setequal(names(pair), ariv))
    for (mode in ariv) {
      comparison <- pair[[mode]]
      valid(is.list(comparison) && !anyDuplicated(names(comparison)))
      x <- comparison$medsim
      y <- comparison$missingmed
      valid(is.numeric(x) && is.numeric(y) && !anyDuplicated(names(x)) &&
        !anyDuplicated(names(y)) && all(c(fields, "stacked_branch") %in% names(x)) &&
        all(fields %in% names(y)))
      valid(all(vapply(fields, function(field) parity_values_agree(x[[field]], y[[field]], field),
        logical(1))))
      valid(identical(attr(y, "ariv"), mode) && x[["stacked_branch"]] %in% 0:1 &&
        identical(attr(y, "stacked_branch"), if (x[["stacked_branch"]] == 1) "a" else "b") &&
        comparison$medsim_branch_mix %in% 0:1 &&
        isTRUE(comparison$medsim_branch_mix == attr(y, "branch_mix")) &&
        isTRUE(attr(y, "k") == 1L) && (x[["p"]] < .05) == (y[["p"]] < .05))
    }
  }
  invisible(TRUE)
}

finalize <- function(root, parity_path, combine_chunks = medsim::medsim_combine_chunks,
                     audit_implementation = NULL) {
  plan <- read_plan(root)
  parity <- read_hashed_rds(parity_path)
  validate_parity(parity$value, plan$runtime)
  report_path <- file.path(root, "decision.rds")
  lock <- file.path(root, "finalize.lock")
  new_directory(lock)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (file.exists(report_path)) stop("This manifest already has a final decision.")
  attempts <- file.path(root, "attempts")
  snapshot <- function() {
    paths <- list.files(attempts, pattern = "[.]rds$", recursive = TRUE, full.names = TRUE)
    info <- file.info(paths)[c("size", "mtime")]
    row.names(info) <- paths
    info
  }
  initial <- snapshot()
  if (!all(list.dirs(attempts, recursive = FALSE, full.names = FALSE) %in%
    plan$groups$group)) {
    stop("Unexpected attempt group.")
  }
  selected <- list()
  for (i in seq_len(nrow(plan$groups))) {
    group <- plan$groups[i, ]
    if (!all(list.dirs(file.path(attempts, group$group),
      recursive = FALSE,
      full.names = FALSE
    ) %in% as.character(seq_len(group$chunks)))) {
      stop("Unexpected chunk in ", group$group)
    }
    cells <- plan$grid[plan$grid$group == group$group, ]
    dir <- file.path(lock, group$group)
    new_directory(dir)
    selected[[group$group]] <- vapply(seq_len(group$chunks), function(chunk) {
      ids <- medsim:::.medsim_chunk_indices(group$attempts, group$chunks, chunk)
      path <- read_attempts(root, plan, group$group, chunk, cells$scenario, ids)
      target <- file.path(dir, medsim:::.medsim_chunk_filename(chunk))
      if (!file.link(path, target)) stop("Cannot stage audited chunk ", path)
      path
    }, character(1))
  }
  # Every planned chunk has now been reconciled. The stock audit still runs.
  results <- list()
  for (i in seq_len(nrow(plan$groups))) {
    group <- plan$groups[i, ]
    cells <- plan$grid[plan$grid$group == group$group, ]
    combined <- combine_chunks(
      file.path(lock, group$group),
      expected_chunks = group$chunks, nsim = group$attempts,
      collapse_exclude = c(
        medsim:::.medsim_collapse_exclude_default,
        "effective_k", "realized_missing"
      ),
      collapse_p_columns = c(src$.calibration_arms, "indirect_p", "indirect_p_fixed")
    )$results
    for (j in seq_len(nrow(cells))) {
      cell <- cells[j, ]
      rows <- combined[combined$scenario == cell$scenario, ]
      results[[cell$scenario]] <- src$calibration_summarize(rows, cell)
    }
  }
  stopifnot(setequal(names(results), plan$grid$scenario))
  pass <- all(vapply(results[plan$grid$scenario[plan$grid$is_null]], function(x) {
    x$decision[["passed"]] == 1
  }, logical(1)))
  if (!identical(initial, snapshot())) stop("Attempt files changed during finalization.")
  save_atomic(list(
    manifest_hash = plan$hash, parity_hash = parity$hash,
    chunks = lapply(selected, function(paths) stats::setNames(vapply(paths, sha256, ""), paths)),
    cells = results, passed = pass, command = commandArgs(),
    audit_implementation = audit_implementation
  ), report_path)
  cat(if (pass) "PASS\n" else "HOLD\n")
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 5L && args[1L] == "plan") {
    plan_run(args[2L], args[3L], args[4L], args[5L])
  } else if (length(args) == 6L && args[1L] == "chunk") {
    run_chunk(args[2L], args[3L], args[4L], args[5L], args[6L])
  } else if (length(args) == 3L && args[1L] == "finalize") {
    finalize(args[2L], args[3L])
  } else {
    stop(
      "Usage: run.R plan DIR MEDSIM_SHA NULL_CHUNKS POWER_CHUNKS | ",
      "chunk DIR GROUP CHUNK ATTEMPT CORES | finalize DIR PARITY_RECEIPT"
    )
  }
}
