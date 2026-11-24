#!/usr/bin/env Rscript

report_driver <- function(directory) {
  path <- file.path(directory, "run.R")
  driver <- new.env(parent = baseenv())
  driver$commandArgs <- function(...) paste0("--file=", path)
  sys.source(path, envir = driver)
  rm("commandArgs", envir = driver)
  driver
}

factorial_counts <- function(rows, harness) {
  usable <- harness$calibration_usable(rows)
  four <- Reduce(`&`, usable)
  reject <- lapply(rows[harness$.calibration_arms], function(p) as.integer(p[four] < .05))
  ariv_li <- reject$fixed_li - reject$own_li
  ariv_chan_meng <- reject$fixed_chan_meng - reject$own_chan_meng
  differences <- list(
    ariv_li = ariv_li,
    ariv_chan_meng = ariv_chan_meng,
    df_own = reject$own_chan_meng - reject$own_li,
    df_fixed = reject$fixed_chan_meng - reject$fixed_li,
    interaction = ariv_chan_meng - ariv_li
  )
  comparison_usable <- with(usable, c(
    sum(fixed_li & own_li), sum(fixed_chan_meng & own_chan_meng),
    sum(own_chan_meng & own_li), sum(fixed_chan_meng & fixed_li), sum(four)
  ))
  counts <- t(vapply(differences, function(d) tabulate(d + 3L, 5L), integer(5)))
  colnames(counts) <- c("minus_two", "minus_one", "zero", "plus_one", "plus_two")
  cbind(comparison_usable = comparison_usable, counts)
}

factorial_summary <- function(counts) {
  signed <- counts[, -1L, drop = FALSE]
  n <- rowSums(signed)
  total <- drop(signed %*% (-2:2))
  squares <- drop(signed %*% (-2:2)^2)
  estimate <- mcse <- rep(NA_real_, length(n))
  estimate[n > 0] <- total[n > 0] / n[n > 0]
  enough <- n > 1
  mcse[enough] <- sqrt(
    (squares[enough] - total[enough] * estimate[enough]) / (n[enough] * (n[enough] - 1))
  )
  data.frame(contrast = rownames(counts), comparison_usable = counts[, 1L],
             usable_four = n, signed, estimate = estimate, mcse = mcse,
             row.names = NULL)
}

report_selection <- function(selected, group, chunks) {
  if (is.null(selected) || !length(selected)) stop("Missing selected chunk in ", group, " chunk 1")
  if (!is.character(selected) || is.null(names(selected)) || anyNA(names(selected))) {
    stop("Invalid chunk selection in ", group)
  }
  parts <- lapply(strsplit(names(selected), "/", fixed = TRUE), utils::tail, n = 5L)
  if (any(lengths(parts) != 5L)) {
    stop("Invalid selected chunk path in ", group, ": ", names(selected)[which(lengths(parts) != 5L)[1L]])
  }
  parts <- do.call(rbind, parts)
  ids <- as.character(seq_len(chunks))
  missing <- setdiff(ids, parts[, 3L])
  if (length(missing)) stop("Missing selected chunk in ", group, " chunk ", missing[1L])
  duplicate <- anyDuplicated(parts[, 3L])
  if (duplicate) stop("Duplicate selected chunk in ", group, " chunk ", parts[duplicate, 3L])
  extra <- setdiff(parts[, 3L], ids)
  if (length(extra)) stop("Unexpected selected chunk in ", group, " chunk ", extra[1L])
  order <- match(ids, parts[, 3L])
  parts <- parts[order, , drop = FALSE]
  filenames <- vapply(seq_len(chunks), medsim:::.medsim_chunk_filename, "")
  valid <- parts[, 1L] == "attempts" & parts[, 2L] == group &
    grepl("^[1-9][0-9]*$", parts[, 4L]) & parts[, 5L] == filenames
  if (!all(valid)) stop("Invalid selected path in ", group, " chunk ", which(!valid)[1L])
  valid_hash <- grepl("^[a-f0-9]{64}$", selected[order])
  if (!all(valid_hash)) stop("Invalid selected hash in ", group, " chunk ", which(!valid_hash)[1L])
  data.frame(
    path = file.path("attempts", group, ids, parts[, 4L], filenames),
    attempt = parts[, 4L], hash = unname(selected[order])
  )
}

report_payload <- function(root, entry, plan, group, chunk, scenarios, driver) {
  path <- file.path(root, entry$path)
  payload <- driver$read_hashed_rds(path, entry$hash, "Selected payload")$value
  start <- readRDS(file.path(dirname(path), "started.rds"))
  receipt <- readRDS(file.path(dirname(path), "receipt.rds"))
  if (!identical(start$manifest, plan$hash) || !identical(start$group, group$group) ||
    !identical(start$chunk, chunk) || !identical(as.character(start$attempt), entry$attempt) ||
    !identical(receipt[names(start)], start) || !identical(receipt$status, "complete") ||
    !identical(receipt$output_hash, entry$hash)) {
    stop("Invalid selected attempt receipts")
  }
  provenance <- attr(payload, "provenance", exact = TRUE)
  if (!identical(provenance$code_sha, plan$runtime$medsim_sha) || !isFALSE(provenance$partial)) {
    stop("Invalid selected payload provenance")
  }
  rows <- payload$results
  ids <- medsim:::.medsim_chunk_indices(group$attempts, group$chunks, chunk)
  if (!is.character(rows$scenario) || !is.numeric(rows$replication) ||
    !all(rows$scenario %in% scenarios) || !all(rows$replication %in% ids)) {
    stop("Invalid replicate keys")
  }
  keys <- paste(rows$scenario, rows$replication)
  expected <- as.vector(outer(scenarios, ids, paste))
  if (length(keys) != length(expected) || anyDuplicated(keys) || !setequal(keys, expected)) {
    stop("Wrong replicate keys")
  }
  if (any(medsim:::.medsim_is_dgp_failure(rows$error))) stop("DGP failure in selected payload")
  rows
}

report_diagnostics <- function(official, cell) {
  fields <- c("branch_mix", "branch_mix_denominator", "complete_pairs",
              "complete_pair_difference", "complete_pair_mcse", "u_paired", "u_size")
  required <- c("attempts", if (cell$is_null) fields else fields[1:5])
  if (!is.numeric(official) || anyDuplicated(names(official)) ||
    !all(required %in% names(official)) || !isTRUE(official[["attempts"]] == cell$attempts)) {
    stop("Incomplete decision diagnostics for ", cell$scenario)
  }
  diagnostics <- unname(official[fields])
  counts <- diagnostics[1:3]
  if (any(!is.finite(counts) | counts != floor(counts) | counts < 0 | counts > cell$attempts) ||
    counts[1L] > counts[2L]) {
    stop("Invalid decision counts for ", cell$scenario)
  }
  bounds <- diagnostics[6:7]
  if (cell$is_null && (any(!is.finite(bounds)) || abs(bounds[1L]) > 1 ||
    bounds[2L] < 0 || bounds[2L] > 1)) {
    stop("Invalid null gate bounds for ", cell$scenario)
  }
  if (!cell$is_null) diagnostics[6:7] <- NA_real_
  stats::setNames(as.list(diagnostics), fields)
}

report_run <- function(root, output, decision_hash, driver) {
  if (file.exists(output)) stop("Report already exists: ", output)
  root <- normalizePath(root, mustWork = TRUE)
  destination <- normalizePath(dirname(output), mustWork = TRUE)
  attempts <- normalizePath(file.path(root, "attempts"), mustWork = TRUE)
  if (destination == attempts || startsWith(destination, paste0(attempts, "/"))) {
    stop("Choose a report destination outside the attempts directory")
  }
  decision_path <- file.path(root, "decision.rds")
  manifest_path <- file.path(root, "manifest.rds")
  if (!is.character(decision_hash) || length(decision_hash) != 1L ||
    !grepl("^[a-f0-9]{64}$", decision_hash)) stop("Expected a decision SHA-256 digest")
  decision <- driver$read_hashed_rds(decision_path, decision_hash, "Decision receipt")$value
  manifest_hash <- decision$manifest_hash
  if (!is.character(manifest_hash) || length(manifest_hash) != 1L ||
    !grepl("^[a-f0-9]{64}$", manifest_hash)) stop("Missing or invalid manifest hash")
  plan <- driver$read_hashed_rds(manifest_path, manifest_hash, "Manifest")$value
  plan$hash <- manifest_hash
  grid <- plan$grid
  groups <- plan$groups
  if (!nrow(grid) || anyDuplicated(grid$scenario) || anyDuplicated(groups$group) ||
    !is.logical(grid$is_null) || anyNA(grid$is_null) ||
    any(!is.finite(groups$chunks) | groups$chunks != floor(groups$chunks) |
      groups$chunks < 1L | groups$chunks > groups$attempts) ||
    any(!is.finite(groups$attempts) | groups$attempts != floor(groups$attempts)) ||
    !all(grepl("^[A-Za-z0-9_-]+$", groups$group)) ||
    !setequal(groups$group, grid$group) || !setequal(groups$group, names(decision$chunks)) ||
    anyDuplicated(names(decision$chunks)) || !setequal(grid$scenario, names(decision$cells)) ||
    anyDuplicated(names(decision$cells))) {
    stop("Manifest and decision have inconsistent groups or cells")
  }
  totals <- vector("list", nrow(grid))
  for (i in seq_len(nrow(groups))) {
    group <- groups[i, ]
    cells <- which(grid$group == group$group)
    scenarios <- grid$scenario[cells]
    if (!all(grid$attempts[cells] == group$attempts)) stop("Wrong budget in ", group$group)
    selection <- report_selection(decision$chunks[[group$group]], group$group, group$chunks)
    for (chunk in seq_len(group$chunks)) {
      counts <- tryCatch({
        rows <- report_payload(root, selection[chunk, ], plan, group, chunk, scenarios, driver)
        lapply(scenarios, function(scenario) {
          factorial_counts(rows[rows$scenario == scenario, ], driver$src)
        })
      }, error = function(e) {
        stop(group$group, " chunk ", chunk, ": ", conditionMessage(e), call. = FALSE)
      })
      totals[cells] <- if (chunk == 1L) counts else Map(`+`, totals[cells], counts)
    }
  }
  report <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    cell <- grid[i, ]
    official <- decision$cells[[cell$scenario]]$decision
    diagnostics <- report_diagnostics(official, cell)
    contrasts <- factorial_summary(totals[[i]])
    data.frame(cell, contrasts, diagnostics, manifest_sha256 = manifest_hash,
               decision_sha256 = decision_hash, row.names = NULL)
  }))
  temporary <- tempfile(".factorial-", tmpdir = destination)
  on.exit(unlink(temporary))
  utils::write.csv(report, temporary, row.names = FALSE, na = "")
  if (!file.link(temporary, output)) stop("Could not publish a new report: ", output)
  invisible(report)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) stop("Usage: report.R RUN_DIR REPORT_CSV DECISION_SHA256")
  entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  report_run(args[1L], args[2L], args[3L], report_driver(dirname(normalizePath(entry))))
}
