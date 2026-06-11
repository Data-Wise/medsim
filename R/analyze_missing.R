# INTERFACE FREEZE (Step 0) — signature stub for WS-E.
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md  (§"5. Coverage/metrics")
# NEW file (NOT editing analyze.R) so WS-E builds beside the DM proposal's interval-coverage work.

#' Summarize the MBCO branch-switch rate per scenario (WS-E)
#'
#' Reads the `branch_switch` column emitted by [medsim_method_mbco_mi()] (the union-null /
#' ab=0 diagnostic from MEMO-MBCO-MI-derivation) and returns a per-scenario summary suitable
#' for joining as a column onto a `medsim_table_coverage()` table. Non-converged rows
#' (`converged == 0`) are dropped before summarizing.
#'
#' @param results A `medsim_results` object from [medsim_run()].
#' @param by Grouping columns (default `"scenario"`).
#' @return A data.frame: one row per group with `branch_switch_rate` and `n_valid`.
#' @export
medsim_summarize_branch_switch <- function(results, by = "scenario") {
  df <- if (is.data.frame(results)) {
    results
  } else if (is.list(results) && !is.null(results$results)) {
    results$results
  } else {
    stop("`results` must be a data.frame or a medsim_results object with a `results` element.")
  }

  required <- c(by, "branch_switch", "converged")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      "`results` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  # Drop non-converged rows before summarizing.
  keep <- !is.na(df$converged) & df$converged != 0
  df <- df[keep, , drop = FALSE]

  group_vals <- df[, by, drop = FALSE]
  groups <- split(seq_len(nrow(df)), group_vals, drop = TRUE)

  out_rows <- lapply(names(groups), function(key) {
    idx <- groups[[key]]
    bs <- df$branch_switch[idx]
    row <- group_vals[idx[1L], , drop = FALSE]
    rownames(row) <- NULL
    row$branch_switch_rate <- mean(bs, na.rm = TRUE)
    row$n_valid <- length(idx)
    row
  })

  out <- do.call(rbind, out_rows)
  rownames(out) <- NULL
  out
}
