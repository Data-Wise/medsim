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
  stop("WS-E not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}
