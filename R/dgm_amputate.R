# INTERFACE FREEZE (Step 0) — signature stub for WS-B.
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md  (§"2. Missingness amputation")

#' Insert missing values under MCAR / MAR / MNAR (WS-B)
#'
#' Thin, documented amputer aligning `mice::ampute` semantics to the medsim DGM contract.
#' Returns the input frame with `NA`s inserted in `target` column(s); column names/order
#' are preserved (DGM data contract).
#'
#' @param data data.frame with columns `X, M, Y` (+ optional covariates).
#' @param target Character vector of column(s) to set `NA` (e.g. `"M"`, or `c("M","Y")`).
#' @param mechanism One of `"MCAR"`, `"MAR"`, `"MNAR"`. MCAR: constant prob; MAR: logistic on
#'   observed `predictors`; MNAR: logistic including the `target` itself (self-mechanism).
#' @param prop Target overall missingness proportion in `target`.
#' @param predictors Columns driving missingness for MAR/MNAR (defaults to all non-`target`).
#' @param weights Optional named weights for the missingness logistic.
#' @param type Amputation tail type, `mice::ampute`-style: one of `"RIGHT","LEFT","MID","TAIL"`.
#' @return `data` with `NA`s inserted in `target`; names/order preserved.
#' @export
medsim_amputate <- function(data, target, mechanism = c("MCAR", "MAR", "MNAR"),
                            prop = 0.2, predictors = NULL, weights = NULL,
                            type = c("RIGHT", "LEFT", "MID", "TAIL")) {
  mechanism <- match.arg(mechanism)
  type <- match.arg(type)
  stop("WS-B not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}
