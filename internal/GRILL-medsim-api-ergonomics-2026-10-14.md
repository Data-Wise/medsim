# GRILL: medsim API ergonomics (the open questions from PR #57)

**Date:** 2026-10-14 · **Repo:** Data-Wise/medsim · **Source:** the "open questions" in PR #57
**Status:** 5/5 branches locked · #57 merged (`c81f571`); D1, D3 and D4 implemented on `feature/api-ergonomics` · **Next:** release 0.6.0

## Evidence from the codebase sweep

- `config$n` is read at `R/runner.R:516` (generator call, falls back to 200) and at `R/cluster.R:928-935` (pilot-vs-full identity check, where NA vs NA counts as equal).
- `medsim_config()` has `...` for custom parameters (`R/config.R:130`, collected at `:217`). `medsim_config(n = 50)` errors because `n` partially matches `n_replications`, `n_cores` and `n_chunks`.
- `medsim_scenario_dm` stores `n = 200` in `true_params` (`R/scenarios_dm.R:34`).
- `tests/testthat/test-performance.R` (4 calls) passes a data.frame to `medsim_analyze_performance()`.
- `medsim_analyze()` warns once for every output column that has no truth (`R/analyze.R:125`).

## Decision ledger

| # | Branch | Decision | Rejected (and why) |
|---|--------|----------|--------------------|
| D1 | Sample-size argument | `medsim_config(..., n = NULL, n_truth = NULL)`, placed **after `...`** (exact match only). NULL keeps the runner's 200 / 10000 fallbacks. | `n = 200` default (a new config would hold 200 where an old pilot holds NULL, so `R/cluster.R:928` would report a false "n differs"). Per-mode defaults (smoke and production runs would use different DGMs). No change (the partial-match trap stays). |
| D2 | Per-scenario n | Run-level `n` only for now. Grids over n use one run per n (cookbook recipe). | `medsim_scenario(n = )` field (bigger PR; deferred until a cluster array over n needs it). Runner reads `params$n` (silently changes scenarios that already store n, such as the DM factory). |
| D3 | Performance truth | `medsim_analyze_performance()` accepts a `medsim_results` object, merging `$truth` as `<param>_truth` internally, **or** a data.frame (unchanged). | Exported `medsim_attach_truth()` helper (extra step users must find). Object-only (breaks data.frame callers and the 4 tests). |
| D4 | Analyze warnings | Skip `<p>_se`, `<p>_ci_lower`, `<p>_ci_upper`, `<p>_p` silently when `<p>` is scored; collect all other truthless columns into **one** warning. | Suffix skip only (mbco_mi's `indirect_p_fixed`, `r4`, `branch_switch` would each still warn). No warnings (a forgotten `compute_truth` would fail silently). Leave as is. |
| D5 | Landing | Merge #57 as is → one follow-up `feature/*` PR with D1+D3+D4, which also updates the three articles to the new API in the same diff → release 0.6.0. | Fold into #57 (mixes code with already-verified docs). 0.7.0 (articles would ship documenting workarounds). Three PRs (three CI rounds). |

## Test plan (follow-up PR)

- D1: `medsim_config("test", n = 50)$n == 50`. A method returning `nrow(data)` gives 50. With `n` unset, `config$n` is NULL and the runner uses 200. A positional `medsim_config("local", 100)` still sets `n_replications`. The pilot check gives no false positive for a NULL-n pilot against a NULL-n full run.
- D3: The object path equals the manual-merge path (`expect_identical`). A param with no truth column raises an informative error. The 4 existing data.frame tests still pass.
- D4: The Sobel method from the tutorial gives 0 warnings. A method that returns `foo` without a truth gives exactly 1 warning naming `foo`. An mbco_mi result gives 1 warning listing its extra columns.
- E2E: rerun the article chunk harness (each article standalone) after switching the articles to `medsim_config(n = )` and `medsim_analyze_performance(results)`.

## Documentation

- Articles: the tutorial (exercise 2, performance step), analyzing-results (performance section, the accuracy warning note), and the cookbook (sample size, the `n` partial-match note, performance recipe).
- Roxygen for `medsim_config` (`@param n`, `@param n_truth`), `medsim_analyze_performance` (`@param results` accepting both), and `medsim_analyze` (`@details` on warnings). NEWS under 0.6.0 "New features".

## Open questions

- **Conflict found while implementing (2026-10-14).** #57 added `medsim_run_chunk(compute_truth = )` and forwards it to **every** chunk. The authoritative `internal/SPEC-medsim-41-truth-integrity-2026-08-22.md` (§D1, issue #41, still open) gates it to `chunk_id == 1L`, with a `config$compute_truth_every_chunk = TRUE` escape hatch, and pairs it with an atomic `medsim_cache_save()` (§D3). Not changed in this PR; the author decides whether to align before 0.6.0.

- A per-scenario `n` field (D2) is deferred. Revisit when a cluster array over n is needed.
- Should `n_truth` also be validated (positive integer)? To settle while implementing D1.
