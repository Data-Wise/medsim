# GRILL — medsim #43: fail-loud parallel/serial replication errors

**Date**: 2026-07-31 · **Target**: issue #43 · **Base**: dev @ v0.5.0.9000
**Status**: PROPOSAL — all questions resolved; awaiting go.
**Location**: canonical design doc for #43 (see §0).

**Method**: 4 interactive grill rounds (19 decisions) + 2 independent adversarial
reviews (35 findings, 26 empirically confirmed against code). Both reviews
returned **AMEND-FIRST**. This document is the twice-amended design.

> Authored via `/craft:grill` + 2 adversarial review rounds; ledger below is the source of truth.

---

## 0. Documentation state — resolved (Q4)

**Decision: this grill doc is canonical.** The design had spread across five
surfaces, three already stale within a day.

| Surface | End state |
|---|---|
| This document → `internal/GRILL-medsim-43-...md` | **Canonical** — the only place the design is maintained |
| GitHub issue #43 | Trimmed to defect + repro + link to the grill doc |
| `.STATUS` | One-line pointer |
| `internal/BRAINSTORM-backlog-actions-2026-07-31.md` | #43 addendum removed — it is a *backlog* doc |
| `scratchpad/swallow-bug.md` | Discarded (scratch) |

Tradeoff accepted: the design lives in-repo (private) rather than on GitHub.

---

## 1. The defect

With `parallel = TRUE`, any error escaping the method-level `tryCatch` inside
`medsim_run_single_replication()` is silently discarded — affected replications
vanish from `$results` with no warning, no failure row, no error.

| Link | Location | Behavior |
|---|---|---|
| 1 | `R/runner.R:413-421` | `tryCatch` wraps **only** `method(...)`. `data_generator()` (`:407`) and the reserved-`error` stop (`:429-435`) sit outside and propagate |
| 2 | `R/parallel.R:212-221` | Catches the propagated error, returns a classed `medsim_error` **list** as that task's result |
| 3 | `R/runner.R:749` | `Filter(!is.null(d) && nrow(d) > 0L)` → `nrow(<list>)` is `NULL` → predicate is `NA` → object dropped silently |

**Reproduced**: a `parallel = TRUE` run with an always-failing DGP completes with
zero rows and only a "No numeric columns to summarize" warning. Serial mode stops
loudly on the same error.

**Why it matters now**: this is the masking path for the v0.5.0 numeric-kind
reserved-`error` regression — under parallel, that contract error produces an
empty result set with zero diagnostics. Same silent-wrong-data class the #34
Gates exist to eliminate, one layer down.

---

## 2. Decision ledger

### Round 1 — fix shape

| # | Question | Resolution |
|---|---|---|
| D1 | Where to detect escaped errors | **Inline runner check** — sweep after `medsim_run_parallel()`; report scenario + count + messages |
| D2 | What the stop carries | **Data-carrying condition** `medsim_run_failure`, `tryCatch`-recoverable (#34 precedent) |
| D3 | Defense in depth | **Yes** — `.medsim_rbind_reps()` stops if handed a `medsim_error` object |
| D4 | Widen the inner tryCatch? | **Yes** *(user override)* — `data_generator()` errors become failure rows in BOTH modes |
| D5 | Timing | **After task_bac224eb merges** |

### Round 2 — semantics

| # | Question | Resolution |
|---|---|---|
| G1 | DGP vs method failure marking | **Message prefix** `"data_generator error: <msg>"`; no schema change |
| G2 | Retry a failed DGP draw? | **No** — failure row keeps its global rep id |
| G3 | Analyze accounting | **Split rates** — method-only `failure_rate` + `dgp_failure_rate` |
| G4 | Multi-scenario control flow | **Stop at first failing scenario** |
| G5 | Chunk-mode rescue | **Flagged partial file** *(user override)* |

### Round 3 — after review 1

| # | Question | Resolution |
|---|---|---|
| R1 | G5 survived review? | **Keep partial file, amended** — atomic write + marker + re-`stop()`. Forensics, not salvage |
| R2 | Empty-chunk schema bug vehicle | **Fold into #43** |
| R3 | Serial semantics after D4 | **Full parity + fail-fast** *(fail-fast later dropped — Q2)* |
| R4 | Accounting re-spec depth | **Full identity, all sites** |
| R5 | Condition class + payload | **Subclass `medsim_error` + full `medsim_results`** |

### Round 4 — after review 2 + final questions

| # | Question | Resolution |
|---|---|---|
| — | 12 review-2 amendments | **All accepted.** Two reverse earlier details: `converged = 0` dropped from DGP rows (poisons `converged_mean` — measured 0 with 2 of 3 reps succeeding); `partial_chunk` stays under `on_violation` (a hard floor kills interim looks and pushes users to `"ignore"`) |
| Q1 | §5a `dgp_failure` violation | **In, mandatory** — without it the PR silences `rep_gap` (1→0 measured) and ships a net detection regression |
| Q2 | Fail-fast heuristic | **DROPPED** — saves subsequent scenarios only, never reps (both branches are batch); §1's `is.function()` validation covers the common deterministic case at rep 0. Removes the K knob, its disable rule, and its edge-case test. **Reverses R3's second half** |
| Q3 | Branch size | **Two PRs, cluster first** — PR-A is unblocked and fixes a live bug; PR-B waits on task_bac224eb |
| Q4 | Doc consolidation | **Grill canonical**, issue trimmed to a pointer |

---

## 3. Review findings that changed the design

### BLOCKERS

| # | Finding | Evidence | Consequence |
|---|---|---|---|
| B1 | **The fix removes an existing detector** | 40-rep scenario, 4 DGP failures: today `rep_gap \| n = 1`, after `n = 0`. `ragged_cells` also goes dead | A 10%-DGP-failure chunked run would combine with **zero violations**. Forces §5a |
| B2 | **`partial_chunk` can't rely on Gate A.1** | A.1 builds `expected <- seq_len(max(ids))` (`cluster.R:545`) — self-validating only to the max id present. A 20→15 truncation produced **0 violations** without the optional `nsim` | The marker must be a first-class violation |
| B3 | **A zero-row partial chunk disables all audits** | `chunk_v2 <- identical(attr(NULL,"medsim_schema"), 2L)` is FALSE (`cluster.R:363-366`) → schema stripped from the whole combine, A.1/A.3 skipped. Reproduced | §4's artifact triggers the bug §5 fixes → ordering constraint |
| B4 | **Message reuse breaks a pinned test** | `test-review-fixes.R:54-66` pins `expect_error(..., "reserved")`; `medsim_check_results()` truncates at 60 chars (`parallel.R:530`), message is 221 chars with `"reserved"` at position 75 | Payload message stays untruncated |

### MAJORS that changed the spec

- **Self-inflicted trap**: building `$results` from the mixed list trips D3's own
  assertion mid-construction → recoverable condition becomes a plain crash. Needs
  explicit filter-then-rbind.
- **Wrong provenance object**: `combined <- chunks[[1L]]` (`:355`) inherits chunk 1's
  attr — a partial chunk 3 would be missed. Must iterate `combined$chunk_provenance`
  (`:402-403`).
- **`converged = 0` poisons summaries**: measured `converged_mean = 0` with 2 of 3
  reps succeeding.
- **Empty-list crash**: `sapply(list(), ...)` returns `list()`; `sum()` errors, and
  `medsim_check_results(list())` errors (`parallel.R:505-507`) — hit by the supported
  `n_replications == 0` case (`runner.R:196-198`).
- **`--requeue` premise was wrong**: it covers node failure, preemption, and explicit
  `scontrol requeue` — **a nonzero exit does not requeue**. Re-`stop()` is for
  `sacct` visibility, not automatic retry.
- **Accounting mis-scoped**: 4 sites not 5 (`:414`, `:461`, and `:551`/`:614` in one
  function), plus the un-noticed typed 0-row constructor at `:573-577`. `n_failed`
  is NA-driven, so a clean three-way partition doesn't hold.
- **Test-theater risk**: `medsim_run_parallel()` falls back to sequential when
  `_R_CHECK_LIMIT_CORES_` is set, `n_cores == 1`, or `length(tasks) < 4`
  (`parallel.R:130-142`).
- **Contract error demoted to data**: a non-function `data_generator` would become
  `nsim` rows of garbage. Validate before the tryCatch.
- **Atomic write under-specified**: `file.rename` is atomic only within a filesystem;
  `tempdir()` is typically a different FS on HPC and cross-device rename returns
  `FALSE` silently.

### Checked, no finding

`medsim_error` class-chain collision · `tryCatch(error=)` user code still fires ·
provenance marker survives combine's attribute handling · `file.rename` overwrite
semantics on POSIX · no unoverturned contradictions among D1–D5/G1–G5.

---

## 4. Implementation — two PRs (Q3)

### PR-A — combine gates (unblocked, lands first)

Branch `feature/combine-gate-fixes` from `dev` **now**; no dependency on task_bac224eb.

**§5 — combine gates** (`R/cluster.R`). Schema-vote exemption for NULL/zero-row
chunks (`:363-381`) — fixes a live bug where one such chunk strips schema v2 from
the entire combine and silently skips A.1/A.3 (also bites legitimate empty chunks
when `n_chunks > n_reps`). Add the `partial_chunk` violation iterating
`combined$chunk_provenance` (`:402-403`), inherited attr cleared, routed through
`on_violation`; update the `@param` prose (`:249-250`) and `vignettes/cluster-testing.qmd`.

**§5a — `dgp_failure` Gate A violation** (Q1, mandatory). In
`.medsim_audit_seed_provenance()`, keyed on `any(.medsim_is_dgp_failure(df$error))`.
Ships with the `.medsim_is_dgp_failure()` helper (contract: accepts `NULL`, returns
`logical(0)`). Inert until PR-B produces DGP failure rows — that is expected and
noted in its NEWS entry.

Tests: cases 8, 9, 12 (plus a `partial_chunk` fixture hand-built, since PR-B writes
the real ones).

### PR-B — runner, chunk write, accounting (after task_bac224eb)

Branch `feature/parallel-fail-loud` from `dev`.

**§1 — replication boundary** (`R/runner.R`). Validate
`is.function(scenario$data_generator)` outside any handler (contract error → abort).
Two `tryCatch` scopes: a DGP scope around `:407` producing a failure row
(`error = "data_generator error: <msg>"`, `elapsed = NA_real_`, global rep id,
**no `converged`**, method skipped), and the existing method scope. Reserved-`error`
stop stays outside both. Preserve `set.seed()` → `data_generator()` ordering
(bit-identity). Guard all-NA `min`/`max` in `medsim_summarize_results()`.

**§2 — runner sweep + condition** (`R/runner.R`). Both branches; serial gets a
per-rep wrapper, with the newly-catchable set enumerated in roxygen (reserved-`error`
stop, `.medsim_det_seed()`/`set.seed()` errors, `result_df` assembly). `vapply`
sweep. Filter-then-rbind before payload assembly. Signal `medsim_run_failure`
immediately, class `c("medsim_run_failure","medsim_error","error","condition")`,
carrying a partial `medsim_results` (`$summary` from NULL, warning suppressed) and
`$errors` with `$task` translated to `(scenario, global_rep_id)`. Untruncated
message. Guard `medsim_check_results()` for the empty list. **No fail-fast** (Q2).

**§3 — assertion** (`R/runner.R`). `.medsim_rbind_reps()` stops on `medsim_error`
input; pinned by a direct unit test since real callers can't reach it.

**§4 — chunk write** (`R/cluster.R`). Handler re-stamps provenance (`code_sha`,
`sec_per_rep` with `na.rm` + all-NA guard, `partial = TRUE`), writes atomically to
a dot-prefixed tmp **inside `output_dir`** with checked `file.rename`, then
re-`stop()`s for `sacct` visibility.

**§6 — split accounting** (`R/analyze.R`). Four sites plus the typed 0-row
constructor `:573-577`. Identity stated against `n_total` (`nrow(merged)`),
subtracting DGP rows from the NA-derived count. Both denominators documented
(`failure_rate` excludes DGP rows; `dgp_failure_rate` uses `n_total`; they do not
sum, by design). `test-failure-rate.R` invariants updated.

**§7 — docs/NEWS**. BREAKING: serial DGP errors no longer abort; reserved-`error`
surfaces as a condition with a changed message shape. NEW: `medsim_run_failure` +
recovery idiom, `dgp_failure_rate`/`n_dgp_failed`. Plus the §0 consolidation.

---

## 5. Verification

Each case must FAIL before and PASS after.

| # | Case | PR | Guards |
|---|---|---|---|
| 1 | Reserved-`error` under `parallel = TRUE` → condition, `"reserved"` untruncated | B | v0.5.0 masking path, B4 |
| 2 | Intermittent DGP error → prefixed failure rows, run completes, `dgp_failure_rate` set, `failure_rate` unaffected, `converged_mean` unpoisoned | B | D4/G1/G3 |
| 3 | All tasks failed → condition with NULL results, never silent | B | the core bug |
| 4 | Failure in scenario 2 of 3 → payload has scenario 1 complete, scenario 3 never started | B | G4 |
| 6 | Serial/parallel parity of class and payload shape | B | R3 |
| 7 | Partial file → `partial_chunk` violation; task exited nonzero; **manual re-run** overwrites | B | G5/R1 |
| 8 | Tail truncation, `nsim` not supplied → violation via marker | A | B2 |
| 9 | Empty/zero-row chunk + a §4-written partial file → schema v2 survives, A.1/A.3 run | A (+B pin) | B3 |
| 10 | DGP failures still detected at combine via `dgp_failure` | A (armed in B) | **B1 regression guard** |
| 11 | `.medsim_rbind_reps(list(df, <medsim_error>))` → stop | B | §3 not test-theater |
| 12 | `.medsim_is_dgp_failure(NULL)` → `logical(0)`, count `0L` | A | legacy/user frames |
| 13 | **Negative control**: clean parallel run bit-identical to pre-fix | B | no silent drift |

*(Case 5 removed — fail-fast dropped, Q2.)*

Cases 1, 6, 13 must assert `n_replications >= 4`, `n_cores >= 2`, and unset
`_R_CHECK_LIMIT_CORES_` — otherwise they silently exercise the sequential path.

Gates per PR: `devtools::test()` in-tree (baseline 1117 / 0 / 0 / 1 SKIP — check
testthat's **error** column, not just pass/fail), `R CMD check --as-cran` against
the 1-NOTE baseline, then independent adversarial review of the branch diff before
integration.

Release vehicle: ride `dev`, no 0.5.1 patch (r-universe publishes `0.5.0.9000`).
