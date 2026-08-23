# GRILL — medsim #43: fail-loud parallel/serial replication errors

**Date**: 2026-07-31 · **Amended**: 2026-08-22 (implementation log) · **Target**: issue #43 · **Base**: dev @ v0.5.0.9000
**Status**: ✅ **IMPLEMENTED & MERGED** — three PRs, all squashed to `dev`
2026-08-22; **issue #43 CLOSED** 2026-08-22T22:16Z. Final state on `dev`:
**1197 pass / 0 fail / 0 error / 1 skip**, `R CMD check --as-cran` **0/0/0**.
See §6 Implementation log for what the build changed versus this design.
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

---

## 6. Implementation log (2026-08-22)

Shipped as **three** PRs, not the two this design planned. All squash-merged
to `dev`; issue #43 closed 2026-08-22T22:16Z. Final `dev` state: **1197 pass /
0 fail / 0 error / 1 skip**, `R CMD check --as-cran` **0/0/0**.

| PR | Squash | Scope | Note |
|---|---|---|---|
| **#47** | `523ce14` | numeric-kind `error` → `est_error` rename | **Not in this design** — see L1 |
| **#48** | `43946e9` | PR-A — combine gates (§5, §5a) | `/code-review` found a hard crash not in the design; fixed pre-merge (L2) |
| **#49** | `8ed4c1f` | PR-B — runner, chunk write, accounting (§1–§4, §6, §7) | Rebased onto #48's squashed tip before merge |

### What the build changed versus this design

| # | Design said | Build found | Resolution |
|---|---|---|---|
| L1 | **Q3:** two PRs; PR-B waits on `task_bac224eb` | That branch bundled a clean `est_error` rename with a **dangerous** parallel re-raise — a plain `stop()` discarding completed replications (A/B measured: `dev` returns 15 of 20 rows, the branch discards all 15) | Branch **split**: the rename shipped alone as **PR #47**; the re-raise was rebuilt properly as PR-B's data-carrying `medsim_run_failure`. Three PRs, not two. |
| L2 | §4's chunk-write rescue is the risky part | `/code-review` on PR-A found an unrelated **hard crash**: with *every* chunk empty, `schema_v2` is vacuously TRUE (zero-length `all()`) while `all_results` is NULL — and `attr(NULL, "x") <- 2L` **errors** in R, unlike assigning NULL, which is a silent no-op. Unreachable before PR-A | Guarded (`schema_v2 && !is.null(all_results)`), regression test at `test-cluster.R:338`. Worse than the reviewer's "inconsistent object" framing — it was a crash. |
| L3 | Cases 4/7: trigger a task-level failure with `stop()` inside `method()` | **Wrong trigger.** A plain `stop()` in `method()` is caught by the *existing* inner `tryCatch` and becomes an ordinary failure row — it never escapes to the sweep | Tests use the reserved-`error`-field contract violation instead: `method()` returns successfully with `list(indirect =, error =)`, failing the post-method check **outside** the method-level handler. |
| L4 | Case 7: reps after a mid-chunk failure are lost (expected 2 of 4 rows) | `lapply` runs **every** task regardless of an earlier failure — only the failing rep is lost | Expected counts corrected 2 → 3, in both the direct chunk read and the post-combine assertion. |
| L5 | The partial payload just needs `rbind` | The `medsim_run_failure` payload was **not schema-v2 stamped**, so PR-A's own schema vote read it as legacy and disabled Gates A.1/A.3 — surfaced as unexpected "lack the schema-v2 stamp" warnings in the new tests | Shared `.medsim_stamp_schema_v2()` helper applied on both the normal-completion and partial-failure paths. |

### Verification cases — as built

Implemented **1, 2, 3, 4, 6, 7, 10, 11, 13** in
`tests/testthat/test-parallel-fail-loud.R` (11 `test_that` blocks: the nine
numbered cases plus the `medsim_check_results()` empty-list and
`medsim_summarize_results()` all-NA guards from §1/§2). Cases **8, 9, 12** live
in `tests/testthat/test-cluster.R` as designed (PR-A, hand-built fixtures),
alongside L2's regression test. Case 5 stays removed (fail-fast dropped, Q2).

### Release vehicle — superseded by events

This design said "ride `dev`, no 0.5.1 patch". **v0.5.1 was released
2026-08-17**, before the work landed, so #43 shipped onto `0.5.1.9000` and
rides the next release instead.

### ⚠️ Post-merge adversarial review (2026-08-22) — 2 MAJOR defects found in the shipped code

An adversarial review run *after* the merge found two confirmed gaps that the
pre-merge reviews missed. **Both are still live on `dev`.**

| # | Severity | Defect |
|---|---|---|
| **PM1** | MAJOR, CONFIRMED | **An all-reps-failed chunk writes no file, so a trailing failed chunk combines with ZERO violations.** `R/cluster.R:224-229` early-`stop()`s before the provenance stamp when nothing survived, so the `partial_chunk` marker never exists. Critically, **the very defect class #43 was filed for** — the deterministic reserved-`error` contract violation — fails *every* rep of any chunk it touches, so `partial_chunk` can never fire for it. Reproduced: 2 chunks × 10 reps with chunk 2 all-failing ⇒ `combined rows: 10 of 20`, `n_replications: 10`, **audit violations: NONE**, clean exit. Mitigations that keep it MAJOR not BLOCKER: the SLURM task does exit nonzero; a *middle* chunk trips `rep_gap`; `expected_chunks` catches it when supplied. But `expected_chunks` is optional and combine is meant to be the integrity layer. **Fix:** move the provenance stamp + atomic write above the line-227 early stop — PR #48's machinery already supports an empty flagged chunk (empty chunks are exempt from the schema vote, and `partial_flags` reads provenance of all chunks). |
| **PM2** | MAJOR, CONFIRMED | **Test theater: the three "real parallel" tests silently exercise the SERIAL path under `R CMD check --as-cran`.** §5 of this design mandated cases 1/6/13 assert `n_replications >= 4`, `n_cores >= 2`, **and unset `_R_CHECK_LIMIT_CORES_`**. The first two shipped; the env-var guard did not (`grep` for `local_envvar` in `test-parallel-fail-loud.R`: 0 hits). Independently reproduced: with the var set, worker pid == main pid (`parallel actually used: FALSE`) and all 11 tests still pass. **So the `--as-cran 0/0/0` in the merge gate exercised zero parallel-path code in that file.** Local `devtools::test()` does spawn real workers, which is why the gap went unnoticed. **Fix:** `withr::local_envvar("_R_CHECK_LIMIT_CORES_" = NA)` in cases 1/6/13, plus a PID assertion so a future fallback regression fails loudly rather than silently. |

Minor findings from the same review: `R/cluster.R:773-774` still says the
`dgp_failure` detector is "Inert today" — PR #49 shipped its writer in the same
series, so the comment now misleads (a maintainer could prune a live,
load-bearing detector); §6 of this design promised `test-failure-rate.R`
invariants would be updated and that file was never touched in
`aad6d02..8ed4c1f` (its existing assertions remain arithmetically valid, so no
test is wrong — but the deviation was unrecorded until now); a scenario whose
reps are 100% DGP failures produces **no row at all** in coverage output, so
its `dgp_failure_rate = 1` never reaches the analysis tables; and the
`is.function(data_generator)` contract check sits *inside* the per-task
`tryCatch` rather than outside it as designed (loud either way).

**Resolution (2026-08-22, issue #50 — PR #51 MERGED, squash `dd0d059`; issue closed).**
PM1 and PM2 both fixed, each with a positive control proving the new assertion
can fail. PM2 refinement worth recording, found while verifying the fix: the "runs
serial under `--as-cran`" framing is true only where `NOT_CRAN=true` is also
set. Under **`devtools::check()`** -- the project's actual gate -- it is: the
tests run, and before this fix they ran serial. Under a bare
`rcmdcheck::rcmdcheck(args = "--as-cran")` `NOT_CRAN` is unset, so
`skip_on_cran()` skips all four outright (16 "On CRAN" skips observed). Either
way PM2's conclusion stands -- the parallel path had no gate coverage -- but
the mechanism differs by harness, and only the devtools path is fixable here
(skipping on true CRAN is correct policy: spawning workers is not allowed).

PM2: `pfl_parallel_config()` now clears `_R_CHECK_LIMIT_CORES_`,
pins `n_cores = 2L`, and raises the fixture to 6 reps -- 4 sat *exactly* on the
`length(tasks) < 4` boundary (tasks are per-replication, `R/runner.R:228`), so
one trimmed rep would have silently re-armed the fallback. A new PID guard
asserts worker pid != main pid and >1 distinct worker. PM1: the early `stop()`
is gone; the write path's own re-`stop()` at the end already preserved the
nonzero exit. Both MINORs closed: the "Inert today" comment is rewritten as
ARMED, and the `test-failure-rate.R` deviation is now recorded in that file's
header (no assertion there was wrong -- the accounting split changed which
column carries the reason, not the arithmetic).

A high-effort review of the PR then found **three further defects**, all in the
all-array-tasks-failed path PM1 newly makes reachable (before it, that run wrote
zero files and combine hard-stopped on "No chunk files found"). The generalizable
point: **fixing a silent skip exposes downstream code that never executed before.**
(1) MEDIUM -- the seed audit saw `attr(NULL, "medsim_schema")` as NULL and warned the
chunks came from "an older medsim", sending a user whose array all died to chase a
version mismatch that does not exist; (2) `config$n_replications` was `-Inf`;
(3) `partial_chunk` claimed reps the chunk did not hold. Fixed in `30ed22b`, guarded
by case 21 with a reverted-guard positive control. Final: 1219 pass / 0 fail,
`--as-cran` 0/0/1.

Confirmed sound by the same review: both new detectors fire on their defects
and stay silent on their negative controls; the schema vote behaves correctly
across empty / all-empty / mixed-legacy combines; coverage and `converged`
accounting are correct on mixed DGP+method-failure fixtures; the
`medsim_run_failure` payload is properly schema-v2 stamped (L5's fix is
effective); and a clean run is **bit-identical** to pre-#43 `aad6d02` — no
silent RNG or ordering drift from the widened DGP `tryCatch`.

### Adjacent work filed after this shipped

None from #43 itself. The 2026-08-22 backlog grill separately found two **new**
correctness defects in adjacent code — truth-cache poisoning and a
gauge/probmed weight-bug exposure — tracked in
[`SPEC-medsim-41-truth-integrity-2026-08-22.md`](SPEC-medsim-41-truth-integrity-2026-08-22.md)
and [`GRILL-medsim-35-gauge-probmed-exposure-2026-08-22.md`](GRILL-medsim-35-gauge-probmed-exposure-2026-08-22.md).
