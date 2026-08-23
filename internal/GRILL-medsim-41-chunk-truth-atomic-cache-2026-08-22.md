# GRILL — medsim #41: compute_truth passthrough, atomic cache writes, config$seed doc fix

**Date**: 2026-08-22 · **Target**: issue #41 · **Base**: dev @ `1150e45` (post-#43)
**Status**: **DECISION LEDGER — superseded for implementation by the SPEC.**
**Location**: decision provenance for #41. The implementation contract is
[`SPEC-medsim-41-truth-integrity-2026-08-22.md`](SPEC-medsim-41-truth-integrity-2026-08-22.md).

> ⚠️ **Read the SPEC, not this file, to build #41.** This ledger records *why*
> each decision was made; the SPEC records *what to build*. Where they differ,
> **the SPEC wins**.
>
> **GD2 / D2′ in this file is WRONG and must not be implemented.** Its
> predicate checked scenario *presence*
> (`setdiff(scenarios_with_results, scenarios_with_truth)`). A later empirical
> reproduction (SPEC §Evidence E-2) proved every scenario **is** present in
> the degenerate truth frame, so that check can never fire on the failure it
> was designed for. The real tell is **zero parameter columns**. Corrected
> predicate: SPEC §D2′. Also restored to scope after this ledger declined it:
> widening `medsim_analyze_coverage()`'s *existing whole-frame* hard stop
> (SPEC §D7) — narrower than, and not the same change as, the per-parameter
> warning GD2 rightly rejected.

**Method**: solo grounding pass (grep + targeted repro against installed
package state) → one adversarial pass via `advisor()` → interactive
`/craft:grill`, 4 branches on this issue (GD1, GD2, GD5, GD6) plus 6 grounding
findings (F1–F6). Ledger in §7 is the source of truth; where the body and the
ledger disagree, the ledger wins.

> **Read this first if you are implementing.** Two things in the original
> issue body are wrong and were corrected here: (1) truth is currently computed
> **zero** times via the chunk path, not `n_chunks` times (§0); (2) the chunk
> file write is **already atomic** as of PR #49 — only `medsim_cache_save()`
> needs it (§0). And one thing the issue does not mention at all is now the
> highest-value item in it: the truth-cache poisoning bug (F3/D5).

---

## 0. Scope confirmed live (re-grounded 2026-08-22, after #43 merged)

All three sub-defects still reproduce on current `dev`, unaffected by the
#43/PR-49 merge:

1. `medsim_run_chunk()` (`R/cluster.R:184`) calls
   `medsim_run(method, scenarios, chunk_config)` with **no `compute_truth`
   argument** — `medsim_run()` itself accepts `compute_truth = NULL`
   (`R/runner.R:128`), but the chunk entry point has no way to pass it
   through. Every one of `n_chunks` array tasks silently skips truth
   computation (since nothing is ever passed), which is actually the
   *opposite* problem from the issue's original framing ("truth computed
   n_chunks times") — see §1 correction.
2. `medsim_cache_save()` (`R/cache.R:53`, direct `saveRDS()` at `:82`) is
   still non-atomic. **Correction to the issue body**: this is a *different*
   write path than `medsim_run_chunk()`'s own chunk-file write, which
   **is already atomic** as of PR #49 (tmp-file-in-`output_dir` + checked
   `file.rename()`, `R/cluster.R:256-265`). Do not re-fix the chunk write;
   only `medsim_cache_save()` needs it.
3. `config$seed`'s roxygen (`R/config.R:24-30`) still says seed "is only
   consulted if you call `medsim_run_parallel()` directly" — false.
   `medsim_run()` calls `set.seed(config$seed)` directly at `R/runner.R:152`
   to seed the `compute_truth` pass. Confirmed by reading the line, not just
   trusting the issue text.

### Correction to the issue's premise (important)

The issue framed item 1 as "truth is computed n_chunks times instead of
once" — a **cost bug**. Re-reading `medsim_run_chunk()`'s actual call site
shows there is currently **no way to pass `compute_truth` at all**, so
truth is computed **zero times** via the chunk path today, not `n_chunks`
times. Any caller who wants truth in a chunked run must currently compute it
out-of-band and merge it in some other way (undocumented). The fix is the
same either way (add the passthrough, gate by default), but the framing
matters for the acceptance criteria in §5: there is no "n_chunks times" cost
regression to benchmark against, because that path doesn't exist yet.

---

## 1. Design

### D1 — `compute_truth` passthrough, gated by default

```r
medsim_run_chunk <- function(scenarios, method, config, verbose = TRUE,
                              code_sha = NULL, compute_truth = NULL)
```

Default gating: `compute_truth` is forwarded to `medsim_run()` only when
`config$chunk_id == 1L`; every other chunk gets `compute_truth = NULL`
regardless of what the caller passed, **unless** the caller explicitly sets
a new `config$compute_truth_every_chunk = TRUE` escape hatch (rare: only
useful when truth is cheap and chunk 1 is not trusted to survive, e.g. a
very short partition with frequent preemption).

This matches `medsim_combine_chunks()`'s existing truth-merge behavior
(`R/cluster.R:401-406`: it unions `$truth` across **all** chunks that carry
one, deduped by scenario name) — chunk 1 is the conventional producer, not
a hardcoded requirement, so the merge already tolerates a different chunk
supplying truth if the caller manually re-routes it.

### D2 — RESOLVED at GD2: `truth_coverage` violation

> The original draft of this section proposed an intent-flag design keyed on
> a `truth_requested` provenance boolean, on the premise that a truth-less
> combine degrades silently. **That premise was wrong** — see F1/F2 in the
> grill ledger (§7). The design below replaces it and is smaller.

A **structural** check on the combined object — no provenance field, no
caller-intent tracking, no new argument:

```r
scenarios_with_results <- unique(combined$results$scenario)
scenarios_with_truth   <- if (is.null(combined$truth)) character(0)
                          else unique(combined$truth$scenario)
missing <- setdiff(scenarios_with_results, scenarios_with_truth)
```

Fire a `truth_coverage` violation when `length(missing) > 0` **and**
`!is.null(combined$truth)`, routed through the existing `on_violation`
control alongside `partial_chunk`/`dgp_failure`. The non-NULL guard means a
run that legitimately requested no truth (the common case,
`compute_truth = NULL`) never fires — that case is already loudly handled
downstream by `medsim_analyze_coverage()`'s hard stop.

Why this is the right shape: it catches the cases nothing else does —
partial truth (some scenarios present), a failed-truth scenario under the
`"warn"` opt-out of D5, and a half-completed chunk-1 truth pass — because
all three present as the same observable: a scenario with results but no
truth row. The all-or-nothing case it does *not* need to catch is already
loud (F1).

### D3 — atomic `medsim_cache_save()`

Reuse PR #49's own pattern (`R/cluster.R:256-265`) rather than inventing a
second one:

```r
tmp <- file.path(dirname(file), paste0(".", basename(file), ".tmp"))
saveRDS(cache_object, tmp, compress = compress)
renamed <- file.rename(tmp, file)
if (!isTRUE(renamed)) {
  stop(sprintf("medsim_cache_save: failed to atomically rename '%s' to '%s'", tmp, file), call. = FALSE)
}
```

Tmp file lives in `dirname(file)`, not `tempdir()` — same cross-device
rationale as the chunk write. **Known, accepted gap**: the existing
`overwrite = FALSE` / `file.exists()` check (`R/cache.R:59-62`) stays
TOCTOU under concurrent array tasks (check-then-write is not atomic against
a second process racing between the check and the rename). Out of scope —
`overwrite = FALSE` is a convenience guard for interactive use, not a
concurrency primitive, and no scenario in this issue calls it from parallel
tasks.

### D4 — `config$seed` doc fix

Rewrite `R/config.R:24-30` to state both facts plainly instead of the one
false one:

```
@param seed Integer: Base seed, stored on the config and printed for
  provenance. Consulted directly by [medsim_run()] to seed the
  `compute_truth` pass (`set.seed(config$seed)` before computing ground
  truth). **Does not** determine individual replication draws -- since the
  `.medsim_det_seed()` fix, those are seeded deterministically from
  `(scenario_name, global_rep_id)`, independent of `seed`/chunk/worker/
  cluster-type. See `seed_stream` below for its deprecation.
```

Doc-only change; no behavior change.

---

## 2. Non-Goals

- Not touching the already-atomic chunk-file write (`medsim_run_chunk()`'s
  own `saveRDS`+`rename`, PR #49) — it needs no change.
- Not adding a general "does this scenario need truth" declarative flag to
  `medsim_scenario()` — D2′ is a structural check on the combined object and
  needs no scenario metadata.
- Not fixing `overwrite = FALSE`'s TOCTOU race (D3, accepted gap).
- Not tightening `medsim_analyze_coverage()`'s per-parameter "no ground truth,
  skipping" warning into an error (offered and declined at GD2 — changes
  analyze.R's contract for callers legitimately analyzing a parameter subset).
- Not emitting a separate SLURM truth job / `--dependency=afterok` array
  (declined at GD6 — changes the submit-script contract from one job to two).

---

## 3. API Summary

| Function | Change |
|---|---|
| `medsim_run_chunk()` | new `compute_truth = NULL` param, forwarded to `medsim_run()` gated by `chunk_id == 1L` (escape hatch: `config$compute_truth_every_chunk`) — GD6 |
| `medsim_run()` | new `truth_on_failure = c("stop", "warn")`, default `"stop"` — **breaking**, GD5 |
| `medsim_combine_chunks()` | new `truth_coverage` violation under the existing `on_violation` — GD2/D2′ |
| `medsim_cache_save()` | atomic tmp+rename write — D3 |
| truth cache read/write (`R/runner.R:679-707`) | never cache a failure sentinel; treat a cached sentinel as a miss so existing poisoned files self-heal — GD1/D5 |
| `medsim_config()` roxygen | `seed` doc corrected — D4 |

---

## 4. Verification (planted-defect matrix)

Each case must FAIL before and PASS after.

| # | Defect ⇒ expected | Negative control |
|---|---|---|
| 1 | `medsim_run_chunk(..., compute_truth = fn)` on `chunk_id = 1` → chunk's `$truth` non-NULL | `chunk_id = 2`, same call → `$truth` NULL (not recomputed), unless `compute_truth_every_chunk = TRUE` |
| 2 | Truth present for scenario A but absent for B while both have results → combine flags `truth_coverage` | all scenarios have truth → silent; **and** `compute_truth = NULL` run with `$truth = NULL` → silent (must NOT fire — the common case) |
| 3 | `medsim_cache_save()` fails between tmp-write and rename → target file untouched, no torn file | normal save → target written, tmp gone |
| 4 | mocked `file.rename` returning `FALSE` → `medsim_cache_save()` stops with the same diagnostic shape as `medsim_run_chunk()`'s | same-device rename → succeeds silently |
| 5 | `?medsim_config`'s rendered `seed` text matches D4 (grep-based doc-drift guard) | — |
| 6 | **GD1 core:** `truth_function` that errors → nothing written to the truth cache; re-running retries instead of loading garbage | successful truth → cached normally |
| 7 | **GD1 self-heal:** a pre-existing cache file already holding a failure sentinel → treated as a miss and recomputed, not loaded | valid cached truth → loaded from cache as today |
| 8 | **GD5:** `truth_function` errors → `medsim_run()` **stops** by default | `truth_on_failure = "warn"` → warns, run continues, and the failed scenario is **absent** from `$truth` (not present-and-bogus) — which case 2 then catches at combine |
| 9 | Sentinel detection is class-based, not name-based: a truth function legitimately returning a parameter named `error` is **not** misread as a failure | — |

Gates: `devtools::test()` in-tree (baseline **1197 / 0 fail / 0 err / 1 skip**,
the post-#43 `dev` state — check testthat's **error** column, not just
pass/fail), `R CMD check --as-cran` against the 0/0/0 baseline, then
independent adversarial review of the branch diff before integration (per
`e2e-before-pr.md`; `pre-pr-testing.md`'s tier table puts a code change at
full-suite, not a subset).

---

## 5. Branch / Worktree Plan (descriptive — not an action item)

Single feature branch off `dev`, e.g. `feature/truth-integrity`. Code change
across `runner.R` / `cache.R` / `cluster.R` ⇒ a worktree is required by this
repo's own convention (`spec-only-mode.md`: no feature code on `dev`).

Natural increments, in dependency order:

1. **GD1/D5 + GD5** (`runner.R`, `cache.R`) — the correctness core: stop
   caching sentinels, self-heal poisoned caches, `stop()` on truth failure.
   Shippable alone and the highest-value slice.
2. **D3 + D4** (`cache.R`, `config.R`) — atomic write + seed doc. Independent
   one-file changes; can ride with 1.
3. **D1 + D2′** (`cluster.R`) — passthrough + `truth_coverage` violation.
   Ship together: the passthrough makes truth reachable on the cluster path,
   the coverage check is what keeps it honest.

**Breaking-change note:** increment 1 changes `medsim_run()`'s contract
(`stop()` where it previously warned). NEWS entry required; consider whether
it rides `dev` or waits for a minor bump.

---

## 7. Grill ledger (interactive, 2026-08-22)

### Pre-answer sweep findings (re-grounded during the grill)

| # | Finding | Effect on this doc |
|---|---|---|
| F1 | `medsim_analyze_coverage()` **hard-stops** on missing truth (`R/analyze.R:82-84`) and is the **only** `$truth` consumer. A truth-less combine is NOT silent — it fails loud, just late (analysis time, not combine time). | **§1 D2's stated rationale was wrong.** The gate's value drops from "prevents silent data loss" to "fails at minute 5 instead of hour 6". |
| F2 | A chunk-1 *total* failure is already caught by Gate A.1 contiguity (missing reps ⇒ id gap). | The main loss path D2 was designed for is largely pre-covered. |
| F3 | **NEW BUG (silent-wrong-data).** `R/runner.R:696-707`: a failed `truth_function()` is downgraded to a `warning()`, and the sentinel `list(error = e$message)` is then **written to the truth cache** via `medsim_cache_save()`. Every later run loads it ("Loaded from cache") with **no warning at all**, and builds a truth column literally named `error`; `medsim_analyze_coverage()` then warns "No ground truth for parameter X, skipping" per parameter and drops them. The fingerprint guard does not help — it invalidates on scenario *content* change, not on the cached value being a failure sentinel. | Promoted into this issue as **item 4** (D5 below). |

### Decisions

| # | Question | Resolution |
|---|---|---|
| GD1 | Where does the F3 truth-cache poisoning bug get handled? | **Fold into #41 as item 4.** `medsim_cache_save()` is already being opened here for the atomic-write fix (D3), the guard is ~3 lines, and shipping atomicity while leaving poison-caching in place is half a fix. Consequence accepted: #41 is no longer a cost/doc cleanup — it is a correctness PR, and its priority rises above #29/#28. |
| GD2 | Does D2 (combine-time truth gate) survive F1/F2? | **Yes, reframed as a truth-coverage check** — but the predicate recorded here was later proven wrong. ⚠️ **SUPERSEDED by SPEC §D2′** (scenario-presence never fires; the tell is zero parameter columns — see SPEC §Evidence E-2). The conclusion "a combine-time gate is warranted" stands; the implementation does not. |
| GD5 | What should a failed `truth_function()` do, once the sentinel is no longer cached? | **`stop()` by default** (breaking; opt-out via a new `truth_on_failure = c("stop","warn")` on `medsim_run()`). Rationale in D5 below — truth is Step 1, so stopping wastes no replication compute. Rejected: keep-warning-and-exclude (standalone runs still degrade quietly), data-carrying `medsim_truth_failure` condition (cleanest architecturally but adds a fourth condition class for a Step-1 abort that loses no data), threading `on_violation` into the runner (too wide for this issue). |
| GD6 | How is `compute_truth` scheduled across array tasks, given F5's straggler? | **Gate on `chunk_id == 1L` (D1 as written) AND document precompute-before-submit as the preferred production pattern** in #28's article. Rejected: precompute-only (kills #41's headline item for no gain on small runs), every-chunk-computes (cold cache ⇒ N× the expensive work — the waste the issue was filed about), SLURM job dependency (cleanest but changes the submit-script contract from one job to two, well past #41's scope). |

### F5 — chunk-1 straggler (found while grilling GD6)

Gating truth on `chunk_id == 1L` means chunk 1 does *truth + its replications*
while chunks 2…N do replications only. Default `n_truth` is 10000
(`R/runner.R:693`), so the imbalance is not negligible. This collides with
**#28's own guidance** to size `--time` **per array task** from a pilot: a
uniform walltime calibrated on chunks 2…N can kill chunk 1 — the only task
carrying truth. The two docs must not contradict each other:

- **#41** (this doc): D1 gates on chunk 1; the straggler is named as a known
  cost with precompute offered as the way out.
- **#28** (article): the per-task `--time` rule must carry an explicit
  exception — *if you use the chunk-1 truth gate, size chunk 1's walltime for
  truth + reps, or precompute truth and skip the problem.*

Preferred production pattern to document (medsim's fingerprint-guarded cache
already supports it, no new code needed): compute truth once before submitting
the array so the cache is warm, then submit every task with
`compute_truth = NULL`. Balanced tasks, no straggler, no duplicated work.

### F4 — the gap that justifies D2′ (found while grilling GD2)

`medsim_analyze_coverage()`'s hard-stop (F1) fires **only** on
`is.null(results$truth) || nrow(results$truth) == 0`. A truth frame that is
**non-empty but wrong** — the shape a failure sentinel produces, carrying a
column named `error` and no real parameter columns — passes that guard and
degrades to a per-parameter `warning("No ground truth for parameter X, skipping")`
(`R/analyze.R:124-126`) and, in the by-scenario branch, to a bare `next`
(`R/analyze.R:195`) with no diagnostic at all. Partial truth (some scenarios
present, others missing) fails the same way. So "truth is loudly checked" is
true only for the all-or-nothing case.

### D2′ — truth-coverage violation (replaces the intent-flag design)

At combine time, a **structural** check on the combined object — no new
provenance field, no caller-intent tracking:

```
scenarios_with_results <- unique(combined$results$scenario)
scenarios_with_truth   <- if (is.null(combined$truth)) character(0)
                          else unique(combined$truth$scenario)
missing <- setdiff(scenarios_with_results, scenarios_with_truth)
```

`length(missing) > 0` **and** `!is.null(combined$truth)` ⇒ `truth_coverage`
violation, routed through the existing `on_violation` control alongside
`partial_chunk`/`dgp_failure`. Guarded on non-NULL truth so that a run which
legitimately requested no truth at all (the overwhelmingly common case:
`compute_truth = NULL`) never fires — that case stays the caller's business
and remains loudly handled downstream by F1 if they then ask for coverage.

This subsumes what the intent flag was for, at less cost: partial-chunk truth,
failed-truth scenarios, and a chunk-1 truth pass that half-completed all
present as the same observable — a scenario with results but no truth row.

**Not** taken (offered and declined at GD2): tightening
`medsim_analyze_coverage()`'s per-parameter warning into an error. That would
close the same hole at the consumer end but changes analyze.R's contract for
callers who legitimately analyze a parameter subset. Left as a possible
follow-up, out of scope here.

### D5 — never cache a failure sentinel (item 4, from GD1)

Two-sided guard, because either side alone leaves a hole:

- **Write side** (`R/runner.R:696-707`): do not call `medsim_cache_save()` when
  `truth_result` is a failure sentinel. A failed truth computation must leave
  the cache untouched so the next run retries instead of inheriting garbage.
- **Read side** (`R/runner.R:679-686` / `medsim_cache_load()`): treat a loaded
  value that is a failure sentinel as a **cache miss**, so pre-existing
  poisoned cache files on disk self-heal rather than needing manual deletion.
- **Escalation — resolved at GD5: `stop()` by default.** The decisive fact is
  structural: truth is computed in **Step 1** (`R/runner.R:176`), *before*
  Step 2 runs any replications (`:199`). A failure there wastes **zero**
  replication compute, so the usual "don't kill a long run over one flaky
  scenario" argument does not apply — and continuing instead produces a run
  whose accuracy analysis is silently degraded (bogus truth column ⇒
  per-parameter "skipping" warnings at `R/analyze.R:124-126`). Failing at
  second 5 strictly dominates failing at hour 6.

  **This is a breaking change** and must be treated as one: NEWS entry, and
  an explicit opt-out for callers who genuinely want partial-truth runs
  (recommended spelling: a `truth_on_failure = c("stop", "warn")` argument on
  `medsim_run()`, defaulting to `"stop"` — narrower than threading the
  combine path's `on_violation` all the way into the runner, which GD5
  explicitly declined as too wide for this issue).

  With `stop()` as the default, the sentinel never reaches `truth_df` on the
  default path, so the "exclude sentinel from truth_df" work is only needed
  on the `"warn"` branch — where the failed scenario must be **absent** from
  `$truth` rather than present-and-bogus, so that D2′ can see it.

Sentinel detection must be precise: `is.list(x) && identical(names(x), "error")`
is the current shape, but a legitimate truth function returning a parameter
genuinely named `error` would be misread. Prefer stamping an explicit class on
the sentinel (`structure(list(error = ...), class = "medsim_truth_failure")`)
and keying the guard on `inherits()`, not on the name.

---

## 6. Open questions

**None blocking.** All six grill branches (GD1, GD2, GD5, GD6 here; GD3/GD4
belong to #29/#28) are resolved — see §7. The design is implementation-ready.

Two deliberate deferrals, recorded so they are not re-litigated:

1. Tightening `medsim_analyze_coverage()`'s per-parameter "no ground truth,
   skipping" warning into an error (declined at GD2 — consumer-side contract
   change, separate work).
2. A SLURM truth job with `--dependency=afterok` on the array (declined at
   GD6 — architecturally cleanest but changes the submit-script contract).

One item needs a call **at implementation time, not now**: whether increment 1's
breaking `stop()` rides `dev` as-is or waits for a minor version bump.
