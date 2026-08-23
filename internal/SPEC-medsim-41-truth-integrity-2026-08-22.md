# SPEC: Ground-truth integrity — fail loud, never cache a failure, never report an empty analysis as complete

- **Issue:** #41 (rescoped — see Problem)
- **Status:** **PROPOSAL — implementation-ready.** No open design questions.
- **Created:** 2026-08-22
- **Author:** medsim maintainer (grounded from code trace + 3 empirical reproductions; hardened by one adversarial pass)
- **Refs:** #34 (Gate A/`on_violation` architecture this reuses), #43 (`medsim_run_failure` precedent for fail-loud), #27 (truth-cache fingerprint guard — the mechanism that *fails* to help here), PR #49 (the atomic-write pattern this copies)
- **Companion ledger:** [`GRILL-medsim-41-chunk-truth-atomic-cache-2026-08-22.md`](GRILL-medsim-41-chunk-truth-atomic-cache-2026-08-22.md) — decision provenance (GD1, GD2, GD5, GD6; findings F1–F6). **This SPEC is authoritative for implementation**; where the two differ, this file wins. GD2 in that ledger is **superseded** by §D2′ here.

---

## Problem

`medsim_run()`'s ground-truth pass has three defects that compound into a
silent-wrong-data failure — the same class the #34/#43 gate lineage exists to
eliminate, one layer up. All three were **empirically reproduced** against
`dev` @ `f4c8dfc` on 2026-08-22; transcripts in §Evidence.

| # | Defect | Location | Observed behavior |
|---|---|---|---|
| P-1 | A failed `truth_function()` is downgraded to a `warning()` and its error sentinel `list(error = <msg>)` is **written to the truth cache** | `R/runner.R:696-707` | Poison persists on disk; every later run with the same fingerprint loads it |
| P-2 | The sentinel is silently dropped when building `truth_df` — `is.numeric(value)` is FALSE for a character message — producing a row with **`scenario` only and zero parameter columns** | `R/runner.R:720-733` | All-failed ⇒ non-empty, non-NULL truth frame carrying no parameters |
| P-3 | `medsim_analyze_coverage()`'s truth guard tests only `is.null(truth) \|\| nrow(truth) == 0` | `R/analyze.R:82-84` | The P-2 frame **passes** the guard; every parameter is then skipped with a warning and an empty analysis is returned as a success |

Plus two independent, smaller items carried from the original issue:

| # | Defect | Location |
|---|---|---|
| P-4 | `medsim_cache_save()` writes with a bare `saveRDS()` — non-atomic; concurrent SLURM array tasks can leave a torn file | `R/cache.R:53-86` |
| P-5 | `?medsim_config`'s `seed` doc is factually wrong — claims `seed` is "only consulted if you call `medsim_run_parallel()` directly", but `medsim_run()` calls `set.seed(config$seed)` for the truth pass at `R/runner.R:152` | `R/config.R:24-30` |

And one API gap:

| # | Gap | Location |
|---|---|---|
| P-6 | `medsim_run_chunk()` has no `compute_truth` passthrough, so ground truth is reachable **zero** times via the chunk path | `R/cluster.R:184` |

### Corrections to the issue body

The GitHub issue is stale on two points; do not implement from it:

1. It says truth is "computed n_chunks times instead of once" — a cost bug.
   **False.** There is no passthrough at all (P-6), so truth is computed
   **zero** times via the chunk path.
2. It says cache writes are non-atomic and implies the chunk write too.
   **The chunk write is already atomic** as of PR #49 (`R/cluster.R:256-265`,
   tmp-in-`output_dir` + checked `file.rename`). Only `medsim_cache_save()`
   needs it (P-4).

Neither the issue nor the original design doc mentions P-1/P-2/P-3 at all —
they are the highest-value content here and were found during the grill.

---

## Evidence (empirical, 2026-08-22)

### E-1 — transient failure permanently poisons the cache (the headline)

A `truth_function` that fails on call 1 and would succeed on call 2, held in
one function object so the fingerprint is unchanged between runs:

```
###### RUN 1 (truth fails transiently) ######
truth after run 1:
  scenario
1       S1

###### RUN 2 (same fn; would now SUCCEED if called) ######
truth after run 2:
  scenario
1       S1
truth_function call count: 1   (2 = recomputed, 1 = cache poison reused)

###### coverage analysis on run 2 ######
   WARN: No ground truth for parameter 'indirect', skipping

Coverage Analysis
=================
Summary:
  Scenarios:         1
  Parameters:        0
  Overall Coverage:  NA
Coverage Rates:
NULL
```

Run 2 called the truth function **zero times**, `medsim_run()` emitted **no
warning at all**, and `medsim_analyze_coverage()` **returned successfully**
with zero parameters. Nothing anywhere reports that ground truth failed.

Note the fingerprint guard (#27) does not save this: it invalidates on
scenario/truth-function *content* change. A transient failure leaves content
identical, so the poison is served indefinitely.

### E-2 — all-scenarios-failed completes and passes the analyze guard

```
run COMPLETED. truth:
  scenario
1       S1
2       S2
is.null(truth): FALSE  nrow: 2  passes analyze.R:82 guard: TRUE
```

### E-3 — mixed failure crashes with an opaque message

One scenario's truth fails, another succeeds ⇒ `do.call(rbind, truth_list)`
receives frames of differing width:

```
RUN ERROR: numbers of columns of arguments do not match
```

Loud, but it names neither the scenario nor truth — and both cache files were
**already poisoned before the crash**.

---

## Architecture decision (locked): one primary fix, two defensive layers

The three failures share one root cause — a failed truth computation is
allowed to continue as data. Fix it at the source; keep the gates as
defense-in-depth for the paths that deliberately continue.

```
LAYER 1 (primary)   stop() at Step 1 on truth failure        <- kills P-1/P-2/P-3 at the root
                    + never cache a sentinel
                    + treat a cached sentinel as a miss       <- heals already-poisoned caches
        |
LAYER 2 (residual)  truth_coverage violation at combine       <- covers ONLY the two paths that
        |                                                        survive Layer 1 (see D2')
LAYER 3 (consumer)  widen analyze.R's EXISTING hard stop      <- last line of defense
```

**Do not over-build Layer 2.** With Layer 1 in place a failed truth aborts
before the degenerate frame is ever built, cached, or combined. Layer 2 has
exactly two live triggers: (a) the `truth_on_failure = "warn"` opt-out, and
(b) a chunked run where chunk 1's truth is absent.

### Why Layer 1 is `stop()` and not a warning

Ground truth is computed in **Step 1** (`R/runner.R:176`), *before* Step 2
runs any replications (`:199`). Aborting there wastes **zero** replication
compute — the usual "don't kill a long run over one scenario" argument does
not apply. Failing at second 5 strictly dominates failing at hour 6, or worse,
not failing at all (E-1).

### Blast radius is narrower than "breaking change" implies

Multi-scenario mixed failures **already hard-crash today** (E-3). The only
behavior that changes from *succeeding* to *failing* is the all-failed case —
which includes any single-scenario run whose truth fails. Framed honestly:
**this converts one opaque crash and one silent success into a single clear
error.** That framing should drive the version-bump call (see Risks).

---

## Goals

1. A failed `truth_function()` never becomes data: it aborts by default, and
   never reaches the cache.
2. Already-poisoned caches on disk self-heal without manual deletion.
3. A truth frame carrying no parameters is never reported as a completed
   analysis.
4. Ground truth is reachable from the chunk path, without making chunk 1 a
   silent straggler.
5. Cache writes are atomic.
6. `?medsim_config`'s `seed` documentation is true.

## Non-Goals

- Converting `medsim_analyze_coverage()`'s **per-parameter** "no ground truth,
  skipping" warning into an error (declined at GD2 — a contract change for
  callers legitimately analyzing a parameter subset). Widening the *existing
  whole-frame* hard stop is a different, narrower change and **is** in scope
  (Layer 3).
- A separate SLURM truth job with `--dependency=afterok` (declined at GD6 —
  changes the submit-script contract from one job to two).
- A declarative "this scenario requires truth" flag on `medsim_scenario()`.
- Fixing `overwrite = FALSE`'s TOCTOU race (see Risks).
- Re-measuring the gauge percentile-bootstrap arm — that is #35.

---

## Design

Dependency order:

```
D5 sentinel guard  ──┐
D6 stop() default  ──┼──► (Layer 1: independent of everything else, ships alone)
D3 atomic cache    ──┘
D2' truth_coverage ─────► needs D6's "absent, not degenerate" rule
D7 analyze guard   ─────► independent (Layer 3)
D1 chunk passthrough ───► independent; pairs with D2' for the cluster story
D4 seed doc        ─────► independent, doc-only
```

### D5 — never cache a failure sentinel; heal what is already cached

Make the sentinel explicitly typed rather than shape-sniffed. A truth
function may legitimately return a parameter named `error`, so
`identical(names(x), "error")` is not a safe test:

```r
# in the tryCatch handler at R/runner.R:696-703
error = function(e) {
  structure(list(error = conditionMessage(e)),
            class = "medsim_truth_failure")
}
```

- **Write side** (`R/runner.R:706`): skip `medsim_cache_save()` entirely when
  `inherits(truth_result, "medsim_truth_failure")`. A failed computation must
  leave the cache untouched so the next run retries.
- **Read side** (`R/runner.R:678-685`): treat a loaded value satisfying
  `inherits(cached$truth, "medsim_truth_failure")` as a **cache miss**
  (`truth_value <- NULL`), so pre-existing poisoned files recompute instead of
  being served. This is what makes E-1's scenario self-heal on upgrade.

### D6 — `truth_on_failure = c("stop", "warn")`, default `"stop"`

New argument on `medsim_run()`, threaded to `medsim_compute_all_truth()`.

- **`"stop"` (default):** raise immediately, naming the scenario and the
  original condition message. Use a data-carrying condition consistent with
  the package's vocabulary — class
  `c("medsim_truth_failure_error", "medsim_error", "error", "condition")`
  carrying `$scenario` and `$parent` — so callers can `tryCatch` it the same
  way they do `medsim_run_failure` / `medsim_combine_violation`.
- **`"warn"` (opt-out):** warn as today, **and omit the failed scenario's row
  from `truth_df` entirely.** The scenario must be **absent**, not
  present-with-fewer-columns.

> **Implementer note — this rule is what prevents E-3's opaque crash.**
> Keeping a stub row for a failed scenario is what makes
> `do.call(rbind, truth_list)` receive frames of differing width and fail with
> `numbers of columns of arguments do not match`. Drop the element from
> `truth_list` (or `Filter(Negate(is.null), ...)` before the rbind). An
> implementer who "helpfully" keeps the row reintroduces E-3.

Absence is also precisely the observable D2′ keys on — the two decisions are
deliberately coupled.

### D2′ — `truth_coverage` violation at combine (supersedes ledger GD2)

> **GD2's original predicate was wrong and must not be implemented.** It
> checked `setdiff(scenarios_with_results, scenarios_with_truth)` — scenario
> *presence*. E-2 proves both scenarios are present in the degenerate frame,
> so that check can never fire on the failure it was designed for. The tell is
> **zero parameter columns**, not a missing scenario.

Two distinct conditions, both guarded on a non-NULL truth frame:

```r
if (!is.null(combined$truth)) {
  truth_params <- setdiff(names(combined$truth), "scenario")

  # (a) degenerate frame: rows exist but carry no parameters at all
  if (length(truth_params) == 0L) <violation: truth_coverage/degenerate>

  # (b) a scenario has results but no truth row
  missing <- setdiff(unique(combined$results$scenario),
                     unique(combined$truth$scenario))
  if (length(missing) > 0L) <violation: truth_coverage/missing>
}
```

Routed through the existing `on_violation = c("stop","warn","ignore")` control
alongside `partial_chunk` / `dgp_failure`. The non-NULL guard keeps the common
`compute_truth = NULL` run silent.

Live triggers after D6: the `"warn"` opt-out, and a chunked run where chunk
1's truth is absent. That is the whole remit — spec'd as defense-in-depth, not
as the main guard.

### D7 — widen `medsim_analyze_coverage()`'s existing hard stop (Layer 3)

`R/analyze.R:82`, from:

```r
if (is.null(results$truth) || nrow(results$truth) == 0) {
```

to additionally catch the degenerate frame:

```r
truth_params <- setdiff(names(results$truth %||% data.frame()), "scenario")
if (is.null(results$truth) || nrow(results$truth) == 0 ||
    length(truth_params) == 0L) {
  stop("Cannot compute accuracy metrics: no ground truth available. ",
       "The truth table has no parameter columns -- this usually means every ",
       "compute_truth() call failed. Re-run with truth_on_failure = 'stop' ",
       "(the default) to see the original error.")
}
```

This is **not** the change GD2 declined: it widens an already-loud whole-frame
guard to cover a case E-2 proves it misses. The per-parameter warning is
untouched, so subset analyses keep working.

### D1 — `compute_truth` passthrough on `medsim_run_chunk()`

```r
medsim_run_chunk(scenarios, method, config, verbose = TRUE,
                 code_sha = NULL, compute_truth = NULL)
```

Forwarded to `medsim_run()` **only when `config$chunk_id == 1L`**; other
chunks receive `NULL` regardless. Escape hatch:
`config$compute_truth_every_chunk = TRUE`.

Rationale and the trade-off accepted (GD6/F5): chunk 1 then does truth + its
replications and becomes a systematic straggler, with default `n_truth =
10000` (`R/runner.R:693`). This collides with #28's rule to size `--time`
**per array task** from a pilot. Both docs must state the exception, and #28's
article documents the preferred production pattern — precompute truth once
before submitting so the cache is warm, then submit every task with
`compute_truth = NULL` (balanced tasks, no straggler, no duplicated work).
The passthrough is the convenience path for small runs, not the recommendation.

### D3 — atomic `medsim_cache_save()`

Copy PR #49's pattern verbatim rather than inventing a second one:

```r
tmp <- file.path(dirname(file), paste0(".", basename(file), ".tmp"))
saveRDS(cache_object, tmp, compress = compress)
if (!isTRUE(file.rename(tmp, file))) {
  stop(sprintf(
    "medsim_cache_save: failed to atomically rename '%s' to '%s' (cross-device rename? tmp and target must share a filesystem)",
    tmp, file), call. = FALSE)
}
```

Tmp lives in `dirname(file)`, never `tempdir()` — on HPC `tempdir()` is
typically a different filesystem and a cross-device `file.rename()` returns
`FALSE` rather than erroring.

### D4 — `config$seed` doc fix (doc-only)

`R/config.R:24-30` — state both facts instead of the one false one:

```
@param seed Integer: Base seed, stored on the config and printed for
  provenance. Consulted directly by [medsim_run()] to seed the
  `compute_truth` pass (`set.seed(config$seed)` before computing ground
  truth). **Does not** determine individual replication draws -- since the
  `.medsim_det_seed()` fix, those are seeded deterministically from
  `(scenario_name, global_rep_id)`, independent of `seed`/chunk/worker/
  cluster-type. See `seed_stream` below for its deprecation.
```

---

## API Summary

| Function | Change |
|---|---|
| `medsim_run()` | NEW `truth_on_failure = c("stop", "warn")`, default `"stop"` — **breaking** (D6) |
| `medsim_compute_all_truth()` | honors `truth_on_failure`; sentinel typed `medsim_truth_failure`; failed scenarios **omitted** from `truth_df`; never cached (D5/D6) |
| `medsim_cache_load()` / truth read path | a cached `medsim_truth_failure` is a **miss** (D5) |
| `medsim_cache_save()` | atomic tmp + checked rename (D3) |
| `medsim_run_chunk()` | NEW `compute_truth = NULL`, gated to `chunk_id == 1L`; `config$compute_truth_every_chunk` escape (D1) |
| `medsim_combine_chunks()` / `medsim_audit_results()` | NEW `truth_coverage` violation under `on_violation` (D2′) |
| `medsim_analyze_coverage()` | existing whole-frame hard stop widened to catch a parameter-less truth table (D7) |
| `medsim_config()` roxygen | `seed` corrected (D4) |
| NEW condition class | `medsim_truth_failure_error` (subclasses `medsim_error`) |

---

## Acceptance Criteria — planted-defect matrix

Each case must FAIL before and PASS after.

| # | Planted defect ⇒ required behavior | Negative control ⇒ must stay silent |
|---|---|---|
| 1 | **E-1 verbatim.** Transient truth failure (fails call 1, succeeds call 2, *same function object*): run 1 stops; after the fix is applied, a re-run **recomputes**. Assert on the **call counter** (`attempts == 2`), not just output — the counter is what proves the cache was not reused | successful truth ⇒ cached, and a second run loads from cache with `attempts == 1` (caching still works) |
| 2 | A failed truth computation leaves **no** cache file for that scenario | successful truth ⇒ file written |
| 3 | A pre-existing cache file holding a `medsim_truth_failure` ⇒ treated as a miss and recomputed (self-heal) | a valid cached truth ⇒ loaded, not recomputed |
| 4 | **E-2.** All scenarios' truth fails ⇒ `medsim_run()` **stops** with a message naming the scenario and the original error | — |
| 5 | **E-3.** Mixed failure with `truth_on_failure = "warn"` ⇒ run completes, failed scenario **absent** from `$truth`, and **no** `numbers of columns of arguments do not match` | all truth succeeds ⇒ every scenario present |
| 6 | `truth_on_failure = "warn"` + a scenario absent from truth ⇒ combine raises `truth_coverage/missing` | all scenarios have truth ⇒ silent |
| 7 | A truth frame with rows but zero parameter columns ⇒ combine raises `truth_coverage/degenerate` | `compute_truth = NULL` run (`$truth` NULL) ⇒ **silent** (must NOT fire — the common case) |
| 8 | **D7.** `medsim_analyze_coverage()` on a parameter-less truth frame ⇒ **stops** with the new message | truth with ≥1 parameter, analyzing a subset of parameters ⇒ still only warns per missing parameter (contract preserved) |
| 9 | Sentinel detection is class-based: a truth function legitimately returning a numeric parameter named `error` is **not** treated as a failure | — |
| 10 | `medsim_run_chunk(compute_truth = fn)` on `chunk_id = 1` ⇒ `$truth` non-NULL | `chunk_id = 2`, same call ⇒ `$truth` NULL, unless `compute_truth_every_chunk = TRUE` |
| 11 | `medsim_cache_save()` failing between tmp-write and rename ⇒ target untouched, no torn file; mocked `file.rename` returning `FALSE` ⇒ informative stop | normal save ⇒ target written, tmp gone |
| 12 | `?medsim_config`'s rendered `seed` text matches D4 (grep-based doc-drift guard) | — |
| 13 | **Negative control:** a clean run with successful truth is unchanged end-to-end vs pre-fix (`$truth`, `$results`, coverage output identical) | — |

**Gates:** `devtools::test()` **in the worktree the PR ships from** (baseline
**1197 pass / 0 fail / 0 error / 1 skip** on `dev` @ `f4c8dfc` — check
testthat's *error* column, not just pass/fail); `R CMD check --as-cran`
against the **0/0/0** baseline; then independent adversarial review of the
branch diff before integration.

**E2E (per `e2e-before-pr.md`):** case 1 is the E2E — it is a live exercise
with a real failure mode, and the PR body must quote its actual transcript
(both the pre-fix `call count: 1` and the post-fix `call count: 2`).

---

## Risks

1. **Breaking change.** Mitigated by scope: E-3 shows multi-scenario mixed
   failures already crash, so only the all-failed case flips from silent
   success to loud failure. NEWS entry required; `truth_on_failure = "warn"`
   restores prior behavior for anyone who needs it.
2. **Version-bump call deferred to implementation.** Whether Layer 1 rides
   `dev` or waits for a minor bump. The narrow blast radius above argues it
   can ride `dev`.
3. **Sentinel misdetection.** Addressed by the explicit S3 class (D5) — never
   sniff `names(x)`; case 9 pins it.
4. **Chunk-1 straggler** (D1/F5) — documented in both this SPEC and #28's
   article; precompute is the recommended production path.
5. **`overwrite = FALSE` stays TOCTOU** under concurrent tasks (check-then-
   write is not atomic against a racing process). Accepted, out of scope:
   it is an interactive convenience guard, not a concurrency primitive.
6. **D7 could surprise a caller** who currently gets an empty coverage table
   and treats it as "no parameters to check". Case 8's negative control keeps
   legitimate subset analyses working; the empty-table case is exactly the
   bug being fixed.

---

## Branch / Worktree Plan (descriptive — not an action item)

Feature worktree off `dev` — code across `runner.R` / `cache.R` / `cluster.R`
/ `analyze.R` / `config.R`, and this repo forbids feature code on `dev`.

Natural increments, shippable in this order:

1. **Layer 1 — D5 + D6** (`runner.R`, `cache.R`): the correctness core. Highest
   value, shippable alone, and it is what E-1 tests.
2. **Layer 3 — D7** (`analyze.R`): one predicate, one message. Cheap, and
   independently valuable.
3. **D3 + D4** (`cache.R`, `config.R`): atomic write + seed doc.
4. **D1 + D2′** (`cluster.R`): passthrough + `truth_coverage`. Ship together —
   the passthrough makes truth reachable on the cluster path, the violation is
   what keeps it honest.

---

## Implementation log

_(empty — append one line per landed increment, per the #34 SPEC's convention)_

---

**Grounded:** code trace + 3 empirical reproductions (E-1/E-2/E-3), 2026-08-22.
**Grilled:** [`GRILL-medsim-41-chunk-truth-atomic-cache-2026-08-22.md`](GRILL-medsim-41-chunk-truth-atomic-cache-2026-08-22.md) (GD1/GD2/GD5/GD6, F1–F6).
**Reviewed:** one adversarial pass, 2026-08-22 — corrected D2′'s predicate (GD2 superseded), restored D7 to scope, and pinned the "absent, not degenerate" rule as E-3's fix.
