# SPEC: Ground-truth integrity — fail loud, never cache a failure, never report an empty analysis as complete

- **Issue:** #41 (rescoped — see Problem)
- **Status:** **IMPLEMENTED 2026-10-14 (`feature/truth-integrity`); see Implementation log.** Was: PROPOSAL — v2, amended after adversarial review (2026-08-22). Implementation-ready. v1 shipped a **blocker** (a self-heal that healed nothing) and a **wrong-function** guard target; both fixed below. Amendments R1–R9 are marked inline.
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

Loud, but it names neither the scenario nor truth — and both cache files are
**written before the crash** (the failing scenario's holds poison; the
succeeding one holds valid truth).

> **Correction (AMENDMENT R7):** the original text said "both cache files were
> already poisoned". A reviewer's independent repro showed only the *failing*
> scenario's file is poisoned; the succeeding scenario's cache is valid. The
> point that survives is that a poisoned file is committed to disk before the
> run dies, so a retry inherits it.

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

### Blast radius — corrected by AMENDMENT R3

Multi-scenario mixed failures **already hard-crash today** (E-3), so those are
not a behavior change. Two classes of run **do** flip from succeeding to
failing:

1. **All-failed truth** (including any single-scenario run whose truth fails) —
   from silent empty-success to a clear error. This is the intended fix.
2. **Successful but wrongly-shaped truth** (D8: a bare named vector, or list
   elements of length ≠ 1) — from silent empty-success to a clear error.
   ⚠️ **The original spec claimed the all-failed case was the *only* flip.
   That was wrong.** This second class is invisible to D5/D6 because nothing
   fails; it is caught by D7/D8. Users whose truth functions have quietly been
   contributing nothing will now be told so — correct, but it *will* surface
   pre-existing latent breakage in working pipelines, which is a louder
   migration story than "one edge case".

Honest framing: **this converts one opaque crash and two silent successes into
clear, actionable errors** — and may reveal that some existing runs never had
usable ground truth at all.

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
- **Read side** (`R/runner.R:678-685`): treat a loaded sentinel as a **cache
  miss** (`truth_value <- NULL`). ⚠️ **The class test alone is not sufficient —
  see below.**

> **⚠️ AMENDMENT R1 (adversarial review, 2026-08-22) — the original read-side
> guard healed nothing.** Two independent reviewers confirmed this. Poison
> already on disk was written by *today's* code as an **unclassed**
> `list(error = <msg>)`. A guard testing only
> `inherits(cached$truth, "medsim_truth_failure")` matches solely the **new**
> classed sentinel — which the write side above guarantees is **never written
> to disk**. So the class check was near-dead code, Goal 2 was unmet, and
> acceptance Case 3 tested a disk state that can never occur.
>
> **Required read-side guard — both shapes:**
>
> ```r
> .medsim_is_truth_sentinel <- function(x) {
>   inherits(x, "medsim_truth_failure") ||                    # new, classed
>   (is.list(x) && identical(names(x), "error") &&            # legacy, unclassed
>    is.character(x$error))
> }
> ```
>
> The legacy arm keys on `is.character(x$error)`, so it cannot collide with
> Case 9's legitimate **numeric** parameter named `error` — the spec's own
> objection to name-sniffing applies to the *write* side, not here.
>
> **Alternative (also acceptable, arguably cleaner):** version-salt
> `.medsim_truth_fingerprint()` so every pre-#41 cache entry misses once.
> Costs one recompute of valid caches; needs no shape heuristic at all. Pick
> one and say which in the implementation log.
>
> **Case 3's fixture must plant the LEGACY UNCLASSED shape** — a classed
> fixture tests an artifact that will never exist in the field.

### D6 — `truth_on_failure = c("stop", "warn")` — ships defaulting to `"warn"`

> **🔒 DECISION D-1 (settled 2026-08-22).** The argument ships with default
> **`"warn"`**, and the default flips to **`"stop"` at the next version bump** —
> *not* on first landing.
>
> **Why the two-step.** `dev` is a live distribution channel, not a staging
> branch: medsim's r-universe registry pins `dev` (serving `0.5.1.9000`), and
> the Missing Effect study installs `remotes::install_github("data-wise/medsim@dev")`
> with a **queued Hopper fixed-branch re-run**. A breaking default would reach
> that run on push, mid-flight.
>
> Landing `"warn"` first gets every detector and diagnostic — D7's widened
> guards, D8's shape validation, D2′'s `truth_coverage`, and the D5 cache fix —
> into consumers' hands **immediately**, because those are what convert silence
> into a message. The default flip only changes *whether the message is fatal*.
> So the safety value lands now; only the abort waits.
>
> **Implementation consequences:**
> - `medsim_run(truth_on_failure = "warn")` is the shipped default; document it
>   as **temporary**, with the flip named in NEWS so consumers can prepare.
> - The `"warn"` branch is therefore the **primary** path at launch, not an
>   opt-out — its "failed scenario must be **absent** from `$truth`, not
>   present-with-fewer-columns" rule (see the implementer note above) becomes
>   load-bearing rather than an edge case. Test it as the main path.
> - D2′ and D7 matter **more** under this decision, since they are what makes a
>   `"warn"` run detectable at combine and at analysis.
> - Add a release-checklist item for the flip so it is not forgotten.
> - **Reassess before flipping**: confirm the Missing Effect Hopper re-run has
>   completed, or that its owner is ready.

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

> **Implementer note — this rule is necessary but NOT sufficient for E-3.**
> Keeping a stub row for a failed scenario is one way to make
> `do.call(rbind, truth_list)` receive frames of differing width and fail with
> `numbers of columns of arguments do not match`. Drop the element from
> `truth_list` (or `Filter(Negate(is.null), ...)` before the rbind). An
> implementer who "helpfully" keeps the row reintroduces E-3.
>
> **⚠️ AMENDMENT R4 (adversarial review):** the crash is caused by **any**
> column-set disagreement, not only failure stubs. A reviewer reproduced it
> with **every truth call succeeding**, where scenarios simply returned
> different parameter sets. Consequences:
>
> - Case 5's "no `numbers of columns` error" assertion holds only when all
>   *surviving* scenarios share an identical parameter set. Say so.
> - **The same latent crash exists in shipped code** at `R/cluster.R:406`
>   (the cross-chunk truth merge).
> - **Preferred fix:** rbind `truth_list` with column-union/NA-fill instead of
>   `do.call(rbind, ...)`. The package already owns a ragged-rbind helper for
>   exactly this class (`.medsim_rbind_reps`, used at `cluster.R:398`) — reuse
>   it and the whole crash class dies, rather than being narrowly dodged.
> - Otherwise, declare heterogeneous truth parameter sets explicitly
>   out of scope and document that scenarios must agree.
>
> Side effect worth knowing: this crash is *also* why D2′(b) cannot
> false-positive on heterogeneous truth today — such runs die before combine
> ever sees them.

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

> **⚠️ PLACEMENT — raise this in `medsim_combine_chunks()`'s own body, NOT
> inside `.medsim_audit_seed_provenance()`** (verified 2026-08-22 against `dev`
> @ `dd0d059`, after #50 shipped). The two named neighbours sit in *different*
> places: `partial_chunk` is raised in the combine body (~`R/cluster.R:505`),
> `dgp_failure` inside the audit helper (~`:776`). Since #50, that helper
> **returns early on a NULL/zero-row frame** — an all-array-tasks-failed run
> has no rows to audit, and falling through made it claim the chunks came from
> "an older medsim". Placing D2′ inside the helper would therefore make it
> silently unreachable for exactly the runs where truth coverage is most
> suspect. The combine body is also where `combined$truth` is in scope.
>
> Same seam, restated as a rule: **when a fix makes a previously-unreachable
> state reachable, re-audit what now runs in it.** That is how #50's own three
> review findings were introduced — see `GRILL-medsim-43` §Resolution.

> **⚠️ AMENDMENT R5 (adversarial review) — trigger (b) is UNREACHABLE as
> written, and the reason is a latent bug in shipped code.** Verified at
> `R/cluster.R:401-406`:
>
> ```r
> truth <- chunks[[1L]]$truth
> if (!is.null(truth)) {          # <-- if chunk 1 has no truth,
>   for (ch in chunks[-1L]) {     #     the union is SKIPPED ENTIRELY and
>     ...                         #     every later chunk's truth is DISCARDED
>   }
> }
> ```
>
> So "chunk 1's truth is absent" yields `combined$truth == NULL`, which D2′'s
> own non-NULL guard deliberately silences — indistinguishable from a
> legitimate `compute_truth = NULL` run. Trigger (b) collapses into (a).
>
> **Two consequences:**
>
> 1. **This is a latent defect in already-merged code**, currently unreachable
>    only because no chunk can produce truth at all (P-6). **§D1 makes it
>    live**: with chunk-1 gating, a failed-then-manually-rerun chunk 1 whose
>    truth lands on a different chunk has that truth silently dropped. Fix the
>    seed-from-chunk-1 logic to union across **all** chunks regardless of
>    whether chunk 1 carries truth — do this in the same increment as D1.
> 2. **GD2's original instinct was right about one thing.** Nothing anywhere
>    records that truth was *intended*. Detecting genuine loss (as opposed to
>    "none requested") needs exactly that one bit. **Recommended:** stamp
>    `truth_requested = !is.null(compute_truth)` into chunk provenance
>    alongside #43's existing `partial` flag (`R/cluster.R:238-247`), and let
>    D2′ fire on `truth_requested && is.null(combined$truth)`. This restores
>    trigger (b) at the cost of one boolean — cheaper than it looked at GD2,
>    because the provenance list already exists.
>
> **Also unstated:** `medsim_audit_results()` accepts a bare data.frame with no
> `$truth` at all. D2′ needs a carve-out there or it will error on legitimate
> input.

Live triggers after D6 **and amendment R5**: the `"warn"` opt-out, a
successful-but-degenerate truth frame (D8), and — once `truth_requested` is
stamped — a chunked run that lost its truth. Defense-in-depth, not the main
guard.

### D7 — widen the existing hard stop (Layer 3) — **TWO guards, not one**

> **⚠️ AMENDMENT R2 (adversarial review, 2026-08-22) — the original section
> pointed at the wrong function.** Both reviewers confirmed independently, and
> so did a direct check: **`R/analyze.R:82` belongs to `medsim_analyze()`**
> (which starts at `:73`), not to `medsim_analyze_coverage()` (which starts at
> `:358`). The coverage function has its **own separate guard with the same
> predicate** at **`R/analyze.R:369-372`** ("Cannot compute coverage: …").
>
> **E-1's evidence transcript runs through the *coverage* path.** An
> implementer following the original text would patch `medsim_analyze()` and
> leave the headline symptom — an empty coverage analysis returned as a
> success — fully reproducible. Both guards must be widened.

Apply the same widening at **both** sites:

| Site | Function | Existing message |
|---|---|---|
| `R/analyze.R:82-84` | `medsim_analyze()` | "Cannot compute accuracy metrics: …" |
| `R/analyze.R:369-372` | `medsim_analyze_coverage()` | "Cannot compute coverage: …" |

Also check `.medsim_analyze_coverage_interval()`, which `medsim_analyze_coverage()`
dispatches to at `:381` — it must not bypass the widened guard.

```r
truth_params <- setdiff(names(results$truth %||% data.frame()), "scenario")
if (is.null(results$truth) || nrow(results$truth) == 0 ||
    length(truth_params) == 0L) {
  stop("Cannot compute <accuracy metrics|coverage>: ground truth has no ",
       "parameter columns. Either every compute_truth() call failed, or ",
       "truth_function() returned a value medsim cannot read -- it must ",
       "return a NAMED LIST of length-1 numerics (a bare named vector, or ",
       "list elements longer than 1, are silently dropped; see ?medsim_run).")
}
```

> **Message wording matters here (AMENDMENT R3).** The original text blamed
> failures only ("this usually means every `compute_truth()` call failed") and
> advised re-running with `truth_on_failure = "stop"`. Per D8 below, a
> **successful** truth function can produce the identical degenerate frame —
> that user would re-run, see nothing fail, hit the same stop, and get the same
> useless advice. The wording above names both causes.

`%||%` is package-defined (`R/config.R:405`) and already used in `analyze.R:112`,
so it is in scope — verified.

This is **not** the change GD2 declined: it widens an already-loud whole-frame
guard to cover a case E-2 proves it misses. The per-parameter warning is
untouched, so subset analyses keep working.

### D8 — NEW (AMENDMENT R3): a *successful* truth function can also degenerate

Reviewers found a silent path with **no failure anywhere**. `truth_df` keeps
only elements satisfying `is.list(truth_value)` **and**
`length(value) == 1 && is.numeric(value)` (`R/runner.R:720-733`). Verified:

```
named numeric VECTOR  c(indirect = 0.09)      -> param cols: 0   (no warning)
list w/ length-2 value list(indirect = c(.05,.15)) -> param cols: 0   (no warning)
proper                list(indirect = 0.09)   -> param cols: 1
```

A truth function returning a bare named vector (an easy and natural mistake)
or an interval silently contributes **nothing**, and the run proceeds to the
same empty-analysis outcome as E-1 — while D5/D6 stay dormant, because nothing
failed.

**Required:** in `medsim_compute_all_truth()`, after building `truth_df`, if a
truth result was **successful** yet contributed zero parameter columns, raise
it — same `truth_on_failure` control, same default `"stop"` — with a message
naming the real cause and the required shape. This is what makes Layer 1
actually sufficient; without it, D5/D6 cover only half the ways to reach a
degenerate frame.

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

> **⚠️ AMENDMENT R6 (adversarial review) — the fixed tmp filename is unsafe
> here, unlike in PR #49.** PR #49's writers never collide: one chunk file per
> task. But truth cache files are **shared across tasks** — under D1's
> `config$compute_truth_every_chunk = TRUE`, or during a concurrent upgrade
> that re-heals caches, N array tasks write the *same* `truth_scenario_k.rds`
> and therefore the *same* `.truth_scenario_k.rds.tmp`. Two concurrent
> `saveRDS()` calls to one path interleave; the rename is atomic but the
> shared tmp **content** is not, so the winner can install a torn file.
>
> **Required:** unique tmp per writer —
> `tempfile(pattern = paste0(".", basename(file), "."), tmpdir = dirname(file))`
> — plus `on.exit(unlink(tmp), add = TRUE)`. Keep the same-filesystem rule.

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
| 3 | **REWRITTEN (R1).** A pre-existing cache file holding the **LEGACY UNCLASSED** `list(error = "<chr>")` ⇒ treated as a miss and recomputed (self-heal). A classed-sentinel fixture is **invalid** — that state cannot occur post-fix | a valid cached truth ⇒ loaded, not recomputed; a truth result with a legitimate **numeric** parameter named `error` ⇒ NOT treated as a sentinel |
| 4 | **E-2 + D-1.** All scenarios' truth fails ⇒ with `truth_on_failure = "stop"` **explicitly passed**, `medsim_run()` stops naming the scenario and the original error. ⚠️ Per D-1 the *shipped* default is `"warn"`, so this case must pass the argument rather than rely on the default — and a companion assertion must pin that the **default** is `"warn"** at launch, so the later flip is a deliberate, test-visible change | Under the shipped `"warn"` default (**the primary launch path**): warns, run continues, and every failed scenario is **absent** from `$truth` — cases 7/14 then catch the consequence at combine and at analysis |
| 5 | **E-3.** Mixed failure with `truth_on_failure = "warn"` ⇒ run completes, failed scenario **absent** from `$truth`, and **no** `numbers of columns of arguments do not match` | all truth succeeds ⇒ every scenario present |
| 6 | `truth_on_failure = "warn"` + a scenario absent from truth ⇒ combine raises `truth_coverage/missing` | all scenarios have truth ⇒ silent |
| 7 | A truth frame with rows but zero parameter columns ⇒ combine raises `truth_coverage/degenerate` | `compute_truth = NULL` run (`$truth` NULL) ⇒ **silent** (must NOT fire — the common case) |
| 8 | **D7.** `medsim_analyze_coverage()` on a parameter-less truth frame ⇒ **stops** with the new message | truth with ≥1 parameter, analyzing a subset of parameters ⇒ still only warns per missing parameter (contract preserved) |
| 9 | Sentinel detection is class-based: a truth function legitimately returning a numeric parameter named `error` is **not** treated as a failure | — |
| 10 | `medsim_run_chunk(compute_truth = fn)` on `chunk_id = 1` ⇒ `$truth` non-NULL | `chunk_id = 2`, same call ⇒ `$truth` NULL, unless `compute_truth_every_chunk = TRUE` |
| 11 | `medsim_cache_save()` failing between tmp-write and rename ⇒ target untouched, no torn file; mocked `file.rename` returning `FALSE` ⇒ informative stop | normal save ⇒ target written, tmp gone |
| 12 | `?medsim_config`'s rendered `seed` text matches D4 (grep-based doc-drift guard) | — |
| 13 | **Negative control:** a clean run with successful truth is unchanged end-to-end vs pre-fix (`$truth`, `$results`, coverage output identical) | — |
| 14 | **D7 both sites (R2).** Case 8 must be run against **both** `medsim_analyze()` and `medsim_analyze_coverage()` — E-1's symptom is on the *coverage* path, so patching only `analyze.R:82` leaves it reproducible. Include the interval branch (`.medsim_analyze_coverage_interval`, dispatched at `:381`) | truth with ≥1 parameter ⇒ both entry points behave as today |
| 15 | **D8 (R3).** A **successful** `truth_function` returning a bare named vector `c(indirect = 0.09)`, and one returning `list(indirect = c(.05, .15))` ⇒ actionable diagnostic naming the required shape — **not** the "every call failed" message | `list(indirect = 0.09)` ⇒ silent, 1 parameter column |
| 16 | **R4.** Under `truth_on_failure = "warn"`, scenarios with *different* successful parameter sets ⇒ no `numbers of columns of arguments do not match` (requires the ragged/union rbind) | identical parameter sets ⇒ unchanged |
| 17 | **R5.** Chunk 1 carries no truth but chunk 2 does ⇒ chunk 2's truth is **not** discarded by `medsim_combine_chunks()`, and (with `truth_requested` stamped) a genuinely truth-less combine raises `truth_coverage` | `compute_truth = NULL` run ⇒ silent |
| 18 | **R6.** Two concurrent writers to the same truth cache path ⇒ no torn file (unique tmp per writer) | single writer ⇒ unchanged |
| 19 | **D2′ routing.** `truth_coverage` honors `on_violation = "warn"` / `"ignore"` (downgrades correctly via `.medsim_signal_violations`, `cluster.R:792`), and `medsim_audit_results()` on a bare data.frame with no `$truth` does **not** error | — |

**Gates:** `devtools::test()` **in the worktree the PR ships from** (baseline
**1197 pass / 0 fail / 0 error / 1 skip** on `dev` @ `f4c8dfc`, independently
re-verified by two reviewers — check testthat's *error* column, not just
pass/fail); `R CMD check --as-cran` against the **0/0/0** baseline; then
independent adversarial review of the branch diff before integration.

> **⚠️ AMENDMENT R9 — one existing test WILL fail, by design.**
> `tests/testthat/test-runner.R:599-619`
> ("medsim_compute_all_truth warns and records error when truth_fn fails") does
> `expect_warning(..., "Truth computation failed")` — exactly the behavior D6
> flips to `stop()`. It must be **rewritten as part of this work** (it is a
> ready-made skeleton for Case 4). Budget it: an implementer who treats it as
> a "pre-existing failure" will have masked the change actually working.
> No other test or vignette depends on the old behavior — `test-pipeline-e2e.R`
> and `test-coverage-discrimination.R` use `compute_truth` only with functions
> that succeed (verified).

**E2E (per `e2e-before-pr.md`):** case 1 is the E2E — it is a live exercise
with a real failure mode, and the PR body must quote its actual transcript
(both the pre-fix `call count: 1` and the post-fix `call count: 2`).

---

## Risks

1. **Breaking change — larger than first assessed.** Per AMENDMENT R3 there
   are **two** flipping classes, not one: all-failed truth, and
   successful-but-wrongly-shaped truth (D8). The second may surface latent
   breakage in pipelines their owners believe are working. NEWS entry
   required; `truth_on_failure = "warn"` restores prior behavior.
2. **⚠️ `dev` IS a live distribution channel — AMENDMENT R8. RESOLVED by
   DECISION D-1.** The original text said the narrow blast radius argues
   Layer 1 "can ride `dev`". That reasoning was wrong on the facts: medsim's
   r-universe registry pins the **`dev`** branch (not `main`), and the Missing
   Effect study installs `remotes::install_github("data-wise/medsim@dev")` for
   its queued Hopper fixed-branch re-run. **A breaking default reaches real
   consumers on push-to-`dev`, not at release.**
   **Resolution (D-1, see §D6):** ship `truth_on_failure` defaulting to
   `"warn"`; flip to `"stop"` at the next version bump, after confirming the
   queued Hopper run has completed. Detectors and diagnostics land immediately;
   only the abort waits.
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

- 2026-10-14 · `feature/truth-integrity` · all four increments in one PR, as asked for 0.6.0:
  - **Layer 1 (D5 + D6 + D8 + R4):** a classed sentinel, never cached. On the read side, both the classed sentinel and the legacy unclassed `list(error = <chr>)` count as misses (the shape heuristic, not a version-salted fingerprint). `truth_on_failure = c("warn", "stop")` keeps D-1's `"warn"` default. Failed or unusable scenarios are absent from `$truth`, which is bound on the column union (`.medsim_rbind_union`).
  - **Layer 3 (D7):** one guard, `.medsim_require_truth()`, at both `medsim_analyze()` and `medsim_analyze_coverage()`. The interval branch is dispatched after it.
  - **D3 + D4:** `medsim_cache_save()` writes a unique tmp file per writer in `dirname(file)` and checks the rename. The `seed` docs are fixed, and so is `print()`'s "provenance only" line.
  - **D1 + D2′ + R5:** truth is gated to `chunk_id == 1L`, with `config$compute_truth_every_chunk`. `truth_requested` is stamped in provenance. The truth union runs across all chunks. `truth_coverage` (degenerate / missing / lost) is raised in the combine body and in `medsim_audit_results()`, with a data.frame carve-out.
  - Note: #57 (`c81f571`) had briefly shipped an ungated pass-through to `dev`. This increment replaces it.
  - Evidence: `tests/testthat/test-truth-integrity.R`, 20 tests. 16 fail on `dev`; the other 4 are the SPEC's negative controls plus case 18, which has nothing to catch on `dev` because it wrote no tmp file. Full suite: 556 / 0 / 0 / 1 skip. The E-1 transcript gives call count 1 on `dev` and 2 on the branch.
  - R9: the existing `test-runner.R` warning test still passes, because the default stays `"warn"` under D-1. It becomes the Case-4 skeleton when the default flips.
  - **Flipped 2026-10-14 (author: "do recommend"):** `truth_on_failure` defaults to `"stop"` in `medsim_run()`, `medsim_compute_all_truth()` and the new `medsim_run_chunk(truth_on_failure = )` pass-through. The R9 test is rewritten as a stop assertion with a `"warn"` companion, and case 4 now pins `"stop"`. Earlier note: D-1 ties the flip to "the next version bump, after the Missing Effect Hopper re-run completes"; that run has completed. The author decides whether the flip goes in 0.6.0.

---

## Amendment index (v2, 2026-08-22 adversarial review)

Three independent Fable reviewers; two found the same blocker independently.
Every amendment below was re-verified against the code before being applied.

| # | Severity | Section | What was wrong in v1 |
|---|---|---|---|
| R1 | **BLOCKER** | §D5 read side, Goal 2, Case 3 | Self-heal keyed on a class **nothing has ever written**; legacy poison is unclassed, so no poisoned cache would ever heal |
| R2 | **MAJOR** | P-3, §D7 | Cited `analyze.R:82`, which is **`medsim_analyze()`** — the coverage guard is `:369`. E-1's symptom is on the coverage path, so v1's fix would have left it reproducible |
| R3 | **MAJOR** | §D8 (new), blast radius, D7 message | A **successful** truth function returning a bare vector or non-scalar values degenerates identically, with nothing failing — v1 missed the class entirely and claimed only one flip |
| R4 | MAJOR | §D6 note | The rbind width crash is **not** failure-specific; same latent crash at `cluster.R:406` |
| R5 | MAJOR | §D2′ | Trigger (b) unreachable: combine **discards all truth** when chunk 1 has none — a latent shipped bug that §D1 would activate |
| R6 | MAJOR | §D3 | Fixed tmp filename collides when N tasks write one shared truth cache file |
| R7 | LOW | Evidence E-3 | "both cache files poisoned" — only the failing one is |
| R8 | MEDIUM | Risk 2 | "can ride `dev`" is false: r-universe pins `dev` and Missing Effect installs `@dev` |
| R9 | MEDIUM | Gates | `test-runner.R:599` pins the old warn behavior and was not budgeted |

Confirmed sound by review (no change needed): the classed sentinel's
`saveRDS`/`readRDS` round-trip and fingerprint-extraction survival; Step 1
genuinely preceding all replication work; D2′'s predicate mechanics and
non-NULL guard; `%||%` scope in `analyze.R`; the GD2-supersession reasoning;
all P-1…P-6 line cites; both issue-body corrections; and **no #41/#43 seam
conflict** — condition classes, violation triggers, and failure accounting all
compose cleanly.

---

**Grounded:** code trace + 3 empirical reproductions (E-1/E-2/E-3), 2026-08-22.
**Grilled:** [`GRILL-medsim-41-chunk-truth-atomic-cache-2026-08-22.md`](GRILL-medsim-41-chunk-truth-atomic-cache-2026-08-22.md) (GD1/GD2/GD5/GD6, F1–F6).
**Reviewed:** one adversarial pass, 2026-08-22 — corrected D2′'s predicate (GD2 superseded), restored D7 to scope, and pinned the "absent, not degenerate" rule as E-3's fix.
