# GRILL — medsim #28: cluster-workflow vignette + array_throttle doc

**Date**: 2026-08-22 · **Target**: issue #28 · **Base**: dev @ `1150e45`
**Status**: PROPOSAL — docs only; lowest-risk of the three live issues
(#41 → #35 → #28; #29 deferred). Vehicle decided at GD4: a pkgdown-only
article under `vignettes/articles/`, **not** a built vignette.
**Location**: canonical design doc for #28.

---

## 0. Scope confirmed live

All pipeline functions already exist, are exported, and are individually
documented — confirmed via `man/`: `medsim_nsim_for_mcse()` (`R/nsim.R:12`),
`medsim_estimate_speedup()` (`R/parallel.R:455`), `medsim_write_submit_script()`,
`medsim_run_chunk()`, `medsim_combine_chunks()`, `medsim_check_results()`,
`medsim_table_timing()` (`R/tables.R:305`). Nothing to build; the gap is
purely narrative — no single document walks the pipeline end to end.

`vignettes/cluster-testing.qmd` exists but covers a different topic (the
two-tier CRAN-vs-Hopper **testing** strategy, `execute: eval: false`
throughout) — confirmed by reading its content; it never mentions
`medsim_nsim_for_mcse`, `medsim_estimate_speedup`, or `medsim_table_timing`.
Not a candidate to extend; a new vignette is the right shape.

`array_throttle` is documented only on `medsim_write_submit_script()`
(`R/cluster.R:24,28`; `man/medsim_write_submit_script.Rd:16,44`) — never
mentioned in `?medsim_config`'s own roxygen (`R/config.R`, confirmed via
grep: zero hits). A user reading `?medsim_config` top-to-bottom has no way
to discover the field exists.

---

## 1. Design

### 1a. New pkgdown article: `vignettes/articles/cluster-workflow.qmd`

> **Vehicle decided at grill GD4 — `vignettes/articles/`, not `vignettes/`.**
> medsim already ships **4 built vignettes** under `VignetteBuilder: quarto`;
> a 5th adds permanent CRAN check time and one more quarto-render failure mode
> on the package's CRAN-submission path. Files under `vignettes/articles/` are
> rendered by pkgdown for the site but **never built by `R CMD check`** —
> same narrative, zero build cost. Precedent: medfit already uses this pattern
> (`vignettes/articles/{bootstrap,extraction}.qmd`).
>
> Accepted trade-off: the article is **not** available offline via
> `vignette()` / `browseVignettes()`, which is a real cost for a user on a
> login node without a browser. Mitigation: `?medsim_write_submit_script` and
> `?medsim_combine_chunks` stay the offline entry points and should carry a
> `@seealso` pointer to the article's URL.
>
> This is medsim's first `vignettes/articles/` directory — check whether
> `.Rbuildignore` needs an `^vignettes/articles$` entry (existing rules at
> lines 14-16 cover `_files` dirs and `.quarto`, not `articles/` itself).

Ties the existing pieces into one pipeline, per the issue's original
scope, plus the two hard-won empirical rules from the issue's 2026-06-26
pilot comment (Hopper P4 DR grid, UNM CARC):

```
medsim_nsim_for_mcse()        -- how many reps for a target MCSE
      |
medsim_estimate_speedup()      -- is parallel/chunking worth it at this n_cores
      |
[PILOT: <=4 array tasks]       -- mandatory gate, not optional
      |                           - calibrate overhead= from the pilot's actual timing
      |                           - size --mem/--mem-per-cpu from pilot MaxRSS x ~2
      |                           - size --time PER ARRAY TASK from pilot wall x ~1.5,
      |                             under the partition ceiling -- not one whole-array value
      |                           - EXCEPTION (#41 F5): if using the chunk-1 truth gate,
      |                             chunk 1 also computes ground truth (n_truth = 10000 by
      |                             default) and is a systematic straggler. Either size its
      |                             walltime for truth + reps, or precompute truth (below).
      |
medsim_write_submit_script()   -- emit the SLURM array script
      |
medsim_run_chunk() (x N)       -- one per array task
      |
medsim_combine_chunks(on_violation=)  -- incl. interim-look pattern
      |    \
      |     medsim_combine_violation recovery (tryCatch, partial results)
      |
medsim_check_results()         -- pre-analysis sanity
      |
medsim_table_timing()          -- publication timing table
```

Sections, in order:

1. **When to use this pipeline** (chunked SLURM array jobs; not for local/test runs).
2. **Step 1 — size the run**: `medsim_nsim_for_mcse()` + `medsim_estimate_speedup()`.
3. **Step 2 — pilot before array, always**: the mandatory-pilot rule, with the
   two calibration formulas above stated as concrete `@details`-style rules,
   not vague guidance. Cite the real pilot numbers from the issue as a worked
   example (oracle n=500: 0.11G/6s; DR n=2000: 1.28G/16:46; DR n=8000:
   >35min) to show the ~12x memory over-provisioning and the wall-time
   span that motivates per-task, not whole-array, sizing.
4. **Step 3 — submit**: `medsim_write_submit_script()`, `array_throttle`
   cross-referenced from here to `?medsim_config`.

   **Ground truth on the cluster (cross-ref #41 GD6/F5).** Document the
   preferred production pattern as: compute truth **once, before submitting
   the array**, so medsim's fingerprint-guarded cache is warm, then submit
   every task with `compute_truth = NULL`. Balanced tasks, no straggler, no
   duplicated work. The `medsim_run_chunk(compute_truth =)` passthrough
   (gated to `chunk_id == 1L`) is the convenience path for small or simple
   runs — present it as such, with the straggler caveat attached, not as the
   default recommendation.
5. **Step 4 — run**: `medsim_run_chunk()` per array task (brief — this is
   what the submit script invokes, not something the user calls directly
   in most workflows).
6. **Step 5 — combine + audit**: `medsim_combine_chunks()`, the
   `on_violation` control, the interim-look pattern
   (`on_violation = "warn"` for combining a still-running array),
   `medsim_combine_violation` recovery via `tryCatch`.
7. **Step 6 — sanity + report**: `medsim_check_results()`,
   `medsim_table_timing()`.

**Explicit correction carried forward from the issue body**: no shell
`[ -s "$chunk_out" ]` output-existence gate anywhere in this vignette — that
was withdrawn in the chunked-run-gates review (R3); completeness lives in
`medsim_combine_chunks()`'s Gates A–D, not the shell template. State this
as a one-line callout so a reader who saw an older draft of this guidance
elsewhere doesn't reintroduce it.

**Explicitly out of scope** (per the issue): MaxRSS/per-task `sacct`
capture as an automated feature — no current consumer. The vignette may
*mention* checking `sacct -j <jobid> --format=MaxRSS,Elapsed` manually for
pilot calibration (that's exactly how the cited pilot numbers were
produced) without medsim automating it.

### 1b. `array_throttle` doc fix

Add one `@param` (or a `@details` cross-reference, whichever roxygen
convention `medsim_config()` already uses for optional cluster-only fields
like `chunk_id`/`n_chunks`) to `R/config.R`:

```
@param array_throttle Integer: optional. Caps concurrently-running SLURM
  array tasks via `--array=1-N%K` in [medsim_write_submit_script()].
  `NULL` (default) submits all `n_chunks` tasks with no throttle.
```

Placed next to the existing `chunk_id`/`n_chunks`/`array_size` params for
locality (they're the same cluster-mode parameter family).

---

## 2. Non-Goals

- No new R code — `array_throttle`'s behavior is unchanged, only documented
  in a second place.
- Not modifying `cluster-testing.qmd` — different topic, left as-is.
- Not building `sacct`-based MaxRSS/timing capture (issue's own out-of-scope note).

---

## 3. API Summary

| File | Change |
|---|---|
| `vignettes/articles/cluster-workflow.qmd` | NEW (pkgdown-only, not a built vignette — GD4) |
| `.Rbuildignore` | add `^vignettes/articles$` if not already covered |
| `R/config.R` | `array_throttle` `@param` added to `medsim_config()` roxygen |
| `_pkgdown.yml` | register the new vignette in the articles/navbar menu (same pattern used for `missing-data-mediation.qmd`/`estimand-kinds.qmd`, per `.STATUS`'s 2026-07-11 pkgdown-audit entry) |
| `NEWS.md` | one-line docs entry |

---

## 4. Verification

Docs-only change — per `pre-pr-testing.md`'s tier table, the minimum gate
is lint + link/count validators, not the full test suite:

1. `devtools::document()` — confirm `array_throttle` appears in the
   rendered `?medsim_config` help text (grep the generated `.Rd`).
2. Strict pkgdown build (`pkgdown::check_pkgdown()` or equivalent) — 0
   problems, matching the bar set by the 2026-07-11 vignette-gap fix.
3. Render `cluster-workflow.qmd` (quarto render, `execute: eval: false`
   throughout, same as `cluster-testing.qmd`) — confirms no syntax/include
   errors; nothing here executes real R code against a live cluster.
4. Manual read-through cross-checking every code snippet's function name
   and argument spelling against the actual exported signatures (an
   `eval: false` vignette has no runtime check catching a typo).

---

## 6. Grill ledger (interactive, 2026-08-22)

| # | Question | Resolution |
|---|---|---|
| GD4 | Built vignette or pkgdown-only article? | **`vignettes/articles/`** — see the callout in §1a. The doc's original "docs-only, cheap" framing understated the cost: a 5th built vignette adds CRAN check time and quarto-render risk on the submission path, for content whose audience is reading the website anyway. Rejected: built vignette (offline `vignette()` access not worth the CRAN surface — mitigated by `@seealso` pointers), array_throttle-fix-only (leaves the actual onboarding gap open), verify-consumer-first (the pipeline is documented from the package's own API; matching one study's scripts would narrow it). |

### Consumer confirmed

Unlike #29, #28 **does** have a live cluster consumer: the Missing Effect
study runs medsim on UNM CARC Hopper (`.STATUS:16`, fixed-branch re-run
unblocked). Note the adjacent open item at `.STATUS:81` — that repo's
`code/config.R` still pins `medsim >= 0.2.0` (pre-RNG-fix); the article
should state the minimum version its guidance assumes, so a reader on an old
pin does not follow instructions their installed version cannot honor.

---

## 5. Branch / Worktree Plan (descriptive — not an action item)

Docs-only work is permitted directly on `dev` per this repo's own
`spec-only-mode.md` convention (code changes need a worktree; docs do not).
A feature branch is still fine if preferred for review isolation — no
technical dependency either way.
