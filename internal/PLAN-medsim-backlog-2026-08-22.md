# PLAN — medsim backlog after the 2026-08-22 grill + adversarial review

**Created:** 2026-08-22 · **Base:** `dev` @ `d033d3e` (v0.5.1.9000)
**Status:** PROPOSAL — sequencing and scope only. No work started.
**Inputs:** [`SPEC-medsim-41-truth-integrity`](SPEC-medsim-41-truth-integrity-2026-08-22.md) (v2),
[`GRILL-medsim-35`](GRILL-medsim-35-gauge-probmed-exposure-2026-08-22.md),
[`GRILL-medsim-28`](GRILL-medsim-28-cluster-vignette-2026-08-22.md),
[`GRILL-medsim-29`](GRILL-medsim-29-prod3-adapter-2026-08-22.md) (deferred),
[`GRILL-medsim-43`](GRILL-medsim-43-parallel-fail-loud-2026-07-31.md) §PM1/PM2.

---

## The constraint that shapes everything: `dev` is a live distribution channel

Not a staging branch. Verified 2026-08-22:

- medsim's r-universe registry pins **`dev`** — currently serving `0.5.1.9000`.
- The Missing Effect study installs `remotes::install_github("data-wise/medsim@dev")`
  and has a **queued Hopper fixed-branch re-run**.

**Consequence:** a breaking change reaches real consumers **on push**, not at
release. This is why Phase 1 and Phase 2 below are split — everything
non-breaking can land freely; the loud layer needs a coordination decision
first (**D-1**).

Second standing constraint: **CRAN is still blocked** — `medrobust` is not on
CRAN (re-verified 2026-08-22 against `cloud.r-project.org`; `probmed` is not
either). Nothing in this plan unblocks it; it is upstream work.

---

## Phase 0 — Close out #43 properly ⚠️ DO THIS FIRST

Two MAJOR defects found by post-merge adversarial review, both **live on `dev`
right now**. Full detail in `GRILL-medsim-43` §"Post-merge adversarial review".

| Item | What | Why first |
|---|---|---|
| **PM2** | The three "real parallel" tests never unset `_R_CHECK_LIMIT_CORES_`, so under `R CMD check --as-cran` they run the **serial** path. Reproduced: worker pid == main pid, all 11 tests still pass | **The parallel path currently has no CI coverage.** Every later phase touches `runner.R`/`parallel.R`. Fixing this first is what makes all downstream test evidence trustworthy |
| **PM1** | An all-reps-failed chunk writes **no file**, so a trailing failed chunk combines with **zero violations**. The reserved-`error` class that motivated #43 fails *every* rep, so `partial_chunk` can never fire for it | Live silent-data-loss gap in the integrity layer #43 was built to be |

**Fixes.** PM2: `withr::local_envvar("_R_CHECK_LIMIT_CORES_" = NA)` in cases
1/6/13, **plus a PID assertion** so a future fallback regression fails loudly
instead of silently. PM1: move the provenance stamp + atomic write **above**
the `R/cluster.R:224-229` early `stop()` — PR #48's machinery already supports
an empty flagged chunk (empty chunks are exempt from the schema vote, and
`partial_flags` reads provenance of all chunks).

Also fold in the two MINORs while in the files: the stale "Inert today" comment
at `R/cluster.R:773-774` (the `dgp_failure` detector is armed and
load-bearing — the comment invites someone to prune it), and record the
`test-failure-rate.R` deviation.

**Non-breaking.** Safe on live `dev`. **Estimate: half a day** including review.
**Tracked as [#50](https://github.com/Data-Wise/medsim/issues/50)** (decision
D-2, settled 2026-08-22) — filed with both reproductions and proposed fixes.

---

## Phase 1 — #41 non-breaking hardening

Everything in the #41 v2 spec that is additive or strictly-better. Lands
freely on live `dev`; no consumer coordination needed.

| Ref | Change | Note |
|---|---|---|
| **D5** | Never cache a failure sentinel; treat a cached sentinel (**classed *and* legacy unclassed** — amendment R1) as a miss so existing poisoned caches self-heal | The headline fix. Kills E-1 |
| **R5** | `medsim_combine_chunks()` must union truth across **all** chunks, not skip the union when chunk 1 has none (`R/cluster.R:401-406`) | Latent shipped bug; D1 would activate it |
| **D3** | Atomic `medsim_cache_save()` — tmp + checked rename, **unique tmp per writer** (amendment R6) | |
| **D1** | `compute_truth` passthrough on `medsim_run_chunk()`, gated to `chunk_id == 1L` | Pairs with R5 |
| **D4** | `config$seed` roxygen correction | Doc-only |

**Estimate: ~1 day.** Acceptance cases 1, 2, 3 (rewritten per R1), 10, 11, 12,
17, 18 from the spec's matrix.

---

## Phase 2 — #41 loud layer — UNGATED by D-1 (ships `"warn"`-default)

> **D-1 settled:** this phase is **no longer blocked**. It ships with
> `truth_on_failure` defaulting to `"warn"`, so the detectors and diagnostics
> land without breaking the queued Hopper run. The `"stop"` flip becomes a
> one-line change at the next version bump — **add it to the release
> checklist**, and confirm the Hopper run has completed before flipping.
>
> Consequence for implementation: `"warn"` is the **primary** path at launch,
> not an opt-out. Its "failed scenario absent from `$truth`, not
> present-with-fewer-columns" rule is load-bearing, and D7/D2′ carry more
> weight — they are what makes a `"warn"` run detectable.

| Ref | Change |
|---|---|
| **D6** | `truth_on_failure = c("stop","warn")`, default `"stop"` |
| **D7** | Widen the hard stop at **both** `analyze.R:82` (`medsim_analyze`) **and** `:369` (`medsim_analyze_coverage`) — plus the interval branch (amendment R2) |
| **D8** | Reject/raise on a **successful** truth function that contributes zero parameter columns (amendment R3) |
| **D2′** | `truth_coverage` violation under `on_violation`, incl. the `truth_requested` provenance bit (amendment R5) |
| **R4** | Ragged/union rbind for `truth_list` so the width-mismatch crash class dies (also fixes the latent `cluster.R:406` crash) |
| **R9** | Rewrite `tests/testthat/test-runner.R:599-619`, which pins the old warn behavior — **budget it; it is not a pre-existing failure** |

**Two classes of run flip from succeeding to failing** (amendment R3): all-failed
truth, and successful-but-wrongly-shaped truth. The second may surface latent
breakage in pipelines their owners believe work today.

**Estimate: ~1 day** plus the migration note. Acceptance cases 4–9, 13–16, 19.

---

## Phase 3 — #35 gauge/probmed exposure guard

Independent of everything above (`R/methods_gauge.R` only). Runtime guard
warning when the installed probmed predates the per-row IPW weight fix, plus
the `se_method=`-is-canonical roxygen note and removal of the stale
"BCa planned pending upstream" framing.

**Blocked-ish:** the clean version floor (`Suggests: probmed (>= X)`) needs a
probmed release containing PR #34, which merged to probmed's **`dev`**, not
`main`. See decision **D-3**. The guard itself can ship without it.

**Estimate: ~2 hours.**

---

## Phase 4 — #28 cluster-workflow article

`vignettes/articles/cluster-workflow.qmd` (pkgdown-only, no CRAN build cost)
plus the `array_throttle` `@param` in `?medsim_config`.

**Must follow Phase 1** — the article documents D1's chunk-1 truth gate, its
straggler exception to the per-task `--time` rule, and the precompute-truth
pattern. Writing it earlier means rewriting it.

**Estimate: half a day.**

---

## Not scheduled

- **#29** — deferred at GD3, zero consumer. Design is complete in its GRILL doc
  if demand appears (CSDA reviewers requesting simulations).
- **CRAN submission** — blocked on `medrobust` reaching mainstream
  distribution. Not actionable here.

---

## Open decisions

| # | Decision | Status |
|---|---|---|
| **D-1** | How to land Phase 2's breaking change given live `dev` | 🔒 **SETTLED 2026-08-22 — ship `truth_on_failure` defaulting to `"warn"`; flip to `"stop"` at the next version bump.** Every detector and diagnostic (D7, D8, D2′, D5) lands immediately — those are what convert silence into a message; only the *abort* waits. Protects the queued Missing Effect Hopper run from a mid-flight contract change. Recorded in [`SPEC §D6`](SPEC-medsim-41-truth-integrity-2026-08-22.md) with its implementation consequences |
| **D-2** | File PM1/PM2 as a GitHub issue, or fix silently under #43 | 🔒 **SETTLED 2026-08-22 — filed as [#50](https://github.com/Data-Wise/medsim/issues/50)**, with both reproductions, proposed fixes, the three minor fold-ins, and the "confirmed sound" list |
| **D-3** | probmed `dev → main` + release, to unblock #35's version floor | ⏳ **OPEN** — cross-repo, needs explicit authorization. Phase 3's guard ships regardless; only the clean version pin waits |

---

## Recommended order

**Phase 0 → 1 → 2 → 3 → 4**, with Phase 3 movable anywhere (no shared files).

Phase 0 first is the load-bearing choice: until PM2 is fixed, `--as-cran`
evidence for anything touching the parallel path is not real evidence.

**Worktree note (descriptive — not an action item):** Phases 0–3 are code
changes across `runner.R`/`cluster.R`/`cache.R`/`analyze.R`/`config.R`/
`methods_gauge.R`, and this repo forbids feature code on `dev`, so each phase
is a feature branch. Phase 4 is docs and may go directly on `dev`.
