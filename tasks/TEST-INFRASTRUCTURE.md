# medsim Test Infrastructure — Knowledge & Runbook

Single-source knowledge doc for the simulation/parallel test-infrastructure
effort on branch `feature/test-infrastructure` (stacked on the RNG-fix branch /
PR #30). Companion files in `tasks/`: `plan.md` (full plan), `todo.md`
(checklist), `GOALS.md` (autonomous `/goal` prompts + operating instructions).

> **Status:** Phase 0 (CRAN blockers) ✅ · Phase 1 (Tier-A guards) ✅ · Phase 2
> (Tier-B cluster) ⏳ · Phase 3 (vignette+docs) ⏳. Commits: `dced161` (Phase 1),
> `8f7e096` (Phase 0). `.Rbuildignore`d (`tasks/` never ships to CRAN).

---

## Part 1 — Postmortem: the chunked-RNG bug (why this effort exists)

**Symptom.** A downstream product-of-three coverage study (a ~370-core-hour,
180-task Hopper array) reported **coverage = 1.0 across all 36 (scenario × N)
cells** — statistically impossible.

**Root cause (three converging facts in medsim's chunked/SLURM path):**
1. `medsim_run()` called `set.seed(config$seed)` with the **same scalar** on
   every chunk.
2. `medsim_run_chunk()` computed `chunk_config$rep_offset` but it was **read
   nowhere** — `replication` always restarted at 1.
3. `medsim_run_parallel()` was called with **no `seed=`**, so its
   L'Ecuyer-CMRG `clusterSetRNGStream` machinery (real, unit-tested, even
   commented "the only pattern that survives chunk-based SLURM jobs") **never
   activated**.

Net: every chunk regenerated the *same* short replication sequence. 1000
replications × 60 chunks collapsed to ~17 distinct outcomes → whatever few CI
values recurred all happened to contain the truth → spurious perfect coverage.

**Fix (PR #30).** `.medsim_det_seed(scenario_name, global_rep_id)` seeds each
replication deterministically, independent of chunk/worker/cluster-type/order.
`config$seed`/`seed_stream` no longer drive `medsim_run()` output (docs
updated). Verified: full suite + 4 new regression tests, local chunk-vs-chunk
distinctness, narrow-CI positive control, and a Hopper pilot (8/8 distinct CI
pairs).

**Why the test suite missed it — the load-bearing lesson.** The only
reproducibility test ran the **direct** `medsim_run()` path, and the chunk test
used a **method returning a hardcoded constant** — structurally incapable of
revealing RNG duplication. *Nothing that could catch the bug ran under
`R CMD check`.* Everything below follows from fixing that.

---

## Part 2 — The two-tier testing model

### First principle: correctness invariants live in Tier A

Every statistical-correctness invariant that can run **cheaply and
deterministically** (reproducibility, cross-chunk independence, coverage
calibration, positive-control discrimination) is a **Tier-A** test — fast,
`n_cores = 1`, tiny reps, always run under `R CMD check`. If a correctness test
can only run on Hopper, the next regression ships to CRAN unseen.

**Structural exception (named, not glossed):** FORK-worker RNG inheritance
*cannot* be guarded in Tier A — `parallel.R:130` forces the sequential path
under `_R_CHECK_LIMIT_CORES_=TRUE` before any cluster is made. Its only guard is
Tier-B (B4). `.medsim_det_seed` being FORK-independent by design shrinks the
exposure.

### The two tiers

| Tier | Home | Run by | How |
|------|------|--------|-----|
| **A — CRAN-safe** | `tests/testthat/` | always (CI + `R CMD check`) | cheap analytic methods, `n_cores=1`, tiny reps, deterministic seeds, `withr::local_envvar` to mock SLURM |
| **B — Hopper** | `inst/hopper-tests/` | manual on Hopper only | real multi-node SLURM; **never run by check** (`inst/` isn't executed); documented in an `eval:false` vignette |

### Second principle: pilot-before-scale (Tier-B execution)

No full-scale cluster job is submitted cold. Every Tier-B run: **pilot (≤4
tasks) → examine `MaxRSS`/`State`/actual results → scale only on a clean
pilot**, transcript captured. This session, the pilot step caught a 64× memory
over-allocation and validated the RNG fix *before* spending ~370 core-hours.

### Decoupling correctness from the expensive kernel

Tier-A correctness tests use a **trivial analytic CI method** (closed-form Wald
on `rnorm` draws), *not* the prod3 test-inversion kernel — so instrument
calibration runs in seconds under CRAN. The real kernel is a **self-contained
synthetic** dogfood at scale in Tier B; the actual prod3 study stays external in
its own repo as the genuine integration test (medsim can't depend on a
non-package kernel).

---

## Part 3 — Current status & phase map

- **Phase 0 — CRAN blockers ✅** (`8f7e096`): ∈ (U+2208) → ASCII in `estimand.R`
  (PDF manual now warning-free); seeded the flaky MCAR/MAR/MNAR p-value tests
  (5× green); `1:nrow` → `seq_len(nrow())` in `runner.R`. Full suite 425/0.
- **Phase 1 — Tier-A guards ✅** (`dced161`): T1 discrimination, T2
  truth-attachment, T3 failure-rate (+ interval-branch `failure_rate` fix), T4
  truth-cache invalidation (+ fix). 26 tests; rforge `--as-cran` clean, no
  revdep breakage.
- **Phase 2 — Tier-B cluster ⏳**: `inst/hopper-tests/` skeleton, SLURM e2e,
  synthetic dogfood, FORK-RNG realism, stress/edge suite. All pilot-gated.
- **Phase 3 — vignette+docs ⏳**: `vignettes/cluster-testing.qmd` (`eval:false`)
  + NEWS/README two-tier note.
- **Integration**: PR is human-gated (base `dev`, or rebased after PR #30
  merges).

Full task detail with acceptance criteria: `tasks/plan.md`. Checklist:
`tasks/todo.md`.

---

## Part 4 — Autonomous `/goal` runbook

`/goal` is a built-in Claude Code command (≥ v2.1.139): `/goal <condition>` runs
turn-by-turn **unsupervised** until a lightweight evaluator confirms the
natural-language completion condition (≤4000 chars). Pair with **Auto mode** for
fully unattended work.

**Two-part pattern (the prompts live in `tasks/GOALS.md`):**
- The **`/goal` condition** is the terse *acceptance test* — the evaluator reads
  it every turn. Because the evaluator **judges the conversation, not tools**,
  each condition says *prove it by pasting the check output here*.
- The **operating instruction** pasted right after is the *working brief* — how
  to proceed, plus this session's gotchas (rforge for devops; **never inline
  `Rscript -e`** — branch-guard false-positives, write a script file; stay on
  the feature branch; conventional commits, no Claude/Anthropic attribution).

**Safety gates are baked into the conditions, not trusted.** The Phase-2 goal's
completion condition *is* "scaffolded + piloted + summarized for approval" — so
an unattended run **auto-halts at the pilot go/no-go** and never scales a Hopper
array itself. PR creation is human-gated (no goal).

**Run order:** G0 *(done — running it just watches it verify)* → G2 *(halts at
go/no-go)* → *(human approves scale-ups)* → G3 → *(human opens PR)*. Paste each
`/goal` + its operating instruction; wait for one to clear before the next
(setting a new goal replaces the active one).

---

## Part 5 — Fixes ledger (latent bugs found while building the tests)

| Fix | File | What / why |
|-----|------|------------|
| interval-branch `failure_rate` | `analyze.R` | the `interval` coverage path (used by real studies) silently omitted `n_failed`/`failure_rate` that NEWS 0.4.0 advertised; now emits them, consistent with the default branch |
| truth-cache invalidation | `runner.R` | `medsim_compute_all_truth` reloaded **stale truth** when an `output_dir` was reused after a scenario changed; now content-fingerprints (`deparse` of DGM/truth + `params` by value; filename unchanged; legacy caches recompute) |
| ∈ Unicode CRAN WARNING | `estimand.R` | raw ∈ (U+2208) broke the PDF manual build; → ASCII "in" |
| flaky MCAR test | `test-dgm-amputate.R` | unseeded null-true p-value test hard-failed `--as-cran` ~5% of runs; seeded, threshold unchanged |
| `seq_len` footgun | `runner.R` | `1:nrow()` → `seq_len(nrow())` for the empty-input edge Phase-2 stress-tests |

---

## Key files

- Bug fix + Phase-1 code: `R/runner.R`, `R/analyze.R`, `R/estimand.R`,
  `tests/testthat/test-{coverage-discrimination,pipeline-e2e,failure-rate,truth-cache-invalidation}.R`
- Planning: `tasks/plan.md`, `tasks/todo.md`
- Autonomous run: `tasks/GOALS.md` (paste-ready `/goal` prompts + operating instructions)
- This doc: `tasks/TEST-INFRASTRUCTURE.md`
