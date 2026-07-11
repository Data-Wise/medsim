# Implementation Plan: medsim Simulation & Parallel-Code Test Infrastructure

**Status:** PLANNING ONLY — no test code written this session.
**Scope:** `medsim` R package (`Data-Wise/medsim`), simulation + parallel-execution surface.
**Motivating exemplar:** the chunked-RNG bug fixed in PR #30 (cross-chunk replication
collapse → spurious coverage = 1.0 in a ~370-core-hour Hopper run).
**Branch note:** implementation belongs on a *new* `feature/test-infrastructure` branch off
`dev` — these planning docs are deliberately **not** part of PR #30 (the RNG fix).
**Skill:** structured per `agent-skills:planning-and-task-breakdown` (each task carries
acceptance criteria, verification, dependencies, files-likely-touched, scope).

---

## Overview

Build a two-tier, three-kind test suite for medsim's simulation/parallel code so that the
class of bug PR #30 fixed **cannot ship to CRAN silently again**, while keeping
`R CMD check --as-cran` fast and cluster-realism tests off the CRAN critical path.

- **Three kinds:** unit · end-to-end (e2e) · dogfood.
- **Two tiers:** **A — CRAN-safe** (fast, deterministic, always run) · **B — Hopper**
  (real multi-node SLURM, never run by CRAN, documented in an `eval=FALSE` vignette).

---

## First principle (non-negotiable): correctness invariants live in Tier A

The RNG bug escaped for exactly one reason: the *only* reproducibility test ran the
**direct** `medsim_run()` path, and the chunk test used a **constant-returning method**, so
**nothing that could catch the bug ran under `R CMD check`.** The generalization:

> **Every statistical-correctness invariant that *can* run cheaply and deterministically is
> a Tier-A test — fast, `n_cores = 1`, tiny reps, always run.** Tier B is reserved *only*
> for what Tier A genuinely cannot do: real multi-node SLURM slicing, real timing, many-core
> FORK RNG, and full-scale coverage at production reps.

Caveat made explicit (see G3 / Risks): a small number of correctness properties — notably
**FORK-worker RNG inheritance** — are *structurally* impossible to guard in Tier A, because
`parallel.R:130` forces the sequential path under `_R_CHECK_LIMIT_CORES_=TRUE` before any
cluster is created. Those are named as accepted Tier-B-only limitations, not glossed over
with a Tier-A test that can't actually exercise the failure mode.

---

## Second principle (execution): pilot-before-scale on every Tier-B run

No full-scale cluster job is ever submitted cold. **Every Tier-B task (B2–B5) runs a small
pilot first — a few array tasks, tiny reps — whose results are examined before scaling.**
This is not ceremony: this session alone, the pilot step caught the 64× memory
over-allocation (would have throttled the full array), confirmed the module-load/shebang
fix, and validated the RNG fix (8/8 distinct CI pairs) *before* spending ~370 core-hours.
The gate, concretely:

1. **Pilot** — ≤ 4 array tasks, minimal reps, minimal N. Submit, wait, `sacct`.
2. **Examine** — check `MaxRSS` (mem sizing), `State` (all COMPLETED), and the *actual
   results* (distinct draws, plausible coverage, expected `failure_rate`) — not just "it ran."
3. **Scale only on a clean pilot** — a failed or surprising pilot blocks the full run; fix
   and re-pilot. Record the pilot transcript (JobIDs, `sacct`, result check) in the vignette.

Applies to the standing rule (memory `ask-local-vs-hopper`): always confirm before scaling
a cluster job, cost-gate first.

---

## Baseline: what PR #30 already shipped (do NOT re-propose)

PR #30 (`feature/rng-seed-audit`) already added, in `tests/testthat/test-cluster.R`:

- ✅ cross-chunk independence (`chunk 1 != chunk 2`, both fully distinct draws)
- ✅ same-chunk reproducibility (same `chunk_id` → identical draws)
- ✅ chunking-invariance (2-chunk vs 5-chunk split of the same total → same draw set)
- ✅ `.medsim_det_seed()` purity + order-sensitivity (anagrams don't collide)

These close old gap "cross-chunk RNG" entirely. This plan starts **after** that.

---

## Review findings — current coverage & remaining gaps

Surface reviewed (line counts): `parallel.R` (543), `runner.R` (619), `cluster.R` (213),
`cache.R` (526), `config.R` (396), `scenarios.R` (403). Existing tests: 25 files, ~5.4k
lines, 414 passing.

**Strengths.** Dense unit coverage of individual functions (config, estimand, scenarios,
cache save/load/expiry, analyze, tables, visualize); PSOCK cluster paths exercised (13
`skip_on_cran()` in `test-parallel.R`); CRAN core-limit guard present
(`_R_CHECK_LIMIT_CORES_` → sequential).

**Remaining gaps (this plan's targets), ranked by bug-prevention value:**

| # | Gap | Kind | Tier | Why it matters |
|---|-----|------|------|----------------|
| G1 | **No positive-control / discrimination test.** Nothing asserts the coverage *instrument* catches undercoverage. Run as a throwaway script this session, never codified. | e2e | **A** | The single genuinely-open correctness delta. A silently-always-covering instrument reads as "1.00 coverage" — indistinguishable from the bug we just fixed. |
| G2 | **No full-pipeline e2e.** No test runs `run_chunk × N → combine_chunks → analyze_coverage` and asserts the *statistical* result. All cluster tests are unit-level shape/plumbing checks. | e2e | **A** | The chunk→combine→analyze seam is where truth-attachment/row-alignment bugs live; the prod3 `compute_truth`-forwarding bug this session lived exactly here. |
| G7 | **No failure-rate / NA-CI handling test.** NEWS 0.4.0 added `n_failed`/`failure_rate`, but nothing asserts that a method failing on a *known* fraction (e.g. the near-singular `test_inversion_ci`→NA path) yields the right `failure_rate` **and** computes coverage over successes only (not NA-poisoned). | unit/e2e | **A** | Directly ties to the near-singular tail the F8 manuscript edit now caveats; NA-poisoned coverage silently biases the headline number. |
| G4 | **Stale-truth cache — latent bug, not just a gap.** `medsim_compute_all_truth` keys the truth cache on scenario *index* (`truth_scenario_%d.rds`), with no content hash and expiry off by default. Changing a scenario's DGM while reusing `output_dir` silently reloads **stale** truth. | unit | **A** | Confirmed against `cache.R`/`runner.R:402`: a changed DGM keeps old truth, biasing every downstream error/coverage number. Test-first ⇒ **fails on current code** ⇒ drives a fix. |
| G3 | **FORK-path RNG has no Tier-A guard (structural).** Only PSOCK is tested, `skip_on_cran()`-gated; the unix-default FORK L'Ecuyer path is un-testable under check (core-limit guard forces sequential first). | unit | **B only** | FORK-worker RNG inheritance is the classic failure mode; accepted Tier-B-only limitation — see Risks. |
| G6 | **No real multi-task SLURM e2e.** Chunk slicing only ever exercised single-process via a mocked `SLURM_ARRAY_TASK_ID`; never across real array tasks with real combine. | e2e | **B** | Only a real array job exercises scheduler-driven slicing, filesystem race on `combine_chunks`, per-task env. |
| G8 | **No stress / edge-case coverage on the cluster.** Nothing tests degenerate inputs or failure/partial-completion at scale: `n_chunks > n_replications`, 100%-failure chunks (near-singular Σ → all-NA), missing/duplicate chunk files (timed-out or requeued tasks), RNG seed-collision across the full grid, and memory/concurrency limits. | unit+e2e | **B** (+ cheap boundary units in **A**) | Every one of these bit *this session*: the 64× mem over-allocation, the requeue/idempotency question, the matrix-755 all-NA tail, and the reviewer's hash-collision concern. Robustness gaps surface only under real scale + real failure. |
| G5 | **No dogfood regression guard.** The real dogfood (prod3 coverage sim) lives *outside* the package and guards nothing. | dogfood | **A** (miniature) + **B** (full) | A packaged miniature study is the end-to-end contract test; the full study is the realism check. |

---

## Architecture Decisions

1. **Physical home for Tier-B tests: `inst/hopper-tests/`.** Installed with the package,
   reachable via `system.file("hopper-tests", package = "medsim")`, **never executed by
   `R CMD check`** (check runs only top-level `tests/*.R`, not `inst/`). Confirm nothing
   `source()`s it during check. (Alt `tests/hopper/<subdir>/` also works — check runs
   top-level `tests/*.R` only — but `inst/` is the conventional home for shipped-not-checked
   runnable assets.)
2. **Tier-A tests stay in `tests/testthat/`**, gated *by construction*: `n_cores = 1`, ≤ 8
   reps, deterministic seeds, `withr::local_envvar()` to mock SLURM. They ride the existing
   `R-CMD-check.yaml` / `test-coverage.yaml` CI with **zero new infra**.
3. **Decouple correctness tests from the expensive integrator (critical).** G1/G2/G7 use a
   **trivial analytic-CI method** (closed-form Wald on `rnorm` draws) — *not* the prod3
   test-inversion kernel — so instrument-calibration runs in **seconds** under CRAN. The
   expensive kernel is exercised at full scale **only** in Tier B (G5/G6).
4. **Vignette documents Tier B with `execute: eval: false`.** Reuse the exact pattern in
   `vignettes/getting-started.qmd`, so every R chunk is shown-not-run and adds **zero**
   CRAN-check time. Expected outputs shown as static text.

---

## Task List

> **Autonomous execution:** paste-ready Claude Code `/goal` completion
> conditions for every phase live in `tasks/GOALS.md`. Phase 0/1/3 goals run
> fully unsupervised (with Auto mode); the Phase-2 goal is written to auto-halt
> at the pilot go/no-go (the pilot-before-scale safety gate is baked into the
> condition, not left to trust).

### Phase 0 — Make `R CMD check --as-cran` green (CRAN-blocker cleanup)

Pre-existing blockers surfaced by rforge at Checkpoint A (none from Phase-1's
diff); an autonomous "CRAN-ready" run needs them closed first.

- **G0.1** ∈ (U+2208) Unicode WARNING from `R/estimand.R` → `man/medsim_estimand.Rd`
  fails the PDF manual build. Fix with `\eqn{\in}{in}`/ASCII + `rforge r:document`.
  Scope: XS.
- **G0.2** unseeded flaky p-value test `test-dgm-amputate.R:33` (MCAR) — seed it;
  5 consecutive runs green; do **not** loosen the 0.05 threshold. Scope: XS.
- **G0.3** (optional) `1:nrow(...)` → `seq_len(nrow(...))` at `runner.R:195,211` —
  the empty-input footgun that G2.5 stress-tests; fixing here makes it correct,
  not just detected. Scope: XS.
- **Checkpoint 0:** `rforge lib.rcmd --kind check --as-cran` → FAIL 0, no ∈
  WARNING, only the spurious first-submission/Remotes NOTE.

### Phase 1 — Tier-A correctness guards (build first; the actual protection)

#### Task T1: Positive-control coverage-discrimination test (G1)
- **Description:** Codify the narrow-CI positive control run as a throwaway this session, as
  a suite test through the *full* chunk→combine→analyze path, using a cheap analytic method.
- **Acceptance criteria:**
  - [ ] A near-nominal analytic method yields coverage inside a tolerance band around 0.95.
  - [ ] A deliberately-narrowed-CI control (⅓ width) yields coverage ≪ 0.95 (undercovers).
  - [ ] Both run via `medsim_run_chunk → medsim_combine_chunks → medsim_analyze_coverage`.
- **Verification:** `devtools::test(filter = "coverage-discrimination")`; runs under
  `R CMD check --as-cran` (`_R_CHECK_LIMIT_CORES_=TRUE`) in < ~2 s; the narrowed control is
  the planted defect — confirm the test **fails** if the control is swapped for the honest
  method.
- **Dependencies:** none (baseline PR #30 merged is preferred but not required).
- **Files likely touched:** `tests/testthat/test-coverage-discrimination.R` (new).
- **Estimated scope:** S (1–2 files).

#### Task T2: Full-pipeline e2e + truth-attachment guard (G2)
- **Description:** A known-coverage scenario driven end-to-end, asserting the statistical
  result *and* that truth survives the combine seam (guards the prod3
  `compute_truth`-forwarding class of bug).
- **Acceptance criteria:**
  - [ ] Combined `$truth` has one row per scenario with correct values after
    `medsim_combine_chunks`.
  - [ ] `medsim_analyze_coverage` on the combined object lands in-band for the good method.
  - [ ] Test asserts a *wrong* truth value would flip the coverage verdict (planted defect).
- **Verification:** `devtools::test(filter = ...)`; `--as-cran` clean; verified to fail
  when truth is perturbed.
- **Dependencies:** T1 (shares the analytic-method + tiny-config helpers).
- **Files likely touched:** `tests/testthat/test-pipeline-e2e.R` (new);
  possibly a shared `tests/testthat/helper-cheap-study.R`.
- **Estimated scope:** S.

#### Task T3: Failure-rate / NA-CI handling test (G7)
- **Description:** A method that fails on a *known* fraction of reps must report the right
  `failure_rate`/`n_failed` and compute coverage over successes only (no NA poisoning).
- **Acceptance criteria:**
  - [ ] With a method returning NA CIs on a fixed k/n reps, `failure_rate ≈ k/n`.
  - [ ] Reported coverage equals coverage over the non-NA subset (not `NA`, not deflated).
  - [ ] A run with zero failures still reports `failure_rate = 0` and unchanged coverage.
- **Verification:** `devtools::test(filter = ...)`; `--as-cran` clean; fails if NA rows are
  silently counted as "not covered".
- **Dependencies:** T1/T2 (analytic-method helper).
- **Files likely touched:** `tests/testthat/test-failure-rate.R` (new); read-only ref to
  `R/analyze.R` coverage path.
- **Estimated scope:** S.

#### Task T4: Stale-truth-cache — test-first bug fix (G4)
- **Description:** **This is a latent-bug fix, not a green regression test.** Write the
  failing test first (changing a scenario's DGM must not reuse cached truth), then fix the
  cache key to be content-sensitive.
- **Acceptance criteria:**
  - [ ] New test: same `output_dir` + same scenario index but *changed* DGM/params ⇒ truth
    is recomputed, not loaded stale. **Fails on current `main`/`dev`.**
  - [ ] Fix: `medsim_compute_all_truth` keys the cache on a scenario *content fingerprint*
    (e.g. `digest` of name+params+DGM body), not index alone — or invalidates on mismatch.
  - [ ] Existing cache-hit/expiry tests still pass (no regression to the legitimate
    cache-reuse path).
- **Verification:** the new test red→green across the fix commit; full `test-cache.R` +
  `test-runner.R` green; `--as-cran` clean.
- **Dependencies:** none; touches shipped code (schedule after T1–T3 so the pure-test tasks
  land first).
- **Files likely touched:** `R/runner.R` (`medsim_compute_all_truth`), possibly `R/cache.R`;
  `tests/testthat/test-cache.R` or a new `test-truth-cache-invalidation.R`.
- **Estimated scope:** M (touches shipped runner + cache logic).

#### Checkpoint: Tier A
- [ ] All new Tier-A tests pass locally **and** under `R CMD check --as-cran`; check-time
  delta < a few seconds.
- [ ] Each correctness test verified to **fail** against its planted defect (T1 narrowed
  control, T2 perturbed truth, T3 NA-as-uncovered, T4 stale truth on current code).

### Phase 2 — Tier-B scaffolding (Hopper only; never run by CRAN)

#### Task B1: `inst/hopper-tests/` skeleton + entry point
- **Description:** Create the shipped-not-checked home + a one-command runner so the Tier-B
  suite is reproducible on Hopper.
- **Acceptance criteria:**
  - [ ] `inst/hopper-tests/` with a `run-all.sh` (or `Makefile` target) that runs the suite.
  - [ ] `R CMD check` confirmed to **not** execute anything under `inst/`.
- **Verification:** `R CMD check --as-cran` log shows no `inst/hopper-tests` execution;
  `system.file("hopper-tests", package = "medsim")` resolves post-install.
- **Dependencies:** Decision on Tier-B home (Open Questions).
- **Files likely touched:** `inst/hopper-tests/run-all.sh` (new), `.Rbuildignore` review.
- **Estimated scope:** S.

#### Task B2: Real multi-task SLURM e2e (G6)
- **Description:** A small real SLURM array → combine → analyze on Hopper, the on-cluster
  analogue of T1/T2.
- **Acceptance criteria:**
  - [ ] Submit script runs a ≥ 4-task array; `combine_chunks` merges without race/loss;
    coverage lands near-nominal for the good analytic method.
- **Verification:** manual on Hopper; transcript (JobIDs, `sacct` states, final coverage)
  captured into the vignette as static output.
- **Dependencies:** B1.
- **Files likely touched:** `inst/hopper-tests/e2e-slurm-array.R` + submit script (new).
- **Estimated scope:** M.

#### Task B3: Full-scale synthetic dogfood (G5)
- **Description:** A **self-contained** full-scale study exercising the whole medsim API
  (`build scenarios → run_chunk × N → combine → analyze_coverage`) with a synthetic analytic
  method — **no external kernel** (decision: prod3 kernel is not a package; the real prod3
  study stays external in product-of-three as the genuine integration test).
- **Acceptance criteria:**
  - [ ] Runs at production scale (full reps, many chunks) using only medsim + base R.
  - [ ] Coverage near-nominal for the well-behaved synthetic method; a planted-defect variant
    (e.g. deflated CI) undercovers — the dogfood can fail.
  - [ ] Zero dependency on `product-of-three/` or any un-packaged code.
- **Verification:** manual on Hopper; expected-output fixtures + transcript in vignette.
- **Dependencies:** B1.
- **Files likely touched:** `inst/hopper-tests/dogfood-synthetic.R` (new).
- **Estimated scope:** M.

#### Task B4: Many-core FORK RNG realism (sole FORK guard; G3)
- **Description:** The only meaningful guard for FORK-worker RNG independence (impossible in
  Tier A). Real FORK cluster, assert all draws distinct at scale.
- **Acceptance criteria:**
  - [ ] Real FORK cluster over many cores; across a full chunked run, no two replications
    share identical draws (the old-bug signature).
- **Verification:** manual on Hopper; transcript in vignette.
- **Dependencies:** B1.
- **Files likely touched:** `inst/hopper-tests/fork-rng-realism.R` (new).
- **Estimated scope:** S–M.

#### Task B5: Cluster stress + edge-case suite (G8)
- **Description:** Push the chunk/combine/analyze pipeline to its failure boundaries at real
  scale — the robustness half of Tier B. Each case grounded in something that bit this
  session or the reviewer flagged.
- **Acceptance criteria (each an assertion, must be able to fail):**
  - [ ] **`n_chunks > n_replications`** — empty/degenerate chunks: pipeline errors cleanly or
    produces correct zero-row chunks that `combine_chunks` handles (no silent wrong count).
  - [ ] **100%-failure chunk** — a near-singular Σ (matrix-755 class) yielding all-NA CIs:
    `failure_rate = 1`, coverage is `NaN`/`NA` gracefully (not 0, not a crash, not
    NA-poisoned into the pooled number).
  - [ ] **Missing chunk file** (timed-out task) — `combine_chunks` reports the gap loudly,
    never silently combines a partial grid as if complete.
  - [ ] **Duplicate chunk file** (requeued task) — combine dedups / does not double-count.
  - [ ] **RNG seed-collision at grid scale** — across the full `(scenario, rep_id)` grid
    (1000s of pairs), `.medsim_det_seed` produces zero collisions (verifies the
    polynomial-hash fix holds at production scale, not just the 6 sample names).
  - [ ] **Memory/concurrency boundary** — a config that would over-request memory is caught
    (guards the 64× over-allocation class); `MaxRSS` sits within the requested envelope.
- **Verification:** manual on Hopper, **pilot-first** (Second principle); transcripts +
  `sacct` for each case captured in the vignette. Cheap boundary cases (`n_chunks > n_rep`,
  duplicate/missing file handling) *also* get a Tier-A unit test where they don't need a real
  cluster.
- **Dependencies:** B1; shares fixtures with B2.
- **Files likely touched:** `inst/hopper-tests/stress-edge-cases.R` (new); a few boundary
  units in `tests/testthat/test-cluster.R`.
- **Estimated scope:** M–L.

#### Checkpoint: Tier B
- [ ] `inst/hopper-tests/` confirmed never sourced during check.
- [ ] **Every Tier-B task (B2–B5) ran its pilot first** (≤ 4 tasks), pilot results examined
  (`MaxRSS`/`State`/actual results), and only then scaled — pilot transcripts captured.
- [ ] Full Tier-B suite runs green on Hopper (one command); transcripts captured.

### Phase 3 — Vignette + docs

#### Task V1: `vignettes/cluster-testing.qmd` (`eval: false`)
- **Description:** Document the Tier-B suite; show each script as non-evaluated R + static
  expected output; state the invocation path.
- **Acceptance criteria:**
  - [ ] Global `execute: eval: false`; every chunk shown-not-run.
  - [ ] Builds; `R CMD check` time delta negligible; explains the two-tier model + why
    FORK-RNG is Tier-B-only.
- **Verification:** vignette builds; grep the built artifact for no evaluated output; check
  timing unchanged.
- **Dependencies:** B1–B4 (documents them).
- **Files likely touched:** `vignettes/cluster-testing.qmd` (new).
- **Estimated scope:** S.

#### Task V2: NEWS.md + README/pkgdown two-tier note
- **Description:** Record the two-tier testing model and where each tier runs.
- **Acceptance criteria:**
  - [ ] NEWS entry + a README/pkgdown paragraph naming Tier A (CRAN) vs Tier B (Hopper) and
    the vignette link.
- **Verification:** pkgdown builds; links resolve.
- **Dependencies:** V1.
- **Files likely touched:** `NEWS.md`, `README.md`, `_pkgdown.yml`.
- **Estimated scope:** XS.

#### Checkpoint: Complete
- [ ] `R CMD check --as-cran` clean, no new NOTES/WARNINGs, negligible time delta.
- [ ] Tier-B suite green on Hopper.
- [ ] Vignette builds with all chunks un-evaluated.

---

## Per-tier invocation path (the "building" half)

| Tier | Where | How run | CRAN sees it? |
|------|-------|---------|---------------|
| **A** | `tests/testthat/` | existing `R-CMD-check.yaml` + `test-coverage.yaml`; `devtools::test()` locally | **Yes** — always |
| **B** | `inst/hopper-tests/` | manual on Hopper via `run-all.sh` / `make hopper-tests`; documented in `cluster-testing.qmd` | **No** — `inst/` not executed by check |

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| A "correctness" test lands Tier-B-only | High — reopens the CRAN-blind-spot that shipped the bug | First principle + Checkpoint A: every *cheaply-testable* invariant runs under `--as-cran` |
| **FORK-RNG correctness has no Tier-A guard** | Medium — a FORK-only regression could pass check | **Accepted structural limitation** (`parallel.R:130` forces sequential under check). B4 is the sole guard; T-tier uses `.medsim_det_seed` which is FORK-independent by design, shrinking the exposure |
| Positive control that can't fail | High — decoration, not protection | Planted-defect requirement on T1/T2/T3 (per `e2e-before-pr` contract) |
| T4 fix regresses legitimate cache reuse | Medium | Keep existing cache-hit/expiry tests green across the fix; content-fingerprint only the *key*, not the reuse policy |
| Tier-A tests slow the CRAN check | Medium | Decision 3: cheap analytic method, `n_cores=1`, tiny reps; measure check-time delta at Checkpoint A |
| `inst/hopper-tests/` sourced during check | Medium | Verify no `source()`/autoload reaches `inst/`; confirm at Checkpoint B |
| Planning docs leak into PR #30 | Low | Keep `tasks/` uncommitted; implement on a separate `feature/test-infrastructure` branch |

## Decisions (resolved 2026-07-10, was Open Questions)

- **Tier-B home:** ✅ `inst/hopper-tests/` — installed, `system.file()`-reachable, never run
  by check (the "check ignores `inst/`" rule is rock-solid). Accepted cost: a few KB of
  cluster scripts ship in the install tarball.
- **Dogfood scope (B3):** ✅ **Self-contained synthetic dogfood.** medsim's own B3 uses a
  synthetic analytic method exercising the full medsim API at scale — **no external kernel,
  no forked math.** The real prod3 coverage study stays in the product-of-three repo as the
  genuine integration test (it already dogfoods medsim as a dependency). Rationale: the
  prod3 test-inversion kernel (`prod3_opt.R`) is not a package, so medsim cannot depend on
  it; vendoring a copy would fork the math and create a cross-repo sync burden.
- **Vignette engine:** ✅ `.qmd` — the package already commits to quarto
  (`getting-started.qmd` + `VignetteBuilder: quarto`), so `.qmd` adds zero new dependency
  and keeps one engine. (`.Rmd` was rejected: it would introduce a second engine unless we
  migrate off quarto entirely, which is out of scope.)
