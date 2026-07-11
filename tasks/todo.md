# TODO: medsim Test-Infrastructure Overhaul

> **✅ COMPLETE — merged to dev as PR #31 (squash `986f0e0`), 2026-07-11.**
> Worktree `feature-test-infrastructure` pruned post-merge. All phases below
> shipped; kept as a historical checklist, not actionable.

Ordered by bug-prevention value (Tier-A correctness guards first — the actual protection —
then Tier-B realism + docs). See `tasks/plan.md` for full per-task detail (acceptance
criteria, verification, deps, files, scope).

## Phase 0 — Make `--as-cran` green (CRAN-blocker cleanup) · ✅ DONE (commit 8f7e096, pre-flighted)

- [x] **G0.1** ∈ (U+2208) → ASCII "in" in R/estimand.R; man/ regenerated; PDF manual builds with no Unicode LaTeX warning
- [x] **G0.2** seeded MCAR/MAR/MNAR p-value tests (test-dgm-amputate.R); 5 consecutive runs green; 0.05 threshold unchanged
- [x] **G0.3** `1:nrow` → `seq_len(nrow())` at runner.R:195,211
- [x] Checkpoint 0: full testthat 425 pass / 0 fail; PDF manual clean of the ∈ WARNING
- Pre-flighted manually so the `/goal` G0 run starts from an already-clean base and just verifies.

## Already done (PR #30 — baseline, do not redo)

- [x] Cross-chunk independence test
- [x] Same-chunk reproducibility test
- [x] Chunking-invariance test (2-chunk vs 5-chunk → same draws)
- [x] `.medsim_det_seed()` purity + anagram/order-sensitivity test

## Phase 1 — Tier-A correctness guards (CRAN-safe, always run) · ✅ DONE (PR #31)

- [x] **T1 (S)** positive-control discrimination — near-nominal ≈ 0.95 vs narrowed-CI control ≪ 0.95, through chunk→combine→analyze, cheap analytic method (G1). *Crown jewel — build first.* → `tests/testthat/test-coverage-discrimination.R`
- [x] **T2 (S)** full-pipeline e2e + truth-attachment guard — known-coverage scenario in-band; perturbed truth flips verdict (G2). Dep: T1. → `tests/testthat/test-pipeline-e2e.R`
- [x] **T3 (S)** failure-rate / NA-CI handling — known-fraction failures ⇒ right `failure_rate`, coverage over successes only, no NA poisoning (G7). Dep: T1/T2. → `tests/testthat/test-failure-rate.R`
- [x] **T4 (M)** stale-truth-cache **bug fix, test-first** — changed DGM must not reuse cached truth; fix cache key to content-fingerprint (G4). *Fails on current code.* Touches shipped `runner.R`/`cache.R`. → `tests/testthat/test-truth-cache-invalidation.R`
- [x] Checkpoint A: all pass under `R CMD check --as-cran`; each verified to fail against its planted defect; check-time delta < a few seconds.

## Phase 2 — Tier-B scaffolding (Hopper only, never run by CRAN) · ✅ DONE (PR #31)

- [x] **B1 (S)** `inst/hopper-tests/` skeleton + `run-all.sh`/`make hopper-tests` entry point; confirm check never executes `inst/`.
- [x] **B2 (M)** real multi-task SLURM e2e — small array → combine → analyze, near-nominal (G6). Dep: B1. → `inst/hopper-tests/{submit_chunk.sh,combine_analyze.R}`
- [x] **B3 (M)** full-scale **synthetic** dogfood — self-contained study, synthetic analytic method, full reps, zero external-kernel dep (G5). Dep: B1. → `inst/hopper-tests/tier_b_synthetic.R`
- [x] **B4 (S–M)** many-core FORK RNG realism — **sole FORK guard** (impossible in Tier A), all draws distinct at scale (G3). Dep: B1. → `inst/hopper-tests/fork_rng_realism.R`
- [x] **B5 (M–L)** cluster stress + edge cases (G8) — `n_chunks > n_rep`, 100%-failure chunk (all-NA, `failure_rate=1`), missing/duplicate chunk files, RNG collision at grid scale, mem/concurrency boundary. Each must-fail-capable; cheap boundary cases also get Tier-A units. Dep: B1. → `inst/hopper-tests/grid_collision_check.R` + `tests/testthat/test-cluster{,-edge-cases}.R`
- [x] **Pilot-before-scale gate (standing):** every Tier-B task (B2–B5) runs a ≤4-task pilot, examines `MaxRSS`/`State`/actual results, scales only on a clean pilot. Caught the 64× mem bug + RNG fix this session.
- [x] Checkpoint B: `inst/hopper-tests/` never sourced during check; every B-task piloted-then-scaled (transcripts captured); suite green on Hopper — production run at 12,000 reps: nominal coverage 0.947, narrow-interval 0.480, 12,000/12,000 distinct seeds, FORK-reproducible, all PASS.

## Phase 3 — Vignette + docs · ✅ DONE (PR #31)

- [x] **V1 (S)** `vignettes/cluster-testing.qmd` with `execute: eval: false` — Tier-B suite documented, chunks shown-not-run, invocation path stated, explains why FORK-RNG is Tier-B-only. Dep: B1–B4.
- [x] **V2 (XS)** NEWS.md + README/pkgdown note the two-tier testing model. Dep: V1.
- [x] Checkpoint Complete: `--as-cran` clean + negligible time delta; Tier-B green on Hopper; vignette builds with all chunks un-evaluated.

## Decisions (resolved 2026-07-10)

- [x] Tier-B home: **`inst/hopper-tests/`** (installed, never run by check)
- [x] Dogfood scope (B3): **self-contained synthetic** dogfood; real prod3 study stays external
- [x] Vignette engine: **`.qmd`** (matches existing; quarto already a build dep)
