# TODO: medsim Test-Infrastructure Overhaul

Ordered by bug-prevention value (Tier-A correctness guards first — the actual protection —
then Tier-B realism + docs). See `tasks/plan.md` for full per-task detail (acceptance
criteria, verification, deps, files, scope). **Planning only; no code written yet.**
Implementation goes on a new `feature/test-infrastructure` branch off `dev`, NOT in PR #30.

## Phase 0 — Make `--as-cran` green (CRAN-blocker cleanup, pre-existing) · AUTO

- [ ] **G0.1** fix ∈ (U+2208) Unicode WARNING in R/estimand.R man page (`\eqn{\in}{in}`), `rforge r:document`, re-check
- [ ] **G0.2** seed the flaky MCAR/MAR p-value tests (test-dgm-amputate.R); 5 consecutive runs green; do NOT loosen 0.05
- [ ] **G0.3** (optional) `1:nrow` → `seq_len(nrow())` at runner.R:195,211 (pre-empts the G2.5 empty-chunk edge)
- [ ] Checkpoint 0: `rforge lib.rcmd --kind check --as-cran` → FAIL 0, no ∈ WARNING, only spurious first-submission NOTE
- See `tasks/GOALS.md` G0 for the paste-ready `/goal` condition covering this phase.

## Already done (PR #30 — baseline, do not redo)

- [x] Cross-chunk independence test
- [x] Same-chunk reproducibility test
- [x] Chunking-invariance test (2-chunk vs 5-chunk → same draws)
- [x] `.medsim_det_seed()` purity + anagram/order-sensitivity test

## Phase 1 — Tier-A correctness guards (CRAN-safe, always run)

- [ ] **T1 (S)** positive-control discrimination — near-nominal ≈ 0.95 vs narrowed-CI control ≪ 0.95, through chunk→combine→analyze, cheap analytic method (G1). *Crown jewel — build first.*
- [ ] **T2 (S)** full-pipeline e2e + truth-attachment guard — known-coverage scenario in-band; perturbed truth flips verdict (G2). Dep: T1.
- [ ] **T3 (S)** failure-rate / NA-CI handling — known-fraction failures ⇒ right `failure_rate`, coverage over successes only, no NA poisoning (G7). Dep: T1/T2.
- [ ] **T4 (M)** stale-truth-cache **bug fix, test-first** — changed DGM must not reuse cached truth; fix cache key to content-fingerprint (G4). *Fails on current code.* Touches shipped `runner.R`/`cache.R`.
- [ ] Checkpoint A: all pass under `R CMD check --as-cran`; each verified to fail against its planted defect; check-time delta < a few seconds.

## Phase 2 — Tier-B scaffolding (Hopper only, never run by CRAN)

- [ ] **B1 (S)** `inst/hopper-tests/` skeleton + `run-all.sh`/`make hopper-tests` entry point; confirm check never executes `inst/`.
- [ ] **B2 (M)** real multi-task SLURM e2e — small array → combine → analyze, near-nominal (G6). Dep: B1.
- [ ] **B3 (M)** full-scale **synthetic** dogfood — self-contained study, synthetic analytic method, full reps, zero external-kernel dep (G5). Dep: B1.
- [ ] **B4 (S–M)** many-core FORK RNG realism — **sole FORK guard** (impossible in Tier A), all draws distinct at scale (G3). Dep: B1.
- [ ] **B5 (M–L)** cluster stress + edge cases (G8) — `n_chunks > n_rep`, 100%-failure chunk (all-NA, `failure_rate=1`), missing/duplicate chunk files, RNG collision at grid scale, mem/concurrency boundary. Each must-fail-capable; cheap boundary cases also get Tier-A units. Dep: B1.
- [ ] **Pilot-before-scale gate (standing):** every Tier-B task (B2–B5) runs a ≤4-task pilot, examines `MaxRSS`/`State`/actual results, scales only on a clean pilot. Caught the 64× mem bug + RNG fix this session.
- [ ] Checkpoint B: `inst/hopper-tests/` never sourced during check; every B-task piloted-then-scaled (transcripts captured); suite green on Hopper.

## Phase 3 — Vignette + docs

- [ ] **V1 (S)** `vignettes/cluster-testing.qmd` with `execute: eval: false` — Tier-B suite documented, chunks shown-not-run, invocation path stated, explains why FORK-RNG is Tier-B-only. Dep: B1–B4.
- [ ] **V2 (XS)** NEWS.md + README/pkgdown note the two-tier testing model. Dep: V1.
- [ ] Checkpoint Complete: `--as-cran` clean + negligible time delta; Tier-B green on Hopper; vignette builds with all chunks un-evaluated.

## Decisions (resolved 2026-07-10)

- [x] Tier-B home: **`inst/hopper-tests/`** (installed, never run by check)
- [x] Dogfood scope (B3): **self-contained synthetic** dogfood; real prod3 study stays external
- [x] Vignette engine: **`.qmd`** (matches existing; quarto already a build dep)
