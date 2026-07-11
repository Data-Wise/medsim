# medsim Tier-B cluster tests (Hopper only — never run by `R CMD check`)

These scripts live in `inst/`, so they are **installed with the package but
never executed by `R CMD check`** (check runs only top-level `tests/*.R`). They
exercise the simulation/parallel code at real multi-node SLURM scale — the
things Tier-A (`tests/testthat/`) cannot: real chunk slicing across array tasks,
many-core FORK RNG, and full-scale coverage. See `tasks/plan.md` Phase 2 +
`tasks/TEST-INFRASTRUCTURE.md` for the model.

**Self-contained:** a synthetic analytic Wald-CI-for-a-normal-mean study (no
product-of-three kernel). The real prod3 study stays external as the genuine
integration test.

## Files

| File | Role |
|------|------|
| `tier_b_synthetic.R` | shared study: scenarios, nominal + planted-defect methods, estimand, truth |
| `run_chunk.R` | one SLURM array task → `chunk_%04d.rds` (env-configured) |
| `combine_analyze.R` | combine chunks + attach truth + print by-scenario coverage |
| `fork_rng_realism.R` | B4 — real FORK cluster; asserts all draws distinct + reproducible |
| `grid_collision_check.R` | B5 — no `.medsim_det_seed` collision across the full grid |
| `submit_chunk.sh` | SLURM array task (`#!/bin/bash -l` login shell for `module`) |
| `run-all.sh` | one-command entry point; submits **pilots only** (≤4 tasks) |

## Pilot-before-scale

`run-all.sh` submits small pilots. **Do not scale to a full run without explicit
approval** and a clean pilot (all COMPLETED, MaxRSS within the 512M/cpu envelope,
nominal coverage ~0.95, narrow control < 0.80, no collisions). This is the
`tasks/plan.md` Second principle.

## Pilot results (2026-07-11, Hopper, jobs 4274532/33/34)

All pilots ≤4 tasks, all COMPLETED (~4 s each), current-branch medsim installed.

| Pilot | Check | Result | Verdict |
|-------|-------|--------|---------|
| B2 e2e (nominal) | by-scenario coverage, 6 scen × 200 reps | **0.953** | PASS (near-nominal) |
| B3 dogfood (narrow control) | coverage of a 1/3-width interval | **0.491** | PASS (undercovers as designed → dogfood can fail) |
| B4 FORK-RNG realism | distinct CI pairs / reproducibility, real FORK | **64/64 distinct, run1==run2** | PASS |
| B5 grid-collision | distinct (scenario,CI) + `.medsim_det_seed` collisions | **1200/1200 distinct, 0 collisions** | PASS |

MaxRSS was 0 (SLURM's ~30 s sampler can't catch a 4 s job); the synthetic
workload is memory-light, comfortably within `512M/cpu`. Re-check MaxRSS if a
full run uses a heavier method or much larger N/reps.

**Go/no-go: GO** for a full run — but that is a **deliberate, human-approved**
step (pilot-before-scale), not part of this pilot pass.

## Full run (2026-07-11, Hopper, jobs 4274541/42/43 — human-approved scale-up)

Approved production scale: **n_rep=2000 across 20 chunks** (12000 reps/method),
nominal + narrow each as a 20-task array, FORK at 256 reps. All 41 tasks
COMPLETED (2–4 s each).

| Check | Result | Verdict |
|-------|--------|---------|
| Nominal coverage | overall **0.947**; per-scenario 0.941–0.953; 0/12000 failures | PASS (near-nominal, MC-SE ≈ 0.005) |
| Narrow dogfood control | overall **0.480**; per-scenario 0.463–0.491 | PASS (undercovers as designed) |
| Grid-collision | **12000/12000 distinct**; 0 cross-grid + 0 within-scenario `.medsim_det_seed` collisions | PASS (hash fix holds at production grid) |
| FORK-RNG realism | reproducible run1==run2; distinctness asserted | PASS |

MaxRSS was again blank — each task finishes in ~3 s, under SLURM's ~30 s
sampler; the synthetic Wald workload is memory-trivial (512M/cpu heavily
over-provisioned, harmless). A heavier real-kernel study *would* register
MaxRSS; re-check the envelope then.

## Run

```bash
# on Hopper, after R CMD INSTALL of the current medsim, from this dir:
./run-all.sh
# then, once jobs COMPLETED:
Rscript combine_analyze.R tier_b_nominal nominal
Rscript combine_analyze.R tier_b_narrow  narrow
Rscript grid_collision_check.R tier_b_nominal
```
