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

## Run

```bash
# on Hopper, after R CMD INSTALL of the current medsim, from this dir:
./run-all.sh
# then, once jobs COMPLETED:
Rscript combine_analyze.R tier_b_nominal nominal
Rscript combine_analyze.R tier_b_narrow  narrow
Rscript grid_collision_check.R tier_b_nominal
```
