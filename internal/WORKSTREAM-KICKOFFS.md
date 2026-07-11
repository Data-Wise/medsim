# Workstream kickoff prompts — medsim missing-data generators

Paste-ready prompts for the parallel build of `SPEC-medsim-missingdata-generators-2026-06-11.md`.
Each workstream is its **own `claude` session in its own worktree** (worktree rules). All branch off
`feature/dgm-interface`, which already has the frozen signatures + red `testthat` skeletons.

**Create the worktrees first (from `~/projects/r-packages/active/medsim`):**

```bash
git worktree add ~/.git-worktrees/medsim/feature-dgm-nonnormal   -b feature/dgm-nonnormal   feature/dgm-interface
git worktree add ~/.git-worktrees/medsim/feature-dgm-amputate    -b feature/dgm-amputate    feature/dgm-interface
git worktree add ~/.git-worktrees/medsim/feature-methods-missing -b feature/methods-missing feature/dgm-interface
# C and E after their deps land:
git worktree add ~/.git-worktrees/medsim/feature-scenarios-missing -b feature/scenarios-missing feature/dgm-interface
git worktree add ~/.git-worktrees/medsim/feature-analyze-missing   -b feature/analyze-missing   feature/dgm-interface
```

Then open a `claude` session in each worktree dir and paste its prompt.

---

## WS-A — `feature/dgm-nonnormal` (start now)
> Implement `medsim_rnonnormal()` in `R/dgm_nonnormal.R` per `SPEC-medsim-missingdata-generators-2026-06-11.md` (§1). Use the Fleishman power method (solve the cubic coefficients b,c,d for target skew/excess-kurtosis, transform standard-normal draws, then rescale to `mean`/`sd`). Error on infeasible (skew,kurtosis) outside the Fleishman region. Pure base R — no new dependency. Make `tests/testthat/test-dgm-nonnormal.R` pass (TDD: run it red first). Then `devtools::document()` + `devtools::test()`. Commit on this branch; do not touch other R/ files.

## WS-B — `feature/dgm-amputate` (start now)
> Implement `medsim_amputate()` in `R/dgm_amputate.R` per the spec (§2). MCAR = constant Bernoulli prob calibrated to `prop`; MAR = logistic on observed `predictors`; MNAR = logistic including the `target` value itself. Calibrate the logistic intercept so realized missingness ≈ `prop`. Support multi-column `target`. Preserve column names/order. Use `mice::ampute` if available (Suggests) else the built-in logistic amputer. Make `tests/testthat/test-dgm-amputate.R` pass. `devtools::document()` + `test()`. Commit on this branch only.

## WS-D — `feature/methods-missing` (start now)
> Implement the three adapters in `R/methods_missing.R` per the spec (§4) honoring the shared return contract in the file header. `medsim_method_mbco_mi()`: MI via `missingmed` + MBCO via `rmediation::mbco()` (during prototyping, wrap `~/projects/research/Missing Effect/code/prototype-d4-mbco.R`); return `branch_switch ∈ {0,1}`. `medsim_method_mc_ci()`: MI + `rmediation::medci()`; `branch_switch = NA`. `medsim_method_ipw()`: thin IPW; `branch_switch = NA`. Keep `missingmed`/`rmediation` in Suggests (load inside the closures; `requireNamespace`). Make `tests/testthat/test-methods-missing.R` pass (it `skip_if_not_installed`s the heavy deps). Commit on this branch only.

## WS-C — `feature/scenarios-missing` (after A + B land on their branches/dev)
> Implement `medsim_scenario_missing()` and `medsim_scenario_missing_grid()` in `R/scenarios_missing.R` per the spec (§3). The `data_generator` draws complete `X→M→Y` from `true_params` (use `medsim_rnonnormal()` for residuals when `nonnormal` is set), then calls `medsim_amputate()`. Build on the frozen WS-A/WS-B signatures. Make `tests/testthat/test-scenarios-missing.R` pass. `devtools::document()` + `test()`. Commit on this branch only.

## WS-E — `feature/analyze-missing` (after D lands)
> Implement `medsim_summarize_branch_switch()` in `R/analyze_missing.R` per the spec (§5). Drop rows where `converged == 0`, group by `by`, return `branch_switch_rate` + `n_valid` per group (ignore `NA` branch_switch in the mean). NEW file only — do NOT edit `analyze.R` (keeps it disjoint from the DM proposal's interval-coverage work). Make `tests/testthat/test-analyze-missing.R` pass. Commit on this branch only.

## WS-F — integration (after A–E merged to `dev`)
> On a `feature/dgm-integrate` worktree off updated `dev`: add `mice`, `missingmed`, `rmediation` to DESCRIPTION **Suggests** (not Imports). `devtools::document()` to regenerate NAMESPACE + man/. Run `devtools::check()` — resolve NOTEs; confirm the package loads and tests pass with the Suggests packages ABSENT (the adapters must degrade gracefully via `requireNamespace`). Update NEWS.md. This is the only step that edits shared files.

---

## Ground rules for every session
- TDD: run the module's test file red, implement, green. Don't edit another workstream's file.
- Roxygen is already written in the stub — fill the body, keep the signature.
- `devtools::document()` regenerates NAMESPACE/man in your branch; conflicts resolve at WS-F.
- Merge each module branch to `dev` via PR once its tests pass; do WS-F last.
