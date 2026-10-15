# GRILL: medsim 0.6.0 release plan

**Date:** 2026-10-14 · **Repo:** Data-Wise/medsim · **Status:** 5/5 branches locked
**To be committed** to `internal/` in the release-prep PR (medsim keeps its grill ledgers there).

## Evidence from the sweep

- `main` = `a000108` (Release v0.5.1). `dev` is 29 commits ahead, at `0.5.1.9000`.
- Open PRs: #58 (API: `n`/`n_truth`, performance object, one analyze warning) and #59 (#41 truth integrity, with `truth_on_failure` defaulting to `"stop"` as of `ff1db84`). Both target `dev`, and both edit `NEWS.md`, `vignettes/cookbook.qmd`, `R/config.R` and `R/analyze.R`.
- Open issues: #41 (resolved by #59), #35 (blocked upstream, probmed#32), #29 (prod3 adapter), #28 (cluster-workflow docs + `array_throttle`).
- **R-universe builds `dev`** (it serves `0.5.1.9000` from `dev`); the GitHub default branch is `main`.
- Missing Effect cites 0.6.0 + `df = "chan_meng"` at `index.qmd:361` and `:438` (PR #33, on HOLD). Its `code/config.R` `truth_fun` returns `list(indirect = a * b)`, a shape #59 accepts.

## Decision ledger

| # | Branch | Decision | Rejected (and why) |
|---|--------|----------|--------------------|
| R1 | Scope | 0.6.0 = what is already in `dev` + #58 + #59. | Finishing #28's docs (delays the release; can go in 0.6.1). Landing #29 (new feature, its own review). #35 (blocked upstream). |
| R2 | Gate | On the combined `dev`: full `devtools::test()`, the three articles run standalone (tutorial, analyzing-results, cookbook), and a local `R CMD check --as-cran`; restore any roxygen/`inst/doc` drift. Then CI. | Suite + articles only (no CRAN-level check before `main`). CI only (the articles' quoted output would never be re-verified on the combined code). |
| R3 | Exposure window | **Revised after the Codex adversarial review (2026-10-14):** the `0.6.0` version bump (`DESCRIPTION`, NEWS heading `# medsim 0.6.0`) goes **inside #59**, which merges last. The breaking `"stop"` default therefore reaches `dev` (and R-universe) already labeled `0.6.0`, with **no** window under `0.5.1.9000`. | *Originally chosen:* a separate release-prep PR right after #59. Codex: the window ("minutes") has no bound, because that PR still has to be created, checked and merged. Moving R-universe to `main` (a standing registry change; deferred and needs separate approval). |
| R4 | Missing Effect | After 0.6.0 is live on R-universe, merge #33 as is; `code/config.R` guard stays `medsim >= 0.4.0`. | Guard `>= 0.6.0` (would name a version that did not produce the reported numbers). A 0.6.0 reproduction rerun (not needed: the re-reference is exact, max difference 8.9e-16 per medsim#55). |
| R5 | After the tag | `dev → main` merge commit, tag `v0.6.0`, GitHub release, close #41 by hand (closing keywords do not fire on `dev`), then bump `dev` to `0.6.0.9000` with a fresh NEWS heading. CRAN is out of scope. | Leaving `dev` at `0.6.0` (post-release commits would claim to be the release). CRAN submission now (a separate multi-day track). |

## Sequence

1. CI green on #58, then merge (squash). Ask first.
2. Merge `dev` into `feature/truth-integrity` (which already carries the `0.6.0` bump and this ledger); run Gate R2 there (full suite, articles standalone, `R CMD check --as-cran`); push; CI green on #59, then merge (squash). Ask first.
3. (Folded into step 2: there is no separate release-prep PR.)
4. `dev → main` release PR (merge commit), tag `v0.6.0`, GitHub release, close #41.
5. Bump `dev` to `0.6.0.9000`.
6. Confirm R-universe serves ≥ 0.6.0, then merge Missing Effect #33.

## Adversarial review (Codex, 2026-10-14): both findings fixed

- **[high] Breaking default published before the version bump** → R3 revised: the bump now lives in #59.
- **[high] #58 reported missing truth as NA performance, silently** → fixed on #58 (`0e563d9`): the results-object path of `medsim_analyze_performance()` now warns and names every scenario without truth for the parameter (absent, or an NA-filled union value). The test fails without the fix.

## Open questions

- Should R-universe build `main` instead of `dev`? Deferred (R3).
- #28's remaining docs → 0.6.1.
