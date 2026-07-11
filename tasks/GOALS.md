# GOALS — medsim test-infrastructure (for Claude Code `/goal`)

Paste-ready completion conditions for the built-in **`/goal`** command
(Claude Code ≥ v2.1.139). `/goal <condition>` runs turn-by-turn **without
supervision** until a lightweight evaluator confirms the condition; pair it with
**Auto mode** for fully unattended work. One goal is active at a time — setting a
new one replaces the old — so run these **in order**, letting each complete
before pasting the next.

Key evaluator fact: the evaluator **judges the conversation, it does not run
tools.** So every condition below says *prove it by pasting the check output into
the conversation* — the main loop must actually run the check (use **rforge** for
R-package devops) and surface the result, or the goal can never verify.

Worktree: `~/.git-worktrees/medsim/feature-test-infrastructure` (branch
`feature/test-infrastructure`, stacked on the RNG-fix / PR #30). Full rationale,
task detail, and autonomy boundaries: `tasks/plan.md`, `tasks/todo.md`.

> **Safety gates are baked into the conditions, not left to trust.** The Hopper
> goal (G2) *completes at the pilot go/no-go* — it does not scale up, because
> scaling a cluster job needs explicit human approval (`ask-local-vs-hopper` +
> pilot-before-scale). Opening the PR (G4) is human-gated and has no `/goal`.

---

## G0 — Phase 0: make `--as-cran` green (AUTO, fully unsupervised)

```
/goal In the medsim worktree ~/.git-worktrees/medsim/feature-test-infrastructure, `R CMD check --as-cran` (run it via rforge: lib.rcmd --kind check --as-cran) has ZERO ERRORs and ZERO WARNINGs attributable to this branch: specifically the "Unicode character ∈ (U+2208) not set up for use with LaTeX" WARNING originating from R/estimand.R's generated man/medsim_estimand.Rd is gone (fix by replacing the raw ∈ with \eqn{\in}{in} or ASCII and re-running rforge r:document), and no leftover medsim-manual.tex NOTE remains. ALSO: the unseeded p-value tests in tests/testthat/test-dgm-amputate.R (the MCAR/MAR "missingness independent/depends" tests) are made deterministic with set.seed, and 5 consecutive testthat::test_file runs of that file all report 0 failures. PROVE completion by pasting into the conversation: (a) the rforge check summary line showing FAIL 0 / WARN 0 with no ∈ warning, and (b) the 5 consecutive pass/fail counts. CONSTRAINTS: do not loosen any statistical threshold (keep 0.05); do not modify main; limit changes to R/estimand.R, man/, and tests/testthat/test-dgm-amputate.R; only the spurious first-submission / Remotes / new-submission NOTE may remain. If not achieved, stop after 25 turns and summarize the blocker.
```

## G3 — Phase 3: Tier-B vignette + docs (AUTO, fully unsupervised)

> Run G3 **after** the Tier-B scripts exist (G2's scaffold), or it has nothing to
> document. If running docs-only first, scope it to the two-tier model + skeleton.

```
/goal In the medsim worktree ~/.git-worktrees/medsim/feature-test-infrastructure, vignettes/cluster-testing.qmd exists with `execute: eval: false` (matching getting-started.qmd), builds cleanly, and a grep of the built artifact shows NO evaluated R output (all chunks shown-not-run); it documents the Tier-B (Hopper) cluster test suite, its one-command invocation path, and why FORK-worker-RNG correctness can only be a Tier-B guard. ALSO: NEWS.md and README (and _pkgdown.yml if present) gain a note naming the two-tier testing model — Tier A = CRAN-safe/always-run, Tier B = Hopper-only/never-run-by-check — and linking the new vignette. PROVE by pasting: the vignette build result, the grep showing no evaluated output, and the rforge check confirming a negligible check-time delta (inst/ and the eval-false vignette add no run time). CONSTRAINTS: the vignette's R must never execute during R CMD check; no new hard dependency (quarto is already the VignetteBuilder). Stop after 20 turns if not achieved and summarize.
```

## G2 — Phase 2: Tier-B cluster suite, scaffold + PILOT ONLY (auto-halts at the safety gate)

> This condition is deliberately satisfied by "scaffolded + piloted + summarized
> for approval," NOT by a full-scale run — so an unsupervised `/goal` completes
> at the go/no-go and never submits a large Hopper array on its own.

```
/goal In the medsim worktree ~/.git-worktrees/medsim/feature-test-infrastructure, the Tier-B cluster test suite is SCAFFOLDED and PILOT-VALIDATED but explicitly NOT scaled up. Done means ALL of: (1) inst/hopper-tests/ exists with an executable run-all.sh (or make hopper-tests) entry point, and rforge lib.rcmd --kind check confirms nothing under inst/ runs during R CMD check (check time unchanged); (2) for EACH Tier-B script — a real multi-task SLURM e2e, a self-contained synthetic full-API dogfood (NO product-of-three kernel), a many-core FORK-RNG realism check, and the stress/edge cases (n_chunks>n_replications, 100%-failure/all-NA chunk, missing chunk file, duplicate chunk file) — a small ≤4-task Hopper PILOT has been submitted and has run to COMPLETED, with its sacct states, MaxRSS-within-envelope check, and result sanity (distinct draws / plausible coverage / expected failure_rate) PASTED into the conversation, followed by an explicit go/no-go recommendation for the full run; (3) the cheap boundary edge cases are ALSO added as Tier-A units in tests/testthat and pass under R CMD check. The goal is COMPLETE once every pilot is examined and summarized. DO NOT submit any full-scale (>4-task) array — scaling requires explicit human approval per the pilot-before-scale rule; halt with the consolidated go/no-go table. Stop after 30 turns regardless and report status.
```

## G4 — Integration PR (HUMAN-GATED — no `/goal`)

Do **not** create a `/goal` for this. Opening/merging PRs is human-gated
(`feature-branch-workflow`, `pr-watch-and-merge-protocol`). When G0/G1/G2/G3 are
green, surface a ready-to-open PR summary (base `dev`, or rebased once PR #30
merges) with the full test evidence, and wait for the human.

---

## Recommended unsupervised run order

1. **`/goal` G0** — clears the CRAN blockers (fully autonomous, ~safe & reversible).
2. **`/goal` G2** — builds + pilots the Tier-B suite, auto-halting at the go/no-go.
3. *(human)* approve the Hopper scale-ups G2 surfaced; run them.
4. **`/goal` G3** — writes the vignette + docs once the suite exists.
5. *(human)* open the PR (G4).

Phase 1 (Tier-A guards) is already ✅ complete (commit dced161) — no goal needed.

## If you want G2 to run FULLY unattended (past the pilot gate)

That requires overriding the standing pilot-before-scale + ask-before-Hopper
rules — an explicit, deliberate choice. If granted, replace G2's "DO NOT submit
any full-scale array … halt" clause with a bounded-scale clause naming the exact
array size, walltime, and a hard core-hour cap, plus "abort and summarize if any
task fails or MaxRSS exceeds the envelope." Do this only with eyes open.
