# GRILL: Chunked-Run Fail-Loud Gates (#34) — interactive decision ledger

- **Target spec:** [SPEC-medsim-chunked-run-gates-2026-07-31.md](SPEC-medsim-chunked-run-gates-2026-07-31.md)
- **Issue:** #34
- **Date:** 2026-07-31
- **Mode:** interactive `/craft:grill` (convergent, one branch at a time)
- **Relation to the spec's own Grill Ledger (G1–G9):** that was the *author's* self-grill,
  folded into the design pre-commit. This is an *independent* interactive pass that attacked the
  committed spec's residual decision branches. New findings below extend, not duplicate, G1–G9.
- **Implementation started 2026-07-31** (P1–P3 + Gate A on `feature/chunked-run-prereqs`); note
  B4's dual-column resolution and B9's shell output gate were superseded by the spec v2 architecture decision.

---

## Resolved branches

### B1 — `on_violation` default posture (stop vs warn), at combine time
**Attack:** combine runs *after* hours of Hopper compute; a plain `stop()` on one bad cell
discards the whole run's good cells. Is stop-by-default right?

**Evidence (investigated):**
- medsim already has base-R structured conditions — `medsim_error`
  (`class = c("medsim_error","error","condition")`, [`parallel.R:217`](../../R/parallel.R)). So
  "stop *with data attached*" is idiomatic; `rlang` is not needed (not a dep).
- `medsim_combine_chunks()` already chose **warn-and-return** for the missing-chunk case
  ([`cluster.R:180`](../../R/cluster.R)) — the same "silently-wrong downstream numbers" class.
  Hard-stopping the seed audit while warning the missing-chunk case is internally inconsistent.

**Resolution:** default `on_violation = "stop"`, but stop via a **data-carrying condition**
(`medsim_combine_violation` subclassing `medsim_error`) whose payload is `$results` +
`$violations` — a `tryCatch` recovers the good cells; an unguarded call still fails loud.
Tie-breaker for stop-default: #34's whole thesis is that *warnings were ignored* (every chunk
exited 0), so a warn-default would repeat the exact failure. **Refinement:** fold the existing
`expected_chunks` warn under the *same* `on_violation` control so the two combine-time integrity
checks don't ship with opposite default postures.

### B2 — Part A.2 collapse-check magic numbers (`0.9`, `round(x, 12)`)
**Attack:** both constants mis-fire — `round(x,12)` false-collapses a bounded/clamped estimator
or an at-boundary p-value (legit exact ties); `0.9` is too strict for a small cell (n_ok=20 needs
18 distinct).

**Resolution:** expose `collapse_threshold` (default 0.9) and `collapse_digits` (default 12);
**skip the check when `n_ok < floor` (e.g. 30)** where the ratio is too noisy to diagnose;
document the 0.3.1 signature (~17 distinct / 1000) as the calibration basis. Keep A.2 — it is the
*only* check that catches a user DGM calling `set.seed()` internally (rep ids stay distinct,
outputs collapse).

### B3 — Part D pilot-match identity assumption
**Attack:** draw-identity on `(scenario, global_rep_id)` also requires **pilot-n == full-run-n**
and identical DGM. The seed ignores n, so a pilot at a different n seeds identically but draws a
different-length sample → values differ for *correct* code → false "seeding regression" alarm.

**User input:** pilots are small-n (n < 1000; n=200 fine).

**Resolution:** absolute n stays the user's choice; the correctness *invariant* is
**pilot-n == full-n for the compared cell**. Store `n` + a scenario fingerprint (reuse
`.medsim_truth_fingerprint`) in the pilot; D **asserts identity first**, then value-matches within
`pilot_tol`. A mismatched-config pilot fails loud ("pilot config differs") instead of masquerading
as a regression. Guards the future P4-Wasserstein reuse where n could diverge.

### B4 — Prerequisite scope + schema blast radius
**Attack:** "persist `global_rep_id`" is not one line. Also needs: register it in `metadata_cols`
([`analyze.R:109`](../../R/analyze.R)) or it silently becomes a fake **estimate column** (line 110
`setdiff`); fix `n_replications` ([`analyze.R:236`](../../R/analyze.R), currently
`max(replication)` → wrong for combined runs); and existing `.rds` fixtures + pmed-modern archives
carry the old (column-less) schema. Tests use `%in%` membership, so they would NOT catch the
metadata_cols mis-classification.

**Resolution:** **additive + tolerate-absent everywhere** — add the column, register it in
`metadata_cols`, and make `n_replications` + every reader treat `global_rep_id` as *optional*
(present → use it; absent → fall back to `max(replication)`). No fixture regen; old and new
results both work. Encodes G2's legacy tolerance at the schema layer.

### B5 — Part C SHA benefit honesty (caller-stamped, default NA)
**Attack:** a caller-stamped SHA defaulting to `NA` means the single-SHA assertion is **inert**
whenever a caller forgets to stamp (all-NA → skip, per G8). The headline benefit is asserted, not
delivered, for the common case.

**Resolution:** `medsim_run_chunk()` **auto-detects** the SHA (git `rev-parse HEAD` of the
run-script dir, or `packageVersion` + a hash of the loaded medsim namespace when not in a git
tree) when `code_sha` is `NA`; caller override retained for installed-package/non-git runs. The
other provenance fields (R version, package versions, hostname, sec/rep) auto-populate regardless
and carry forensic value even when SHA is `NA`.

---

## New finding folded in (not a branch)

- **`metadata_cols` mis-classification** ([`analyze.R:109`](../../R/analyze.R)): the prerequisite
  MUST add `global_rep_id` to `metadata_cols`, else it lands in `estimate_cols` and gets coverage/
  accuracy computed on it. Membership-style tests (`%in%`) will not catch this — needs an explicit
  test asserting `global_rep_id` is NOT in `estimate_cols`.

## Open questions (carried, not blocking a plan)

- **A.2 `n_ok` floor value** — 30 is a placeholder; pick from the smallest production cell size.
- **`pilot_tol` default** — 1e-9 vs a looser default; depends on the estimators' conditioning.
- **B's `set -euo pipefail`** interaction with existing `module` shells on Hopper — verify the
  fallback chain doesn't trip `nounset` on unset SLURM vars.

---

## Handoff

Locked decisions feed `/craft:plan` (tier 4 plan-orchestrator) → `ORCHESTRATE-*.md` → `/craft:do`.
Implementation increment order (from the spec, now hardened): prerequisite (additive, tolerate-absent)
→ A (data-carrying stop + configurable A.2) → B (subsumes #28B, shared `.medsim_chunk_filename()`)
→ C (auto-detect SHA) → D (assert pilot identity first).

---

## Resolved branches (continued — second pass)

### B6 — A.2's `collapse_col` default matches no real column
**Attack:** the spec defaults `collapse_col = "estimate"`, but medsim has **no consistent
`estimate` column**. Real estimate columns are method-dependent: `indirect` (+ the CLAUDE.md
6-field `method()` contract), `pmed`/`w` ([`methods_gauge.R:73`](../../R/methods_gauge.R)),
`pmed_sobol`. Only a doc *example* in CLAUDE.md uses `estimate =`. So A.2 would match nothing and
**silently skip by default** — the same inert-gate trap as B5's SHA.

**Resolution:** drop the named-column default. Run the collapse check on **every column in
`estimate_cols` that is continuous** — numeric with `> 2` distinct values across the run. This
auto-covers `indirect`/`pmed`/`pmed_sobol` (and gauge's *two* estimates, which a
pick-the-first heuristic would half-miss), and the `> 2 distinct` test **structurally** excludes
discrete `branch_switch`/`converged` — folding G3's manual exclusion into the selection rule
instead of a maintained denylist.

### B7 — A.1 has no trustworthy source for `nsim`
**Attack:** two gaps. (1) At combine time there is no total-`nsim` to compare against: combine
sees only `rbind`'d rows, and `config$n_replications` *inside* a chunk file is the **chunk** size
([`cluster.R:125`](../../R/cluster.R)), not the total. (2) `n` (sample size) lives in `config`, not
the scenario, so in a scenario×n grid the same scenario name at different `n` would collapse into
one "cell" if ever combined in a single dir.

**Verified topology:** every chunk loops all scenarios, each emitting reps `1..n_replications`
([`runner.R:196`](../../R/runner.R)), so a cell == one scenario — A.1's grouping by `scenario` is
correct as written.

**Resolution:** make A.1 **self-validating** rather than externally parameterized. Per cell assert
(a) `global_rep_id` forms a **contiguous `1..max` run with no gaps and no duplicates**, and (b)
`max` is **identical across all cells** in the combined frame. This detects duplicate ids, missing
chunks, and ragged cells with zero external input. Accept an optional explicit `nsim=` to
additionally pin the absolute total. Explicitly **rejected**: reconstructing nsim by summing each
chunk's `config$n_replications` — that is the same chunk bookkeeping that collapsed in 0.3.1, and
a missing chunk would lower the expected total to match the truncated data, silently passing the
very failure the gate exists to catch.

**Carried:** if a scenario×n grid ever shares one `output_dir`, cells must be disambiguated by
`(scenario, n)` — mirrors the #27 truth-cache collision (same root cause: identity keyed too
coarsely). Today's convention (`nN/` dirs per n) avoids it.

### B8 — Part B's module-init mechanism contradicts the repo's own field evidence
**Attack:** #34-B proposes module init by sourcing `/etc/profile.d/modules.sh` (fallback chain).
But this repo already ships a field-tested Hopper script,
[`inst/hopper-tests/submit_chunk.sh:1`](../../inst/hopper-tests/submit_chunk.sh), using
**`#!/bin/bash -l`** with an explicit comment that a *login* shell is **required** on Hopper
because `module` is a function sourced only by login-shell init. Meanwhile the *generated*
template ([`cluster.R:61`](../../R/cluster.R)) emits plain `#!/bin/bash` — which by that same
comment fails with "module: command not found."

**Resolution:** use **`#!/bin/bash -l`**, the mechanism already proven in this repo; keep a
source-fallback only as a secondary guard. **This is also a latent-bug fix, not just hardening:**
the template medsim generates today would fail on Hopper in exactly the silent-failure mode
#34-B was written to prevent.

### B9 — B's output gate references a path the shell cannot know
**Attack:** the proposed final line `[ -s "$chunk_out" ] || exit 1` has no `chunk_out` in scope.
`output_dir` lives in the **R** config, and `run_script` is **user-authored** — medsim never sees
its contents ([`inst/hopper-tests/run_chunk.R`](../../inst/hopper-tests/run_chunk.R) is the user's
file, not a shipped template). The emitted shell script cannot know where the chunk landed.

**Resolution:** `medsim_write_submit_script()` already **has** the config at write time — bake the
resolved path into the emitted script:
`chunk_out="<output_dir>/$(printf 'chunk_%04d.rds' $SLURM_ARRAY_TASK_ID)"`, built through the
shared `.medsim_chunk_filename()` convention (B5/G5) so writer and gate cannot drift.
Self-contained; no coupling to the user's `run_script`. **Documented caveat:** breaks if the user
overrides `output_dir` inside their own `run_script` — state this in the roxygen.

---

## Grill closed — 9 branches resolved

Every part of #34 (prerequisite, A, B, C, D) now has its load-bearing assumptions resolved.
Three findings were **latent bugs**, not just design hardening:

1. `global_rep_id` would be mis-classified as an estimate column (`analyze.R:109`) — B4.
2. `n_replications` under-counts for combined chunk runs (`analyze.R:236`) — B4.
3. The generated submit template fails on Hopper today (plain `#!/bin/bash`) — B8.

Two proposed gates would have shipped **inert** without this pass: A.2 (`collapse_col="estimate"`
matches no real column — B6) and C's SHA assertion (all-NA → always skips — B5).
