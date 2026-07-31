# SPEC: Fail-Loud Gates for Chunked Runs (seed audit, output gating, provenance, pilot control)

- **Issue:** #34
- **Status:** Draft
- **Created:** 2026-07-31
- **Author:** medsim maintainer (drafted from #34 + code trace + advisor review)
- **Type:** Enhancement (integrity enforcement layer over the 0.4.0 seeding fix)
- **Refs:** #28 (planning workflow — Part B **subsumes** its unshipped Part B), #25 (failure *reporting*; this is failure *gating*), #27 (truth-cache collision — same "silent wrong data" class), Morris/White/Crowther (2019)

---

## Problem

medsim 0.4.0 fixed chunk-seed collapse by seeding each replication from
`(scenario_name, global_rep_id)` ([`runner.R:373`](../../R/runner.R)). Nothing
currently *detects* a reintroduction of that class of bug: the 0.3.1 failure
produced ~17 distinct outcomes out of 1000 **while every SLURM chunk exited 0**.
#34 proposes four fail-loud gates so the fix is regression-proof.

### Spine finding (not stated in the issue): `global_rep_id` is never persisted

`global_rep_id` is computed for `set.seed()` only and thrown away. The result
row stores the **local** chunk rep id:

- [`runner.R:373`](../../R/runner.R) — `global_rep_id <- (config$rep_offset %||% 0L) + rep_id` (seeding)
- [`runner.R:402`](../../R/runner.R) — `replication = rep_id` (**local**, written to `$results`)
- [`cluster.R:125`](../../R/cluster.R) — `medsim_run_chunk()` sets `chunk_config$n_replications <- length(indices)`, so every chunk emits `replication = 1..chunk_size`
- [`cluster.R:191`](../../R/cluster.R) — `medsim_combine_chunks()` does a plain `rbind`

**Consequence:** the combined frame has **colliding** rep ids and **no global
id at all**. Parts A and D of #34 are unimplementable as written until a
`global_rep_id` column exists — A keys its distinctness audit on it, and D needs
it to identify "reps `1..B_pilot`."

**Adjacent real bug this surfaces:** [`analyze.R:236`](../../R/analyze.R) computes
`n_replications = max(results$results$replication)`. For a combined chunk run
this returns the **chunk size**, not the total `nsim` — a silent under-count in
every summary. (Coverage itself is unaffected: it merges by `scenario` and takes
a proportion over rows, never keying on `replication`.)

---

## Goals

1. **Persist `global_rep_id`** in `$results` (prerequisite for A + D; also fixes the `n_replications` under-count).
2. **A — combine-step seed-provenance audit**: hard-stop on duplicate global rep ids and on the collapse signature, per cell.
3. **B — output-existence gating** in `medsim_write_submit_script()`: make silent-empty-success impossible (subsumes #28B's throttle/requeue/pipefail).
4. **C — provenance header per chunk** + single-SHA assertion at combine.
5. **D — pilot-subset positive control**: assert a full run's reps `1..B_pilot` match an archived pilot within tolerance.
6. **Never break legacy chunk files** — missing fields degrade to a skipped gate + warning, never a stop.

## Non-Goals

- Not a documentation task (#28) or a reporting/summary task (#25).
- No change to the seeding contract itself (0.4.0, done) — only detection around it.
- Not a general provenance/experiment-tracking framework — four targeted gates only.
- D does **not** promise cross-platform byte-equality (see Design D).

---

## Design

Dependency order (build A's prerequisite first; B/C are independent and
individually shippable):

```
prerequisite (global_rep_id column)
   ├── A (combine audit)  ── needs global_rep_id
   └── D (pilot control)  ── needs global_rep_id
B (shell template)        ── fully independent
C (provenance header)     ── independent; same "add fields" class as prerequisite
```

### Prerequisite — persist `global_rep_id`

One-line addition in `medsim_run_single_replication()` result frame
([`runner.R:400`](../../R/runner.R)):

```r
result_df <- data.frame(
  scenario      = scenario$name,
  replication   = rep_id,          # local (unchanged — existing tests depend on it)
  global_rep_id = global_rep_id,   # NEW: true position in 1..nsim
  elapsed       = elapsed_time,
  stringsAsFactors = FALSE
)
```

Also fix [`analyze.R:236`](../../R/analyze.R): `n_replications` becomes
`dplyr::n_distinct(global_rep_id)` when the column is present, falling back to
`max(replication)` when it is absent (legacy frames).

### A — combine-step seed-provenance audit

New internal `.medsim_audit_seed_provenance(results, on_violation)` called from
both `medsim_combine_chunks()` and `medsim_check_results()`. Per `(scenario)`
cell:

1. **Duplicate rep ids** — `n_distinct(global_rep_id) == nsim`. A duplicate =
   two chunks claimed the same rep = a chunking bug.
2. **Collapse signature** — on a **named, continuous** estimate column only
   (default `"estimate"`; configurable via `collapse_col`), assert
   `n_distinct(round(x, 12)) > 0.9 * n_ok`. **Never** applied to discrete
   fields (`branch_switch`, `converged` — 0/1 per the `method()` contract) which
   would false-positive on correct data. If `collapse_col` is absent from the
   frame, **skip this sub-check with a warning** — do not stop.

**Legacy degradation:** if `global_rep_id` is absent, skip check 1 with a warning
naming the fix (re-run under ≥ this version). Never stop on a missing column.

### B — output-existence gating (subsumes #28B)

Rewrite the `medsim_write_submit_script()` template
([`cluster.R:60`](../../R/cluster.R)) to emit, in order:

1. module-init sourcing (`/etc/profile.d/modules.sh` fallback chain),
2. `module load <r_module>` with hard `exit 1` on failure (**never** `|| true`),
3. `command -v Rscript >/dev/null || exit 1`,
4. `#28B` hardening: `set -euo pipefail`, `--requeue`, `%N` throttle line,
5. `Rscript <run_script>`; capture and propagate its exit code,
6. **final line:** `[ -s "$chunk_out" ] || exit 1`.

The chunk filename convention (`chunk_%04d.rds`) currently lives only in
`medsim_run_chunk()` ([`cluster.R:132`](../../R/cluster.R)). Centralize it in an
internal `.medsim_chunk_filename(chunk_id)` that **both** `medsim_run_chunk()`
and the template use, so the `[ -s ... ]` path can't drift from the writer.

**#28B is subsumed here**, not extended — implementing a 6-line template split
across two issues would drift. Note #28B as folded into this SPEC.

### C — provenance header per chunk

`medsim_run_chunk()` attaches `attr(results, "provenance")`:
`list(r_version, medsim_version, dep_versions, hostname, code_sha, sec_per_rep)`.
`code_sha` is **caller-stamped** (a `code_sha` arg on `medsim_run_chunk()`,
defaulting to `NA`). At combine, assert a **single non-NA SHA across all chunks**
(catches a mid-run code edit + partial resubmit). When SHA is `NA` (interactive
run, no checkout) → skip the SHA assertion with a warning, never stop. A
documented deliberate resubmit uses `on_violation = "warn"`.

### D — pilot-subset positive control

`medsim_combine_chunks(pilot_reference = <path>, pilot_tol = 1e-9)`: join the
full run's rows with `global_rep_id %in% pilot$global_rep_id` against the pilot
by `(scenario, global_rep_id)` and assert per-column agreement within
`pilot_tol`. **Tolerance, not byte-equality** — the 1e-12 field match was one
FORK-reproducible run; exact `identical()` will fail across a different
BLAS/R build on *correct* code. D's scope inherits `.STATUS`'s "FORK-reproducible"
contract. Default `pilot_tol = 1e-9`; document that a genuinely reordered/rebuilt
environment may need a looser tol or `on_violation = "warn"`.

### Cross-cutting — one control, not four switches

Single `on_violation = c("stop", "warn", "ignore")` argument threaded through A,
C, D (default `"stop"`). Four independent opt-outs is how a gate ends up
silently disabled. The existing `expected_chunks` soft-warning
([`cluster.R:180`](../../R/cluster.R)) folds under the same control.

---

## API Summary

| Function | Change |
|---|---|
| `medsim_run_single_replication()` | + `global_rep_id` column (internal) |
| `medsim_analyze()` | `n_replications` counts distinct `global_rep_id` (fallback to `max`) |
| `medsim_run_chunk()` | + `code_sha` arg; attaches `provenance` attr; uses `.medsim_chunk_filename()` |
| `medsim_combine_chunks()` | + `pilot_reference`, `pilot_tol`, `on_violation`; runs A + C + D audits |
| `medsim_check_results()` | + runs A audit; `on_violation` |
| `medsim_write_submit_script()` | rewritten template (B, subsumes #28B) |
| `.medsim_audit_seed_provenance()` | NEW internal |
| `.medsim_chunk_filename()` | NEW internal (shared name convention) |

---

## Acceptance Criteria — planted-defect matrix

Each gate must FIRE on its defect **and** NOT fire on the matched negative control:

| Gate | Planted defect ⇒ stop | Negative control ⇒ silent |
|---|---|---|
| A.1 dup rep ids | two rows same `(scenario, global_rep_id)` | distinct ids, full grid |
| A.2 collapse | continuous `estimate` with ~17 distinct/1000 | **discrete `branch_switch`/`converged` 0/1** (must NOT fire) |
| B output gate | zero-byte `chunk_out` ⇒ `exit 1` | non-empty chunk ⇒ exit 0 |
| B module fail | `module load` fails ⇒ `exit 1` (no `|| true`) | module loads ⇒ proceed |
| C SHA | two chunks, different non-NA SHA ⇒ stop | single SHA, or all-NA ⇒ skip+warn |
| D pilot | one perturbed rep vs pilot ⇒ stop | pilot-identical subset within tol |
| legacy | chunk file missing `global_rep_id` ⇒ **warn + skip**, never stop |

**E2E (per `e2e-before-pr.md`):** A/C/D via unit fixtures; B via a **local render
+ stubbed `module`/`Rscript` exit-code harness** (a real cluster run is not
performable — state that in the PR body, do not claim a Hopper run).

---

## Risks

1. **Legacy `.rds` bricking** — the #1 way this breaks real work (pmed-modern archives have no `global_rep_id`/provenance). Mitigation: missing-field ⇒ warn+skip, encoded in acceptance matrix.
2. **Collapse check false-positive** on discrete columns — mitigation: named continuous column only + explicit negative control.
3. **D over-strict tolerance** across environments — mitigation: `pilot_tol` param, documented `warn` escape.
4. **Template coupling** — B duplicates the chunk-name convention; mitigation: shared `.medsim_chunk_filename()`.
5. **`expected_chunks` semantics change** (warn→configurable) is a mild behavior change; document in NEWS.

---

## Grill Ledger

Convergent adversarial interrogation of this SPEC. Each finding → resolution now
folded into the Design above.

| # | Finding (attack) | Resolution |
|---|---|---|
| G1 | **A/D assume a `global_rep_id` column that doesn't exist** — `runner.R:402` stores local `rep_id`; combine `rbind`s colliding ids. Both parts are dead on arrival. | Promoted to **Prerequisite** (persist the column). Made the spec's spine, not a footnote. |
| G2 | **"Hard stop, opt-out not opt-in" bricks every archived chunk** (no `global_rep_id`, no provenance). | Legacy rule: missing field ⇒ **warn + skip that gate, never stop**. In acceptance matrix. |
| G3 | **Collapse signature fires on correct data** for any discrete field (`branch_switch`, `converged` are 0/1 per the `method()` contract). | Applies to a **named continuous column only** (`collapse_col`, default `"estimate"`); discrete negative control is a gating acceptance test. |
| G4 | **D conflates "byte-level" and "1e-12"** — exact equality fails on correct code across a different BLAS/R build; `.STATUS` scopes reproducibility to FORK only. | D asserts **within `pilot_tol` (default 1e-9)**, not `identical()`; scope inherits FORK-reproducibility; `warn` escape documented. |
| G5 | **B's output gate re-hardcodes the chunk filename** that lives only in `medsim_run_chunk()`. | Centralized in `.medsim_chunk_filename()` used by both writer and template. |
| G6 | **"Extends #28B" — #28B doesn't exist yet** (template is a bare `module load`). | B **subsumes** #28B (throttle/requeue/pipefail folded in); one-pass implementation; #28B noted as absorbed. |
| G7 | **Four opt-outs = a gate gets silently disabled.** | Single `on_violation = c("stop","warn","ignore")` threaded through A/C/D + the existing `expected_chunks` warning. |
| G8 | **C's SHA is absent on interactive/non-git runs** and blocks legitimate deliberate resubmits. | `NA` SHA ⇒ skip+warn; deliberate resubmit uses `on_violation="warn"`. |
| G9 (residual) | A.1's distinctness can't detect a bug where a user DGM calls `set.seed()` internally yet rep ids stay distinct — only A.2 (collapse) catches that, and only on the continuous column. | Accepted & documented: A.1 and A.2 are complementary; neither alone is complete. Matches #34's own framing (both checks needed). |

---

## Branch / Worktree Plan (descriptive — not an action item)

Feature-branch work off `dev` (code changes, not docs-only). Parts B and C are
independently shippable; A depends on the `global_rep_id` prerequisite; D depends
on A's column. Natural increment order: prerequisite → A → B → C → D.
