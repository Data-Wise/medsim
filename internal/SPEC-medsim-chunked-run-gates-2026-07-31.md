# SPEC: Fail-Loud Gates for Chunked Runs (seed audit, output gating, provenance, pilot control)

- **Issue:** #34
- **Status:** **IMPLEMENTED** — all increments (P1–P3, Gates A–D) landed on `feature/chunked-run-prereqs` (`dde3692` → `0e3b1b8`; suite 1077 PASS / 0 FAIL / 1 SKIP; `--as-cran` 0/0/1-known-NOTE). (v2 amended 2026-07-31 pm after independent grill + 8-angle adversarial review)
- **Created:** 2026-07-31 · **Amended:** 2026-07-31
- **Author:** medsim maintainer (drafted from #34 + code trace; hardened by grill B1–B9 + review R1–R10)
- **Refs:** #28 (Part B **subsumes** its unshipped Part B), #25 (reporting vs gating), #27 (same "silent wrong data" class), #36 (rep-id collision — the prerequisite here IS its fix), #37 (Hopper shebang — fixed by Part B), #38 (chunk CSV clobber — fixed by Prerequisite 3), Morris/White/Crowther (2019)
- **Companion ledgers:** [GRILL-medsim-chunked-run-gates-2026-07-31.md](GRILL-medsim-chunked-run-gates-2026-07-31.md) (interactive grill, B1–B9); 26-finding adversarial review (2026-07-31, reported in-session — 19 CONFIRMED, 7 empirically reproduced)

---

## Problem

medsim 0.4.0 fixed chunk-seed collapse by seeding each replication from
`(scenario_name, global_rep_id)` ([`runner.R:373`](../../R/runner.R)). Nothing
*detects* a reintroduction: the 0.3.1 failure produced ~17 distinct outcomes out
of 1000 **while every SLURM chunk exited 0**. #34 adds four fail-loud gates.

The grill + review found the gates' foundations are missing or broken in current
`dev` — five confirmed bugs (all empirically reproduced) that this spec now
fixes as prerequisites:

| Bug | Where | Symptom |
|---|---|---|
| Local rep ids collide across chunks; no global id persisted (#36) | `runner.R:402` | 20-rep/4-chunk run: 5 distinct `replication` values, each ×4 |
| `medsim_analyze()`/`print()` report chunk size as `n_replications` (#36) | `analyze.R:236` | reports 5 for a true nsim of 20 |
| Combine returns chunk-1's stale `$summary`/`$config` | `cluster.R:204` | quarter-run statistics labeled as the study |
| One failed replication crashes the whole run (ragged `rbind`) | `runner.R:233` | `names do not match previous names`; in chunk mode → missing chunk file |
| Logical contract fields silently dropped (`branch_switch`, `converged`) | `runner.R:410` | `medsim_summarize_branch_switch()` stops on missing column |

---

## Architecture decision (locked 2026-07-31): ONE global `replication` column

The grill (B4) initially locked an *additive* design — keep local `replication`,
add `global_rep_id` beside it. The review challenged this (two finders,
independently), and a code check settled it: **the justification was inverted**.
Existing tests do not depend on local ids — [`test-cluster.R:412-414`](../../tests/testthat/test-cluster.R)
*works around* them ("cannot align rows to a common global order across chunks"),
and [`test-cluster-edge-cases.R:31`](../../tests/testthat/test-cluster-edge-cases.R)
documents the repeat as a known wart. `test-runner.R:360` asserts at
`rep_offset = 0`, where global == local.

**Decided:** `replication` itself becomes the **global** rep id (offset applied at
write time). No second column, no permanent reader fallback, no
`metadata_cols` change (`replication` is already registered). Trade-off accepted:
a one-time sweep of the few chunk-path tests versus a forever-branched schema in
which every consumer must know which of two id columns is safe to key on.

Schema versioning: the runner stamps `attr(results$results, "medsim_schema") <- 2L`
(v1 = local ids, absent attribute). Readers treat absent/`1L` as legacy.

---

## Goals

1. **Prerequisites (fix the confirmed bugs):** global `replication`; combine rebuilds `$summary`/`$config`; runner-owned failure rows (no ragged rbind, no logical drop); no chunk CSV clobber.
2. **A — combine-step audit:** self-validating rep-id contiguity + collapse signature + cross-scenario seed-collision check; hard-stop by default via a *data-carrying* condition.
3. **B — hardened submit template** (subsumes #28B, fixes #37): login shell, hard-fail module load, propagated exit codes; **completeness gating lives in the combiner, not the shell**.
4. **C — provenance header** per chunk with **auto-detected** code SHA; single-SHA assertion at combine.
5. **D — pilot-subset positive control:** identity-asserted, estimate-columns-only, tolerance-based.
6. **Never brick legacy artifacts** — schema-absent inputs degrade to warn + skip, never stop.

## Non-Goals

- Not documentation (#28 Part A) or reporting (#25). No change to the 0.4.0 seeding contract itself.
- No general experiment-tracking framework.
- D does not promise cross-platform byte-equality (FORK-reproducibility scope, per `.STATUS`).

---

## Design

Dependency order:

```
P1 global replication ──► A (audit)  ──► D (pilot control)
P2 combine rebuild     ──► A
P3 failure rows + CSV  (independent)
B  (template)          (independent; fixes #37)
C  (provenance)        (independent)
```

### Prerequisite P1 — `replication` becomes global (fixes #36)

[`runner.R:402`](../../R/runner.R): `replication = (config$rep_offset %||% 0L) + rep_id`
(the same expression the seeding already uses at line 373 — hoist to one source
of truth). Stamp `medsim_schema = 2L` attribute. Standalone runs
(`rep_offset = 0`) are byte-identical to today.

- [`analyze.R:236`](../../R/analyze.R): `n_replications = max(replication)` is now
  *correct* for combined runs — no fallback branch needed.
- **Column-provenance attribute (review R8, root-cause fix):** the runner also
  stamps `attr(results$results, "medsim_meta_cols") <- c("scenario", "replication", "elapsed")`.
  `medsim_analyze()` and gate A.2 consume the attribute when present, falling
  back to the hardcoded name list for legacy frames. Future bookkeeping columns
  can no longer silently become "estimates" (the B4 misclassification class).
- Test sweep: update the local-id workaround at `test-cluster.R:412` and the
  edge-case comment at `test-cluster-edge-cases.R:31`; add a test asserting
  chunk 2's first row has `replication = chunk_size + 1`.

### Prerequisite P2 — combine rebuilds metadata (confirmed bug)

[`cluster.R:204`](../../R/cluster.R): after merging, `medsim_combine_chunks()`
must (a) recompute `$summary` over the combined frame via
`medsim_summarize_results()`, (b) set `config$n_replications` to the combined
distinct-rep count and drop `config$rep_offset`/`chunk_id`, (c) keep
`n_chunks_combined`. Never return chunk-1's slice statistics as the study.

### Prerequisite P3 — runner-owned failure rows + no CSV clobber (fixes #38)

Two empirically-confirmed crashers live in the exact lines P1 touches; fixing
them here is not scope creep — a chunk that *crashes* on one transient rep
failure manufactures a missing chunk file, the opposite of a fail-loud run:

- **Failure schema** ([`runner.R:385-414`](../../R/runner.R)): on method error,
  synthesize a row with `NA` for the estimate columns (observed from successes
  or declared via the estimand descriptor), plus `converged = 0` and an `error`
  string column present on *every* row (`NA` on success) — all rows share one
  schema, `rbind` cannot crash, and adapters stop hand-rolling private NA
  templates (three exist today in `methods_missing.R` alone).
- **Logical fields** ([`runner.R:410`](../../R/runner.R)): widen the filter to
  `is.numeric() || is.character() || is.logical()` so the documented 6-field
  contract (`branch_switch`, `converged`) actually reaches `$results`.
- **CSV clobber (#38):** when `config$chunk_id` is set, skip the intermediate
  per-scenario/summary CSV writes entirely — the chunk `.rds` is the artifact;
  n_chunks concurrent tasks overwriting `results_scenario_1.csv` is wasted I/O
  plus a partial-data-labeled-as-complete trap.

### A — combine-step audit (`.medsim_audit_seed_provenance()`)

Runs inside `medsim_combine_chunks()` **only** — NOT `medsim_check_results()`,
which takes a *list of parallel task results*, not a `medsim_results` object
(review R1; wiring the audit there would silently no-op). Standalone entry
point: a new exported `medsim_audit_results(results, ...)` thin wrapper for
auditing an already-combined object.

Per `(scenario)` cell:

1. **A.1 contiguity (exact, self-validating — grill B7):** `replication` values
   form a contiguous `1..max` run, no gaps, no duplicates; `max` identical
   across cells. Catches duplicate ids, missing chunks, and ragged cells with
   zero external input. Optional `nsim =` pins the absolute total. **Never**
   derive expected counts from chunk-file configs (`config$n_replications`
   inside a chunk file is the chunk size — review R2's 100%-false-positive trap)
   nor by summing chunk metadata (circular: a missing chunk shrinks the
   expectation to match the truncated data).
2. **A.2 collapse signature (heuristic — grill B2/B6, review R5):** for **every
   continuous estimate column** — the `medsim_meta_cols` attribute complement
   (legacy: name-subtraction), keeping numeric columns with > 2 distinct values
   (structurally excludes 0/1 `branch_switch`/`converged`) — assert
   `n_distinct(round(x, collapse_digits)) > collapse_threshold * n_ok`.
   Defaults `collapse_threshold = 0.9`, `collapse_digits = 12` (calibration:
   the 0.3.1 signature, ~17/1000). **Skip when `n_ok < 30`** (small-cell noise).
   **`n_ok == 0` is its own violation type** (`cell_failed`), never reported as
   collapse (review R5: `0 > 0` is FALSE — the naive check would misreport an
   all-failed cell as seed collapse).
3. **A.3 cross-scenario seed collision (exact, zero storage — review R9):**
   recompute `.medsim_det_seed(scenario, replication)` per cell×rep (vectorized)
   and assert no two *scenarios* share a seed sequence — detects the
   hash-bucket collision residual ([`runner.R:342`](../../R/runner.R), 1000003
   buckets) that A.1 cannot see. Within-scenario duplicate seeds are already
   A.1 duplicates (seeds are deterministic in `(scenario, replication)`).

**Legacy degradation:** schema-absent frames (local ids) → skip A.1/A.3 with a
warning naming the re-run fix; A.2 still runs (column selection falls back to
name-subtraction). Never stop on a missing column/attribute.

### Cross-cutting — `on_violation` + data-carrying condition (grill B1)

One control threaded through combine/audit: `on_violation = c("stop", "warn", "ignore")`,
default `"stop"` (#34's mandate — the 0.3.1 warnings were ignored; every chunk
exited 0). "Stop" signals a **`medsim_combine_violation`** condition (subclassing
the existing `medsim_error`, [`parallel.R:217`](../../R/parallel.R)) carrying
`$results` (the combined object) and `$violations` — a `tryCatch` recovers an
hours-long run's good cells; an unguarded call still fails loud. Base-R
conditions only (`rlang` is not a dependency).

The existing `expected_chunks` check folds under the same control (grill B1
refinement). **Consequences handled explicitly (review R4):**
`tests/testthat/test-cluster-edge-cases.R:47` (expects a warning on a missing
chunk) is updated in-PR to expect the condition / pass `on_violation = "warn"`;
the interim-look workflow (combining 58/60 chunks mid-run) is documented as
`medsim_combine_chunks(out, on_violation = "warn")` — an explicit one-argument
opt-out for a deliberate partial combine, with partial results still returned.
NEWS documents the behavior change.

### B — hardened submit template (subsumes #28B; fixes #37)

Rewrite of the emitted script ([`cluster.R:60`](../../R/cluster.R)):

1. **`#!/bin/bash -l`** (grill B8) — the mechanism this repo already field-tested
   on Hopper ([`inst/hopper-tests/submit_chunk.sh:1`](../../inst/hopper-tests/submit_chunk.sh):
   "`module` is a function sourced only by login-shell init"). The current plain
   `#!/bin/bash` template **fails on Hopper today** (#37). A
   `source /etc/profile.d/modules.sh` fallback remains as a secondary guard only.
2. `set -euo pipefail` + `#SBATCH --requeue` + `%N` array throttle (#28B, folded).
3. `module load <r_module>` with hard `exit 1` on failure — never `|| true`.
4. `command -v Rscript >/dev/null || exit 1`.
5. `Rscript <run_script>`; propagate its exit code.

**No shell-side output-path gate.** The grill's `[ -s "$chunk_out" ]` line (B9)
is **withdrawn** (review R3): the baked path comes from the *writer's* config
while the chunk path is resolved at runtime by the run script's *own* config —
the two routinely differ (`inst/hopper-tests` reads `TIER_B_OUTDIR`), so the
gate would `exit 1` on every successful task and, with `--requeue`, loop
requeuing successful work. Completeness is the **combiner's** job (A.1 catches
missing/short chunks exactly; unreadable RDS fails at `readRDS`). The historical
`COMPLETED 0:0, no output` mode is closed by items 1–5: `medsim_run_chunk()`
either `saveRDS`es or errors → nonzero exit → propagated.

The chunk filename convention still centralizes in `.medsim_chunk_filename(chunk_id)`
(used by `medsim_run_chunk()` and `medsim_combine_chunks()`'s default pattern).

**Exemplar sweep (review R7):** `inst/hopper-tests/submit_chunk.sh` and its
README are updated to the hardened pattern in the same PR — the copy users
actually start from must not reproduce the unhardened template.

### C — provenance header per chunk

`medsim_run_chunk()` attaches `attr(results, "provenance")`:
`list(r_version, medsim_version, dep_versions, hostname, code_sha, sec_per_rep)`.
`code_sha` **auto-detects** (grill B5): `git -C <run-script dir> rev-parse HEAD`
when in a git tree, else a `packageVersion("medsim")`-based tag; an explicit
`code_sha =` argument overrides (installed-package / non-git runs). At combine,
assert a single non-NA SHA across chunks (catches a mid-run edit + partial
resubmit); all-NA → skip + warn; deliberate resubmits use `on_violation = "warn"`.

### D — pilot-subset positive control

`medsim_combine_chunks(pilot_reference = <path>, pilot_tol = 1e-9)`:

1. **Identity first (grill B3):** the pilot artifact stores `n`, the scenario
   fingerprint (reuse `.medsim_truth_fingerprint()`), and its schema version.
   Assert pilot-n == full-run-n per compared cell and matching fingerprints — a
   mismatched-config pilot fails loud as `pilot_config_differs`, never
   masquerading as a seeding regression. (Absolute pilot size stays the user's
   choice — small-n pilots are fine; the invariant is *equality*, not size.)
2. **Value match — estimate columns ONLY (review R6):** join on
   `(scenario, replication)`; compare the estimate-column set (from the
   provenance attribute), explicitly excluding `elapsed` (wall time never
   matches) and all metadata. The v1 text compared all columns — a guaranteed
   false positive on every correct run.
3. Tolerance `pilot_tol`, not `identical()` (grill G4): FORK-reproducibility
   scope; document the looser-tol / `warn` escape for rebuilt environments.

---

## API Summary

| Function | Change |
|---|---|
| `medsim_run_single_replication()` | `replication` = global id; schema + meta-cols attributes; failure rows share success schema; logical fields kept |
| `medsim_run_chunk()` | provenance attr (auto-SHA + `code_sha` override); skips intermediate CSVs; uses `.medsim_chunk_filename()` |
| `medsim_combine_chunks()` | rebuilds `$summary`/`$config`; runs A.1–A.3 + C + D; `on_violation`, `nsim`, `pilot_reference`, `pilot_tol`, `collapse_*` args |
| `medsim_audit_results()` | NEW export: standalone audit of a combined object |
| `medsim_analyze()` | consumes `medsim_meta_cols` attr (legacy fallback) |
| `medsim_write_submit_script()` | hardened template (B); no path gate |
| `.medsim_audit_seed_provenance()`, `.medsim_chunk_filename()` | NEW internals |

**Not** touched: `medsim_check_results()` (wrong input shape for this audit — review R1).

---

## Acceptance Criteria — planted-defect matrix

Each gate must FIRE on its defect and stay SILENT on the matched negative control:

| Gate | Planted defect ⇒ violation | Negative control ⇒ silent |
|---|---|---|
| P1 | 4-chunk/nsim-20 combine ⇒ 20 distinct `replication`, `n_replications = 20` | standalone run unchanged (byte-identical at `rep_offset = 0`) |
| P2 | combined `$summary` recomputed over all rows | single-chunk combine |
| P3 | method failing on rep k ⇒ run completes, NA row, `converged = 0`, `rbind` succeeds | all-success run identical to today (+ `error` col NA) |
| P3 | `branch_switch = NA` (logical) survives to `$results` | — |
| A.1 | duplicated rep id ⇒ stop; deleted chunk ⇒ gap ⇒ stop | full contiguous grid |
| A.2 | continuous `indirect` with ~17/1000 distinct ⇒ stop | discrete 0/1 `branch_switch` (must NOT fire); `n_ok < 30` cell skipped; `n_ok == 0` ⇒ `cell_failed`, NOT collapse |
| A.3 | two scenario names forced into one hash bucket ⇒ stop | distinct buckets |
| B | `module load` failure ⇒ task exit ≠ 0; `Rscript` error ⇒ exit ≠ 0 | clean run exits 0 |
| C | two chunks, different non-NA SHA ⇒ stop | single SHA; all-NA ⇒ skip + warn |
| D | one perturbed estimate vs pilot ⇒ stop; pilot at different n ⇒ `pilot_config_differs` | pilot-identical subset within tol (`elapsed` differing must NOT fire) |
| legacy | schema-absent chunk ⇒ warn + skip A.1/A.3, never stop |
| condition | `tryCatch(medsim_combine_violation)` recovers `$results` from a stopped combine |

**E2E (per `e2e-before-pr.md`):** A/C/D + P1–P3 via unit fixtures (the P1/P2/P3
defects are already reproduced in-session — the fixtures encode those
transcripts); B via local render + stubbed `module`/`Rscript` exit-code harness.
A real Hopper run is not performable from this environment — the PR body states
this; `inst/hopper-tests/` remains the on-cluster validation path.

---

## Risks

1. **P1 blast radius** — global ids change chunk-frame contents. Mitigated: standalone runs byte-identical; the only affected tests are the ones working around the old behavior; schema attribute + legacy warn+skip protect archived `.rds`.
2. **`on_violation` default flips `expected_chunks` semantics** — a documented breaking change (NEWS), test updated in-PR, interim-look opt-out documented (review R4).
3. **A.2 false positives** — configurable knobs + small-cell floor + structural discrete exclusion + matrix negative controls.
4. **Auto-SHA wrong tree** — detect the *run-script's* dir, not `getwd()`; override retained; all-NA degrades to warn.
5. **D pilot staleness** — identity assert converts silent staleness into a loud `pilot_config_differs`.
6. **Exemplar drift** — `inst/hopper-tests` updated in-PR (review R7); `.medsim_chunk_filename()` keeps writer/combiner naming coupled.

---

## Grill Ledger (v1, retained) — G1–G9

| # | Finding | Resolution (as amended) |
|---|---|---|
| G1 | A/D assume a `global_rep_id` column that doesn't exist | Promoted to prerequisite; **v2: single global `replication` column** (Architecture decision) |
| G2 | Hard-stop bricks archived chunk files | Legacy rule: schema-absent ⇒ warn + skip, never stop |
| G3 | Collapse check fires on discrete fields | v2: structural exclusion via "> 2 distinct values" in auto column selection |
| G4 | D conflates byte-level and 1e-12 | `pilot_tol`, FORK scope, documented escapes |
| G5 | B re-hardcodes the chunk filename | `.medsim_chunk_filename()` shared by writer + combiner |
| G6 | "#28B" doesn't exist to extend | B subsumes it one-pass |
| G7 | Four opt-outs → silent disable | Single `on_violation` control |
| G8 | SHA absent on non-git runs blocks resubmits | Auto-detect + NA ⇒ skip + warn |
| G9 | A.1 can't catch DGM-internal `set.seed()` | Accepted: A.2 is the complementary heuristic for exactly this |

## Interactive grill (B1–B9): see [GRILL ledger](GRILL-medsim-chunked-run-gates-2026-07-31.md)

Note: B4's "additive dual-column" and B9's shell output-path gate were
superseded in v2 (Architecture decision; review R3) — the ledger records the
reasoning at the time; this spec is authoritative.

## Review Ledger (v2 amendments) — R1–R10

From the 8-angle adversarial review (26 findings; 19 CONFIRMED, 7 empirically
reproduced):

| # | Confirmed finding | Amendment |
|---|---|---|
| R1 | `medsim_check_results()` takes a task LIST, not `medsim_results` — an audit wired there no-ops | Audit lives in combine + new `medsim_audit_results()`; `check_results` untouched |
| R2 | `combined$config$n_replications` is the chunk size — any config-derived nsim false-positives on 100% of correct chunked runs | A.1 is self-validating (contiguity); config never consulted; P2 fixes the stale config itself |
| R3 | B's `[ -s "$chunk_out" ]` bakes the writer's path; the runtime config differs ⇒ exit 1 on success + requeue loop | Path gate **withdrawn**; combiner owns completeness; shell keeps only exit-code discipline |
| R4 | `expected_chunks` stop-default breaks `test-cluster-edge-cases.R:47` + interim looks | Test updated in-PR; interim look = documented `on_violation = "warn"`; data-carrying condition returns partials even on stop |
| R5 | `collapse_col = "estimate"` matches no real column (gate inert); `n_ok = 0` ⇒ `0 > 0` misreports an all-failed cell as collapse | Auto column selection (grill B6); `cell_failed` violation type for `n_ok == 0` |
| R6 | D compares ALL columns — `elapsed`/local ids guarantee false positives | Estimate-columns-only compare; join on `(scenario, replication)` |
| R7 | `inst/hopper-tests/submit_chunk.sh` is a second unhardened template copy | Exemplar + README updated in the same PR |
| R8 | `metadata_cols` name-subtraction is the root cause of the misclassification class | `medsim_meta_cols` provenance attribute stamped by the runner; name list demoted to legacy fallback |
| R9 | Seed-space hash collisions across scenario names are undetectable by A.1 | A.3: recomputed cross-scenario seed check (exact, zero storage) |
| R10 | Ragged-rbind crash + logical-field drop live in the exact lines P1 touches (both empirically reproduced) | P3: runner-owned uniform failure schema + widened type filter |

Review findings **not** folded here (separate work, unblocked by this spec):
mojibake sweep (docs), unseeded pmed-truth MC → closed form, gauge/sobol DGP
dedup, delta-SE / ab-fit shared helpers, MBCO-MI duplicate fits, truth-recompute
race across chunks (`compute_truth` gating), dead `seed_stream` knob,
`.gen_complete_med` rename, interval-kind validation skip, empty interval
coverage table, cache-doc example drift, `config$seed` no-op documentation.

---

## Branch / Worktree Plan (descriptive — not an action item)

Feature-branch work off `dev`. Natural increments: **P1+P2+P3 first, one branch**
(they share `runner.R`/`cluster.R` lines and are the #36/#37-adjacent/#38 fixes)
→ A → B (+ #37 exemplar sweep) → C → D. B and C remain independently shippable.

---

## Implementation log

- **2026-07-31** — `dde3692` fix(chunked runs): Prerequisites P1–P3 landed — global replication ids / results schema v2, combine-step metadata rebuild, runner-owned uniform failure-row schema + logical-field fix, chunk-CSV skip. Fixes the two crashers plus #36 and #38; the breaking `expected_chunks` change shipped with its test update.
- **2026-07-31** — `0fd0510` feat(audit): Gate A landed — combine-step seed-provenance audit (contiguity / collapse / seed-collision checks), `on_violation` control, data-carrying `medsim_combine_violation` condition, new `medsim_audit_results()` export, `.Rbuildignore` additions. Full suite 1038 PASS / 0 FAIL / 1 SKIP.
- **2026-07-31** — `8e272d4` feat(template): Gate B landed — hardened login-shell template (fixes #37), no shell-side output gate (R3), exemplar sweep (R7), `.medsim_chunk_filename()`; + parallel 3-agent doc sync (vignette audit section, _pkgdown.yml index, bidirectional cross-refs). Suite 1056/0/1.
- **2026-07-31** — `0e3b1b8` feat(provenance): Gates C+D landed — chunk provenance header with auto-detected SHA + single-SHA combine assertion (B5/G8), pilot-subset positive control with identity-first assert (B3) and estimate-columns-only tolerance compare (R6/G4). Suite 1077/0/1. **Spec fully implemented.**

---

**Grilled:** independent interactive grill on 2026-07-31 → [GRILL-medsim-chunked-run-gates-2026-07-31.md](GRILL-medsim-chunked-run-gates-2026-07-31.md) (B1–B9 + metadata_cols finding). **Reviewed:** 8-angle adversarial review, 2026-07-31 pm (R1–R10 folded above).
