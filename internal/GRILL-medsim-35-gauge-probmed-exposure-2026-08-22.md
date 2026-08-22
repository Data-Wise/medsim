# GRILL — medsim #35: gauge estimand runs against a buggy probmed (rescoped from a doc note)

**Date**: 2026-08-22 · **Target**: issue #35 · **Base**: dev @ `1150e45`
**Status**: PROPOSAL — rescoped at grill GD7. Was "add a roxygen note"; is now
a correctness exposure with a shippable guard.
**Location**: canonical design doc for #35.

**Method**: interactive `/craft:grill` branch GD7, grounded read-only against
`Data-Wise/probmed` via `gh` (releases, branch tips, PR base) and medsim's own
`DESCRIPTION` / gauge sources.

---

## 0. Why this issue changed shape

#35 was scoped as documentation: state `se_method=` as the canonical
interface, and note that a BCa arm was "planned pending upstream
(probmed#32)". Grounding that framing on 2026-08-22 found the upstream
situation had moved, and found something the issue never contemplated.

### F6 — medsim's gauge workflow silently runs against a known upstream bug

| Fact | Evidence |
|---|---|
| probmed PR #34 fixed a per-row inverse-probability weight bug in `.corner_fit()` (present since `10b41aa`): `ifelse(z == 1, p1, 1 - p1)` with scalar `z` returned `p1[1]` — the first fold row's propensity — for **every** row | probmed PR #34 |
| That PR merged into **`dev`**, not `main` | `gh pr view 34 -R Data-Wise/probmed` → `base: dev`, sha `a73efc8`, merged 2026-08-22T21:57Z |
| probmed `main` does **not** carry the fix | `gh api .../branches/main` → tip `0486b01`, unchanged |
| The only probmed **release** is v0.3.0 (2026-06-22) — predates the fix | `gh release list -R Data-Wise/probmed` |
| medsim's `Remotes: Data-Wise/probmed` resolves to the **default branch (`main`)** | `DESCRIPTION:51` |
| medsim's `Suggests: probmed` carries **no version floor** | `DESCRIPTION:29` |

Measured effect upstream: with the weights corrected, the default analytic
Wald interval covers **0.936** (continuous `Y`) and **0.972** (binary `Y`) at
n = 800 under strong identification. The pre-fix grid that motivated the whole
BCa discussion read **~0.85–0.90**. That is a decision-changing difference.

**Consequence for medsim:** `medsim_method_gauge()` wraps
`probmed::ward_residual()`. Every current install route — release, r-universe,
`Remotes` — yields a probmed **without** the fix. A gauge coverage grid run
today produces materially wrong coverage numbers, with no warning.

Note what is *not* wrong: medsim's own documentation is descriptive, not
evaluative — `R/methods_gauge.R:38` says `"analytic"` (default) or
`"bootstrap"` (percentile), and `vignettes/estimand-kinds.qmd:99,120` describe
which arms exist. medsim never claimed coverage properties, so no medsim
sentence needs retracting. The exposure is in the *results*, not the prose.

### The BCa framing is also stale — in the opposite direction from the issue

probmed#32 is **still OPEN**, but its latest comment (2026-08-22) retracts the
prior two and recommends **leaving BCa out**: with weights corrected there is
no under-coverage for a tail-aware interval to fix, and near the null the
ratio is non-regular for every construction, so BCa would not help in the one
regime where the Wald interval still fails. Do **not** write "BCa planned
pending upstream" — upstream is trending won't-fix. Do not write "upstream
resolved" either; the issue is open with a recommendation, not a decision.

---

## 1. Design

### D1 — runtime guard in `medsim_method_gauge()` (the shippable protection)

Documentation does not stop a silently-wrong overnight simulation; this
package's whole gate architecture (#34/#43 lineage) exists on that premise.
Add a version/behavior check that fires when the installed probmed predates
the weight fix.

**Detection is the hard part** and needs a decision at implementation time,
because there is no released version to compare against yet:

- **(i) Version string** — cleanest once probmed cuts a release carrying the
  fix: `packageVersion("probmed") >= "<that version>"`. Unavailable today.
- **(ii) Dev-version string** — `>= "0.3.0.9000"` catches a dev install but
  is a lie in both directions: `0.3.0.9000` existed *before* PR #34 too.
- **(iii) Behavioral probe** — call `.corner_fit()`-adjacent machinery on a
  tiny fixture where the buggy and fixed weights differ observably. Robust
  and version-independent, but reaches into probmed internals, which is
  fragile across upstream refactors.

**Recommended:** ship (i) as a placeholder that is *inert until the version
exists*, plus a loud `@section Warning` in the roxygen naming the exposure
explicitly, and open the version floor as the follow-up (D3). Prefer a
`warning()` over a `stop()` here — unlike #41's GD5, medsim cannot prove the
user's probmed is buggy without either a released version to compare or an
internals probe, and stopping on a guess would break legitimate dev installs
that already carry the fix.

### D2 — the original doc item, still worth doing while in the file

Add a one-line roxygen note to `medsim_method_gauge()` recording that
`ci = "percentile"` from the #24 spec maps to `se_method = "bootstrap"`,
closing the naming divergence without introducing an alias. `se_method=` is
already load-bearing everywhere (`tests/testthat/test-gauge.R:57-58`,
`vignettes/estimand-kinds.qmd:100,106`, `R/methods_gauge.R:38-39`, and
downstream `pmed-modern/run_gauge_boot_grid.R`), so this is documentation
catching up to reality, not an interface change.

Rewrite the BCa contract note to: the gauge estimand's CI arms are
`analytic` and `bootstrap` (percentile), **full stop** — citing probmed PR
#34's weight fix as why a BCa arm is not planned. Delete "planned pending
upstream".

### D3 — version floor (follow-up, blocked on upstream)

Once probmed releases a version containing PR #34, add
`probmed (>= <version>)` to `Suggests:`. Until then there is nothing to pin
to. **Cross-repo note:** cutting that probmed release is work in another
repo and requires explicit authorization — it is not part of this issue.

---

## 2. Non-Goals

- No BCa arm (upstream recommends against it; nothing to implement).
- No change to `se_method`'s interface — documentation only (D2).
- No probmed changes. Read-only inspection only; a probmed `dev → main`
  release is the user's call, in the user's repo.
- Not re-measuring the percentile-bootstrap arm's calibration post-fix.
  probmed#32 notes it is now **unknown** ("the percentile arm has not been
  re-measured"), but that measurement belongs upstream, and medsim makes no
  calibration claim to correct.

---

## 3. Verification

| # | Case | Check |
|---|---|---|
| 1 | Guard fires on an old probmed | with a probmed lacking the fix installed, `medsim_method_gauge()` emits the exposure warning once |
| 2 | Guard silent on a fixed probmed | with a fixed probmed, no warning (negative control — a guard that always fires is noise, and users will suppress it) |
| 3 | Guard never errors | the check itself must degrade to silence if `probmed` is absent or `packageVersion()` throws — a diagnostic must not become a new failure mode |
| 4 | Doc note rendered | `?medsim_method_gauge` shows the `ci = "percentile"` ↔ `se_method = "bootstrap"` mapping and the two-arm contract with no "BCa planned" text (grep-based drift guard) |

Docs + one guarded warning ⇒ `devtools::test()` plus `R CMD check`;
`pre-pr-testing.md`'s tier table puts a behavior change (the guard) above the
docs-only lint tier.

---

## 4. Branch / Worktree Plan (descriptive — not an action item)

Small; `R/methods_gauge.R` only. Independent of #41 and #28 — no shared
files. Could ride #41's branch if convenient, but the change is unrelated and
a separate branch keeps the breaking-change story in #41 clean.

---

## 5. Grill ledger (interactive, 2026-08-22)

| # | Question | Resolution |
|---|---|---|
| GD7 | How should #35 be rescoped, given medsim's gauge estimand silently runs against a buggy probmed in every current install route? | **Rescope around the F6 weight-bug exposure.** Runtime guard + roxygen note now (D1/D2); version floor tracked as a follow-up once upstream releases (D3). Rejected: guard-now-and-split-the-pin-into-a-new-issue (same work, one more open issue in a backlog being shrunk), document-only (docs do not stop a silently-wrong overnight run), upstream-first (correct at the source but cross-repo, needs authorization, and leaves medsim exposed meanwhile). |

### Corrections to the issue body to apply when retitling

1. "BCa arm blocked upstream" ⇒ **not planned**; probmed#32 open with a
   won't-fix recommendation, grounded in PR #34's weight fix.
2. Add F6 as the issue's headline — it is the reason the issue matters now.
3. Keep the existing correction already in the body (the `se_method`
   `match.arg` finding and the corrected line cites `R/methods_gauge.R:51`,
   `:61-63`) — re-verified accurate this session.
