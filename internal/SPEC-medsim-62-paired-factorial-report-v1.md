# Spec delta for the #62 paired factorial report, V1

Status: approved and implemented

This extends the reporting in the [#62 calibration harness spec](SPEC-medsim-62-mbco-mi-calibration-2026-11-23.md).
V1 covers the first priority in the production review. Its input is an
already finalized run, including the 72-cell Hopper run.

## Adversarial review

1. **The decision receipt cannot identify the four paired contrasts.**
   It records arm totals and the candidate-versus-legacy discordances.
   Different joint outcomes can give those same totals and different
   factorial MCSEs. Use the saved dataset-level arm decisions and their keys.
2. **Different denominators can create an apparent interaction.** A
   difference between contrasts calculated from different usable subsets
   can reflect sample composition. All five estimates use the same
   four-arm usable subset. Report the eligible count for each comparison
   so gaps between that subset and the available pairs stay visible.
3. **Receipt paths are host-specific.** Hopper records absolute selected
   chunk paths. A relocated copy can contain other attempts or stale
   payloads. Use exact manifest keys and the selected chunk hashes before
   admitting rows.
4. **Nominal power and calibrated power answer different questions.** The
   18.26-point nominal power gap came from the Gaussian alternative at
   `n = 100` and 50% MAR. Size inflation elsewhere warrants a prospective
   size-matched comparison. Branch mixing and factorial contrasts remain
   descriptive.

The saved decision receipt lists 1,320 selected payloads across six groups
and zero unusable arm outputs. The denominator policy still covers future
finalized runs with unavailable arms.

## Objective and contract

Add a read-only reporting command under `inst/mbco-calibration/`. It reads
`manifest.rds`, `decision.rds`, and the receipt-selected chunk payloads
from a run directory. Require the caller's expected SHA-256 of
`decision.rds` and verify the manifest hash recorded there. The original
Hopper decision SHA-256 is
`b4857102487381b2ca9a5f70a23fa18776bc4393f952b76b42283b6257174f3b`.

Resolve chunk paths relative to the supplied run directory using the
expected `attempts/<group>/<chunk>/<attempt>/chunk_<id>.rds` layout.
Check each selected path, hash, producer SHA, sealed payload provenance,
and exact assigned scenario and replicate keys against the manifest.
Use the package's chunk index calculation. Require one selected payload
per assigned chunk. Fail with the affected group and chunk when evidence
is missing or inconsistent. Process one chunk at a time.

Use the harness validity rules for complete data and imputations. Code
rejection as `1L` for a usable p-value below `0.05` and `0L` otherwise
within the four-arm usable subset. For each row in that subset compute
the four simple differences and one interaction.

| Contrast | Per-draw difference |
|---|---|
| ARIV at Li | Fixed/Li minus own/Li |
| ARIV at Chan–Meng | Fixed/Chan–Meng minus own/Chan–Meng |
| df at own ARIV | Own/Chan–Meng minus own/Li |
| df at fixed ARIV | Fixed/Chan–Meng minus fixed/Li |
| Interaction | ARIV difference at Chan–Meng minus ARIV difference at Li |

For example, the interaction calculation on integer rejection indicators is
`(fixed_chan_meng - own_chan_meng) - (fixed_li - own_li)`.
Report the mean of the per-draw differences and their sample SD divided by
the square root of the number of usable draws. Count differences at each
integer from minus two through two. Report missing estimates when zero draws
are usable, and missing MCSE when fewer than two are usable. Use probability
units in the CSV.

Write one CSV with five rows per cell. Each row identifies the scenario
and contrast and includes attempts, the comparison's usable count, the
four-arm usable count, signed counts, estimate, and MCSE. Include the
manifest and decision hashes, scenario-level branch-mix counts and
denominator from the official receipt, the nominal complete-pair
candidate-versus-legacy difference and MCSE, and the official bounds
for null cells. Power rows have missing gate bounds. Publish the requested
output only after validating the full input and refuse an existing output.

## Commands and project structure

The report uses base R and the existing medsim dependency.
`report.R` owns the command and accumulation. Reuse the harness's
arm names and usability logic. Put deterministic report fixtures under
`tests/testthat/` and document the command in the harness README.

```sh
Rscript --vanilla inst/mbco-calibration/report.R "$RUN_DIR" "$REPORT_CSV" "$DECISION_SHA256"
Rscript --vanilla -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-calibration-factorial-report.R", stop_on_failure = TRUE)'
git diff --check
```

## Acceptance and boundaries

- Always check paired counts and MCSE against direct calculation on hand-built
  rows. Include missing arms, zero and one usable row, p equal to 0.05,
  and complete data. Reject altered bytes, bad keys, and partial reports.
- Validate the command on all 1,320 saved Hopper chunks. It must emit 360
  rows covering all 72 cells and account for 5,580,000 attempted datasets
  across unique cells. Check that the null bounds agree with the verified
  receipt. Confirm the interaction agrees with both differences of simple
  contrasts on the common subset.
- Keep the production gate, saved decisions, and attempt directories
  read-only. Use a separate output destination. Production simulation,
  changes to acceptance thresholds, and release actions are outside V1.

## Deferred

Size-matched power needs a separate protocol for composite-null calibration
and independent evaluation. General finalization recovery, changes to
imputation telemetry, and the 25% and 40% MAR bridge each have separate
acceptance criteria and can follow this report.
