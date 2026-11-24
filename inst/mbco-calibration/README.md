# medsim #62 calibration harness

The [harness spec](../../internal/SPEC-medsim-62-mbco-mi-calibration-2026-11-23.md)
defines the 72 cells, the paired release rule, the missingmed crosswalk, and
the fixed 5,580,000-dataset budget. The four arms share each data draw,
imputation list, and set of likelihood fits. The scripts use the stock medsim
chunk runner and combine audit.

## Local checks

Install the medsim candidate and `mice`. Install missingmed at
`4272b367b483018fccb1a0aadc7f618e94ee6901` with its `RemoteSha` recorded. Set
`FIXTURE` to the absolute path to `tests/testthat/fixtures/mbco-d4-medsim-0.5.1.rds`
from that revision. The adapter and parity checks verify its Git blob ID. The
smoke and adapter checks require `testthat`.

Run from the repository root.

```sh
Rscript --vanilla inst/mbco-calibration/check.R smoke
Rscript --vanilla inst/mbco-calibration/check.R adapter "$FIXTURE"
Rscript --vanilla inst/mbco-calibration/check.R parity "$FIXTURE"
```

The smoke runs two skewed MAR replicates in one cell. It also checks the full
seed map and exercises finalization on synthetic files. The adapter and parity
checks reuse fixed inputs. Local check results are outside the release budget.

## Confirmatory workflow after production approval

Use the pilot to choose both chunk counts before freezing the manifest. Install
the committed medsim candidate with its Git `RemoteSha` recorded. Run all
commands against the same installed build and dependency environment. Set
`RUN_DIR` to an absolute path for a new directory on the shared filesystem.
Use the same installed harness for each command, under
`system.file("mbco-calibration", package = "medsim")`.

The plan command freezes the exact scenario and replicate ranges, six
sample-size-by-budget groups, and package and runtime identities in
`manifest.rds` after checking every deterministic seed.

```sh
Rscript --vanilla "$HARNESS/run.R" plan "$RUN_DIR" "$MEDSIM_SHA" "$NULL_CHUNKS" "$POWER_CHUNKS"
Rscript --vanilla "$HARNESS/check.R" parity "$FIXTURE" "$PARITY_RECEIPT"
```

Run one chunk command per planned group and chunk ID in the approved
allocation. `GROUP` is one of `100-null`, `100-power`, `200-null`,
`200-power`, `500-null`, or `500-power`. Use `ATTEMPT=1` on the first run,
and a fresh positive attempt number when the scheduler interrupts a chunk.
Keep the original attempt directory. The chunk ID and manifest determine the
replicate IDs and seeds. A hard interruption can leave only `started.rds`
or an unsealed payload. Finalization checks any readable overlapping results
and requires a sealed, complete attempt for every planned chunk.

```sh
Rscript --vanilla "$HARNESS/run.R" chunk "$RUN_DIR" "$GROUP" "$CHUNK" "$ATTEMPT" "$CORES"
```

After all scheduler attempts and retries have ended, finalize once. Preserve
the manifest, parity receipt, and attempt directories with the decision.
The finalizer requires the signed-off fixture identity, matching R, RNG and
dependency versions, and recorded values for all six case-by-ARIV
comparisons. It checks the numeric and discrete parity criteria before
reconciling the attempts.
Finalization compares overlapping results, runs stock combine per group, and
writes `decision.rds` with selected chunk hashes and per-cell results. The
collapse check uses 12 significant digits for the four arm p-values and
the two p-value aliases. It uses decimal places for other audited fields. A
provenance or integrity error leaves the decision unwritten. A valid result
that misses a size bound records `HOLD`; its fixed budget has ended.

```sh
Rscript --vanilla "$HARNESS/run.R" finalize "$RUN_DIR" "$PARITY_RECEIPT"
```

## Paired factorial report from saved chunks

Use the report command after finalization. A relocated archive is supported
when it preserves `manifest.rds`, `decision.rds`, and the attempt directory
layout with the selected payloads and their receipts. Obtain the expected
decision SHA-256 from the trusted run record. Use a new CSV path outside
the attempts directory.

```sh
Rscript --vanilla "$HARNESS/report.R" "$RUN_DIR" "$REPORT_CSV" "$DECISION_SHA256"
```

The CSV reports four simple contrasts and the ARIV-by-df interaction for
each cell. Estimates and paired Monte Carlo standard errors use the same
four-arm usable draws and are in probability units. Each row includes the
number of draws usable for its particular comparison, the signed difference
counts on the common subset, and diagnostics from the official decision.
The gate bounds are blank for power cells. The command validates the
receipt-selected chunks, exact replicate keys, and decision diagnostics
before writing the report. It hashes and reads the same private snapshot of
each RDS input. The command needs `sha256sum` or `shasum` and an output
filesystem that supports hard links. It only reads saved simulation data.
