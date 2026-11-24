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
Finalization compares overlapping results, runs stock combine per group, and
writes `decision.rds` with selected chunk hashes and per-cell results. A
provenance or integrity error leaves the decision unwritten. A valid result
that misses a size bound records `HOLD`; its fixed budget has ended.

```sh
Rscript --vanilla "$HARNESS/run.R" finalize "$RUN_DIR" "$PARITY_RECEIPT"
```
