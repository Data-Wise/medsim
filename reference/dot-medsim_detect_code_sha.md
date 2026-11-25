# Auto-detect a code-state identifier for chunk provenance (Gate C)

`git rev-parse HEAD` in the running script's directory (from
`commandArgs(--file=)`, falling back to
[`getwd()`](https://rdrr.io/r/base/getwd.html) for interactive use – the
RUN SCRIPT's tree, not wherever R happened to start). Outside a git
tree, degrades to a `pkg:medsim-<version>` tag: stable across chunks of
one submission (so the combine assertion stays quiet) but unable to
distinguish code states – true mid-run-edit detection needs git.

## Usage

``` r
.medsim_detect_code_sha()
```

## Value

Character scalar: a 40-hex SHA or a `pkg:medsim-<version>` tag.
