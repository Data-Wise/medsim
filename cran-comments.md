## Submission

This is the first CRAN submission of medsim (0.1.0), a simulation
infrastructure package for mediation analysis.

## Test environments

* Local: macOS Sequoia 15.x, R 4.6.0
* GitHub Actions:
  * macOS-latest, R release
  * windows-latest, R release
  * ubuntu-latest, R release
  * ubuntu-latest, R oldrel-1
  * ubuntu-latest, R-devel (weekly scheduled)
* R-hub (manual dispatch) — Linux, Windows, macOS R-devel

## R CMD check results

0 errors, 0 warnings, 0 notes (anticipated; verified locally with
--as-cran).

## Reverse dependencies

This is a first submission. No CRAN reverse dependencies.

`mediationverse` (umbrella package, GitHub-only) imports medsim;
verified to load cleanly with this version.

## Notes for CRAN reviewers

* The `Suggests:` field lists `medfit`, `medrobust`, and `probmed` —
  these are part of the same mediationverse ecosystem (Data-Wise org)
  but are not currently on CRAN. They are only loaded by examples and
  tests behind `requireNamespace()` checks, and the package functions
  correctly without them. The `Remotes:` field in DESCRIPTION points
  to their GitHub source for development installation only; CRAN
  builds do not use it.
* The vignette is built with Quarto (`VignetteBuilder: quarto`). The
  `quarto` package is declared in Suggests as required.
* `RMediation` (already on CRAN) is suggested but not required.

## Maintainer

Davood Tofighi <dtofighi@gmail.com>
ORCID: 0000-0001-8523-7776
