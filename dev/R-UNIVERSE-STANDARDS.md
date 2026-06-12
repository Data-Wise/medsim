# R CMD check Philosophy & r-universe Standards

**Scope:** What “passing checks” means for `medsim` on **r-universe**,
how that relates to `R CMD check` (“ccheck”) and CRAN, and a concrete
readiness checklist.

**Audience:** Maintainers of `medsim` and the wider mediationverse
(`medfit`, `probmed`, `RMediation`, `medrobust`, `missingmed`).

**Last updated:** 2026-06-11

> **Live status (2026-06-11):** The universe is already registered and
> active at **<https://data-wise.r-universe.dev>** with all 7 packages.
> `medsim` **v0.2.0** (released 2026-06-11; r-universe rebuilds to it on
> its ~hourly cron) builds **green on every platform** (linux, macOS,
> Windows, wasm); `_status: success`, zero failures. Install with the
> snippet in §5. This doc is now a *maintenance* reference, not a setup
> guide.

------------------------------------------------------------------------

## 1. The Three Layers of “Check”

There is one engine and three places it runs. Understanding the
distinction prevents wasted effort chasing the wrong gate.

| Layer | Where it runs | What it is | Blocking? |
|----|----|----|----|
| **`R CMD check`** (“ccheck”) | Your machine / CI | The canonical correctness engine. `rcmdcheck`, `devtools::check()`, GitHub Actions `R-CMD-check.yaml` all wrap it. | Yes, for *you* |
| **r-universe** | rOpenSci build farm | The *same* `R CMD check` run continuously on every git push, on Linux/macOS/Windows, plus binary + docs builds. | **No** — publishes anyway, reports status |
| **CRAN** | CRAN incoming + daily farm | The same engine, plus extra policy checks (`--as-cran`), human review, and reverse-dependency checks. | Yes — human gatekeeper |

**Key insight:** the *check itself* is identical machinery. The
difference is the **governance model** around it. Get `R CMD check`
clean locally and you are simultaneously satisfying all three layers’
core requirement.

------------------------------------------------------------------------

## 2. r-universe Philosophy

r-universe is rOpenSci’s **personal CRAN-like repository** system. Its
design philosophy is the inverse of CRAN’s gatekeeping model:

1.  **Permissionless publishing.** Anyone can publish; you do not wait
    in a review queue. Packages need not even be on GitHub — any public
    git server (GitLab, university host) works.

2.  **Continuous, not episodic.** Every push to a registered repo
    triggers a rebuild. Source tarball, Windows + macOS binaries, and
    documentation are all built and **deployed simultaneously** — so
    binaries are never out of sync with source (unlike CRAN, where
    binaries lag by days).

3.  **Transparency over gatekeeping.** Check results are surfaced as a
    **dashboard + badge + API**, not a pass/fail gate. A failing check
    does not un-publish you; it shows red on your universe page. The
    social pressure is visibility, not rejection.

4.  **CRAN-compatible by construction.** Because it runs real
    `R CMD check` and produces a real CRAN-like repo, a package that is
    green on r-universe is *almost* CRAN-submittable. r-universe is the
    natural staging ground: **make it green here first, then submit to
    CRAN.**

5.  **Dependency resolution from your metadata.** r-universe resolves
    dependencies from CRAN, Bioconductor, *other universes*, and your
    `Remotes:` field — which is why non-CRAN siblings resolve without
    manual intervention (see §4).

> Practical consequence for mediationverse: r-universe lets the whole
> ecosystem (`medfit`, `probmed`, etc., none on CRAN) install cleanly
> via
> [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
> from a single `data-wise.r-universe.dev` repo, with cross-package
> `Remotes` resolved automatically. This is the distribution story
> *until* CRAN submission.

------------------------------------------------------------------------

## 3. What r-universe Builds (per push)

| Build step | Produces | Fails when |
|----|----|----|
| `src` | Source tarball (`R CMD build`) | Vignettes/docs error, missing files |
| `R CMD check` | Check log (NOTE/WARNING/ERROR) | Any ERROR; WARNINGs surfaced |
| `WinBinary` | Windows binary | Compiled code or system deps fail on Windows |
| `MacBinary` | macOS binary (arm64 + x86_64) | Same, on macOS |
| `docs` | pkgdown/Quarto site, vignettes, reference | `Config/Needs/website` deps missing, vignette build error |
| metadata | System-dependency detection, logo, badges | — (best-effort) |

`medsim` exercises **all** of these: it has compiled-free R (good — no
Win/Mac compile risk), Quarto vignettes (`VignetteBuilder: quarto`), and
a pkgdown site. The docs build is the most likely place to go yellow.

------------------------------------------------------------------------

## 4. DESCRIPTION Fields That Matter for r-universe

These are the fields r-universe *reads* to drive the build. `medsim`’s
current state is annotated.

| Field | Why r-universe cares | `medsim` status |
|----|----|----|
| `Package`, `Version`, `Title`, `Description`, `License` | Registry identity + repo metadata | ✅ Present (`v0.2.0`, GPL ≥ 3) |
| `Authors@R` with ORCID | Author pages, contributor feeds | ✅ ORCID present |
| `URL` | Links dashboard → source + pkgdown site | ✅ GitHub + pages URL |
| `BugReports` | “Report a bug” link on universe page | ✅ Issues URL |
| **`Remotes:`** | **Resolves non-CRAN deps** (the critical one) | ✅ `Data-Wise/medfit` (only GitHub-only dep as of v0.2.0; PR \#18 dropped missingmed/rmediation) |
| `Depends` / `Imports` / `Suggests` | Dependency graph; Suggests built if resolvable | ✅ heavy deps in Suggests |
| `Config/Needs/website` | Extra pkgs installed *only* for the docs build | ✅ `pkgdown, quarto` |
| `VignetteBuilder` | Tells build farm how to render vignettes | ✅ `quarto` |
| `Config/testthat/edition` | testthat 3e behavior | ✅ `3` |

> **Remotes is the linchpin.** Same lesson as your CI fix (CLAUDE.md, PR
> \#1): without `Remotes:`, r-universe — like `pak` in R-CMD-check —
> treats `medfit` (the sole GitHub-only dep) as missing and the build
> fails. CRAN, by contrast, **forbids** `Remotes` and requires every
> hard dependency to be on CRAN/Bioconductor. This is a real fork in the
> road (see §6).

------------------------------------------------------------------------

## 5. Registering medsim on r-universe

r-universe builds from a **registry**: a `packages.json` in a git repo
named `<org>.r-universe.dev`
(e.g. `Data-Wise/data-wise.r-universe.dev`).

> **Already done.** `Data-Wise/data-wise.r-universe.dev` exists and its
> `packages.json` registers all 7 mediationverse packages (current
> contents below). To add a future package, append its entry and push —
> r-universe picks it up automatically.

Current registry entry:

``` json
[
  { "package": "mediationverse", "url": "https://github.com/Data-Wise/mediationverse" },
  { "package": "medfit",     "url": "https://github.com/Data-Wise/medfit" },
  { "package": "medsim",     "url": "https://github.com/Data-Wise/medsim" },
  { "package": "medrobust",  "url": "https://github.com/Data-Wise/medrobust" },
  { "package": "probmed",    "url": "https://github.com/Data-Wise/probmed" },
  { "package": "missingmed", "url": "https://github.com/Data-Wise/missingmed" },
  { "package": "rmediation", "url": "https://github.com/Data-Wise/rmediation" }
]
```

Once registered, the universe is live at
`https://data-wise.r-universe.dev` and installable with:

``` r

install.packages(
  "medsim",
  repos = c("https://data-wise.r-universe.dev", "https://cloud.r-project.org")
)
```

Optional per-package controls (subdir, branch, pkgdown) go in the
registry entry, e.g. `"branch": "main"` or `"subdir": "pkg"`. `medsim`
lives at repo root on `main`, so defaults suffice.

------------------------------------------------------------------------

## 6. The `Remotes` Fork: r-universe vs CRAN

This is the single most important standards decision for mediationverse.

|  | r-universe | CRAN |
|----|----|----|
| Non-CRAN deps via `Remotes` | ✅ Resolved | ❌ Rejected |
| All hard deps must be on CRAN | No | **Yes** |
| Verdict for `medsim` today | **Publishable now** | **Blocked** until siblings are on CRAN |

Because `medfit`, `missingmed`, and `rmediation` are GitHub-only,
**`medsim` cannot go to CRAN until they do** (or until those deps move
to `Suggests` with graceful `requireNamespace` degradation — which
`medsim` already does for the heavy ones). r-universe is the correct
distribution channel in the meantime, and your existing `Remotes:` field
makes it work today.

> Aligns with the existing memory: CRAN not currently pursued;
> GitHub-only, latest tag `v0.2.0`. r-universe distributes it on its
> next rebuild.

------------------------------------------------------------------------

## 7. Readiness Checklist

Run before relying on the r-universe build. Each maps to a check layer.

``` r

# 1. The canonical check — must be clean (0 errors; 0 warnings ideally)
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "warning")

# 2. CRAN-policy superset (catches what r-universe tolerates but CRAN won't)
rcmdcheck::rcmdcheck(args = c("--as-cran"), error_on = "warning")

# 3. Docs build (the field r-universe builds from Config/Needs/website)
pkgdown::check_pkgdown()      # navbar/articles index integrity
pkgdown::build_site(preview = FALSE)

# 4. Vignettes render under the declared builder
tools::buildVignettes(dir = ".")   # exercises VignetteBuilder: quarto

# 5. Dependency hygiene — does anything need a Remotes hint?
#    (attachment::att_amend_desc() can auto-detect + update DESCRIPTION)
```

Manual checklist:

`R CMD check` clean on Linux/macOS/Windows (CI matrix already covers
this)

`Remotes:` lists **every** non-CRAN dependency, owner/repo form

`URL` + `BugReports` present and correct

`Config/Needs/website` lists everything the pkgdown/Quarto build imports

Heavy/optional deps in `Suggests`, guarded by
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)

No `\dontrun{}` hiding broken examples; prefer `\donttest{}` or real
runs

Vignettes build offline (no network calls at build time)

Registry entry added to
`Data-Wise/data-wise.r-universe.dev/packages.json`

Build badge added to `README.md`:
`[![r-universe](https://data-wise.r-universe.dev/badges/medsim)](https://data-wise.r-universe.dev/medsim)`

------------------------------------------------------------------------

## 8. medsim-Specific Notes

- **No compiled code** → Windows/macOS binary builds are low-risk. The
  failure surface is concentrated in **vignettes + pkgdown**, not
  compilation.
- **Quarto vignettes** require `quarto` on the build farm; it is
  declared in both `Suggests` and `Config/Needs/website`, so r-universe
  will install it. Confirm the Quarto CLI version assumption holds
  (r-universe pins its own).
- **Missing-data DGM (shipped in v0.2.0)** added `mice`/`mitml` to
  `Suggests` (`RMediation` was already present). The validated D4-MBCO
  estimator uses `mice` + base R, so `missingmed`/`rmediation` were
  **dropped** from both `Suggests` and `Remotes` — leaving
  `Remotes: Data-Wise/medfit` only. Keeping these in `Suggests` (not
  `Imports`) preserves the estimator-agnostic boundary.
- **CI ≈ r-universe.** Your `R-CMD-check.yaml` already mirrors the
  r-universe matrix (macOS/Windows/Ubuntu release + oldrel). Green CI is
  a strong predictor of green r-universe.

------------------------------------------------------------------------

## 9. References

- [r-universe documentation](https://docs.r-universe.dev/)
- [rOpenSci — A first look at the R-universe build
  infrastructure](https://ropensci.org/blog/2021/03/04/r-universe-buildsystem/)
- [rOpenSci — Setting up your own
  R-universe](https://ropensci.org/blog/2021/06/22/setup-runiverse/)
- [pkgdown reference
  manual](https://r-lib.r-universe.dev/pkgdown/doc/manual.html)
- [attachment — manage DESCRIPTION
  dependencies](https://thinkr-open.github.io/attachment/)
- [Writing R Extensions — the `R CMD check` source of
  truth](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
