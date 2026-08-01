# Gate B of SPEC-medsim-chunked-run-gates-2026-07-31 (#34; fixes #37):
# hardened submit-script template. Textual assertions on the emitted script
# plus a stubbed-PATH exit-code harness (a real Hopper run is not performable
# from CI -- inst/hopper-tests/ remains the on-cluster validation path).

.template_cfg <- function(...) {
  medsim_config("cluster", n_chunks = 40L, output_dir = tempfile(), ...)
}

.render_template <- function(cfg = .template_cfg()) {
  out <- tempfile(fileext = ".sh")
  medsim_write_submit_script(cfg, output_file = out)
  readLines(out)
}

# ---- textual acceptance ----------------------------------------------------

test_that("B: template uses a login shell (#37 -- module is login-init-only on Hopper)", {
  lines <- .render_template()
  expect_identical(lines[1L], "#!/bin/bash -l")
})

test_that("B: template hardening -- pipefail, requeue, hard-fail module, Rscript pre-check", {
  lines <- .render_template()
  txt <- paste(lines, collapse = "\n")
  expect_match(txt, "set -eo pipefail", fixed = TRUE)
  expect_match(txt, "#SBATCH --requeue", fixed = TRUE)
  expect_match(txt, "command -v Rscript", fixed = TRUE)
  # module load must hard-fail -- the historical silent-empty-success came
  # from `module load ... || true` swallowing the failure. (The module-INIT
  # fallback source line is best-effort `|| true` by design -- the login
  # shell is the primary mechanism -- so assert on the load line only.)
  load_line <- grep("^module load ", lines, value = TRUE)
  expect_length(load_line, 1L)
  expect_false(grepl("|| true", load_line, fixed = TRUE))
  expect_match(load_line, "\\|\\| \\{.*exit 1")
})

test_that("B: no shell-side output-path gate (withdrawn per review R3)", {
  # The writer's config and the run script's runtime config are independent;
  # a baked [ -s <path> ] gate exits 1 on every successful task whenever they
  # differ (and --requeue then loops). Completeness belongs to the combiner.
  lines <- .render_template()
  expect_false(any(grepl("[ -s", lines, fixed = TRUE)))
})

test_that("B: array throttle emitted when configured, absent otherwise", {
  cfg <- .template_cfg()
  cfg$array_throttle <- 16L
  lines <- .render_template(cfg)
  expect_true(any(grepl("--array=1-40%16", lines, fixed = TRUE)))

  lines_plain <- .render_template()
  expect_true(any(grepl("--array=1-40$", lines_plain)))
})

# ---- behavioral: stubbed exit-code harness --------------------------------

.run_with_stubs <- function(module_exit = 0L, rscript_exit = 0L) {
  skip_on_os("windows")
  stubs <- tempfile("stubs"); dir.create(stubs)
  marker <- file.path(stubs, "rscript_ran")

  writeLines(c("#!/bin/bash", sprintf("exit %d", module_exit)),
             file.path(stubs, "module"))
  writeLines(c("#!/bin/bash", sprintf("touch %s", shQuote(marker)),
               sprintf("exit %d", rscript_exit)),
             file.path(stubs, "Rscript"))
  Sys.chmod(file.path(stubs, c("module", "Rscript")), "0755")

  script <- tempfile(fileext = ".sh")
  medsim_write_submit_script(.template_cfg(), output_file = script)

  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  Sys.setenv(PATH = paste(stubs, old_path, sep = .Platform$path.sep))

  status <- system2("bash", script, stdout = FALSE, stderr = FALSE)
  list(status = status, rscript_ran = file.exists(marker))
}

test_that("B: clean run exits 0 and reaches Rscript (negative control)", {
  r <- .run_with_stubs(module_exit = 0L, rscript_exit = 0L)
  expect_identical(r$status, 0L)
  expect_true(r$rscript_ran)
})

test_that("B: module-load failure exits nonzero BEFORE Rscript (planted defect)", {
  # Pre-fix, this was the Hopper silent-empty-success: module load failed,
  # || true swallowed it, a trailing command reset $?, task reported 0:0.
  r <- .run_with_stubs(module_exit = 1L)
  expect_true(r$status != 0L)
  expect_false(r$rscript_ran)
})

test_that("B: Rscript failure propagates its exit code (planted defect)", {
  r <- .run_with_stubs(rscript_exit = 3L)
  expect_identical(r$status, 3L)
  expect_true(r$rscript_ran)
})

# ---- shared filename convention -------------------------------------------

test_that("B: .medsim_chunk_filename is the single naming authority", {
  expect_identical(.medsim_chunk_filename(7L), "chunk_0007.rds")
  # The writer must use it: run a chunk and find the file under that name.
  sc <- medsim_scenario(
    name = "fname_conv",
    data_generator = function(n = 20) data.frame(x = stats::rnorm(n)),
    params = list()
  )
  out <- withr::local_tempdir()
  cfg <- medsim_config("test", chunk_id = 7L, n_chunks = 8L,
                       n_replications = 16L, n_cores = 1L, output_dir = out)
  medsim_run_chunk(list(sc), function(data, params) list(m = mean(data$x)),
                   cfg, verbose = FALSE)
  expect_true(file.exists(file.path(out, .medsim_chunk_filename(7L))))
})
