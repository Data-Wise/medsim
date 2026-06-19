# Tests for cache functions

test_that("medsim_cache_save and load work", {
  # Create temp cache file
  cache_file <- tempfile(fileext = ".rds")

  # Save object
  test_object <- list(value = 42, name = "test")
  result <- medsim_cache_save(test_object, cache_file)

  expect_true(file.exists(cache_file))
  expect_equal(result, cache_file)

  # Load object
  loaded <- medsim_cache_load(cache_file, verbose = FALSE)

  expect_equal(loaded$value, test_object$value)
  expect_equal(loaded$name, test_object$name)

  # Cleanup
  unlink(cache_file)
})

test_that("medsim_cache_load returns NULL for missing file", {
  non_existent <- tempfile(fileext = ".rds")

  result <- medsim_cache_load(non_existent, verbose = FALSE)

  expect_null(result)
})

test_that("medsim_cache_save creates directory if needed", {
  # Create nested temp path
  temp_dir <- tempfile()
  cache_file <- file.path(temp_dir, "subdir", "cache.rds")

  test_object <- list(x = 1:10)
  medsim_cache_save(test_object, cache_file)

  expect_true(file.exists(cache_file))
  expect_true(dir.exists(dirname(cache_file)))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("medsim_cache_exists works", {
  cache_file <- tempfile(fileext = ".rds")

  expect_false(medsim_cache_exists(cache_file))

  # Create file
  test_object <- list(x = 1)
  medsim_cache_save(test_object, cache_file)

  expect_true(medsim_cache_exists(cache_file))

  # Cleanup
  unlink(cache_file)
})

test_that("medsim_cache_info returns correct information", {
  cache_file <- tempfile(fileext = ".rds")
  test_object <- list(value = 42)

  medsim_cache_save(test_object, cache_file)

  info <- medsim_cache_info(cache_file)

  expect_type(info, "list")
  expect_true(info$exists)
  expect_true(info$size_mb > 0)
  expect_true(info$age_days >= 0)
  expect_s3_class(info$modified, "POSIXct")

  # Cleanup
  unlink(cache_file)
})

test_that("medsim_cache_info works for non-existent file", {
  non_existent <- tempfile(fileext = ".rds")

  info <- medsim_cache_info(non_existent)

  expect_false(info$exists)
  expect_true(is.na(info$size_mb))
})

test_that("medsim_cache_list works", {
  # Create temp directory
  cache_dir <- tempdir()

  # Create some cache files
  for (i in 1:3) {
    file <- file.path(cache_dir, sprintf("test_cache_%d.rds", i))
    medsim_cache_save(list(i = i), file)
  }

  # List cache
  cache_list <- medsim_cache_list(cache_dir, pattern = "test_cache_*.rds")

  expect_s3_class(cache_list, "data.frame")
  expect_equal(nrow(cache_list), 3)
  expect_true("file" %in% names(cache_list))
  expect_true("size_mb" %in% names(cache_list))
  expect_true("age_days" %in% names(cache_list))

  # Cleanup
  unlink(file.path(cache_dir, "test_cache_*.rds"))
})

test_that("medsim_cache_clear removes files", {
  # Create temp directory
  cache_dir <- tempfile()
  dir.create(cache_dir)

  # Create cache files
  for (i in 1:3) {
    file <- file.path(cache_dir, sprintf("cache_%d.rds", i))
    medsim_cache_save(list(i = i), file)
  }

  # Verify files exist
  expect_equal(length(list.files(cache_dir, pattern = "*.rds")), 3)

  # Clear cache without confirmation
  deleted <- medsim_cache_clear(cache_dir, confirm = FALSE)

  expect_equal(deleted, 3)
  expect_equal(length(list.files(cache_dir, pattern = "*.rds")), 0)

  # Cleanup
  unlink(cache_dir, recursive = TRUE)
})

test_that("medsim_cache_init creates directory and README", {
  cache_dir <- tempfile()

  medsim_cache_init(cache_dir)

  expect_true(dir.exists(cache_dir))
  expect_true(file.exists(file.path(cache_dir, "README.md")))

  # Cleanup
  unlink(cache_dir, recursive = TRUE)
})

test_that("cache handles compression correctly", {
  cache_file <- tempfile(fileext = ".rds")

  # Large object
  large_object <- list(
    matrix = matrix(rnorm(10000), nrow = 100),
    vector = rnorm(10000)
  )

  # Save with compression
  medsim_cache_save(large_object, cache_file, compress = TRUE)
  size_compressed <- file.info(cache_file)$size

  unlink(cache_file)

  # Save without compression
  medsim_cache_save(large_object, cache_file, compress = FALSE)
  size_uncompressed <- file.info(cache_file)$size

  # Compressed should be smaller
  expect_true(size_compressed < size_uncompressed)

  # Both should load correctly
  loaded <- medsim_cache_load(cache_file, verbose = FALSE)
  expect_equal(dim(loaded$matrix), dim(large_object$matrix))

  # Cleanup
  unlink(cache_file)
})

test_that("cache expiry check works", {
  cache_file <- tempfile(fileext = ".rds")

  test_object <- list(value = 42)
  medsim_cache_save(test_object, cache_file)

  # Should load with reasonable max_age
  loaded <- medsim_cache_load(cache_file, check_expiry = TRUE,
                               max_age = 30, verbose = FALSE)
  expect_equal(loaded$value, 42)

  # Should not load with zero max_age (always expired)
  loaded_expired <- medsim_cache_load(cache_file, check_expiry = TRUE,
                                      max_age = 0, verbose = FALSE)
  expect_null(loaded_expired)

  # Cleanup
  unlink(cache_file)
})

# ---------------------------------------------------------------------------
# Additional coverage: overwrite=FALSE, legacy format, verbose=FALSE, max_age
# ---------------------------------------------------------------------------

test_that("medsim_cache_save with overwrite=FALSE does not replace existing file", {
  cache_file <- tempfile(fileext = ".rds")
  original <- list(v = 1)
  replacement <- list(v = 999)

  medsim_cache_save(original, cache_file)

  # overwrite=FALSE: should emit message and leave file unchanged
  expect_message(
    medsim_cache_save(replacement, cache_file, overwrite = FALSE),
    "already exists"
  )

  # file still contains original
  loaded <- medsim_cache_load(cache_file, verbose = FALSE)
  expect_equal(loaded$v, 1)

  unlink(cache_file)
})

test_that("medsim_cache_load handles legacy-format RDS (bare object, no $data)", {
  cache_file <- tempfile(fileext = ".rds")
  # Save as a raw numeric — no medsim wrapper list
  saveRDS(42.0, file = cache_file)

  # Should return the raw value, not NULL
  result <- suppressMessages(medsim_cache_load(cache_file, verbose = TRUE))
  expect_equal(result, 42.0)

  unlink(cache_file)
})

test_that("medsim_cache_load verbose=FALSE emits no message on miss", {
  missing_file <- tempfile(fileext = ".rds")
  expect_no_message(medsim_cache_load(missing_file, verbose = FALSE))
})

test_that("medsim_cache_clear with max_age skips files younger than threshold", {
  cache_dir <- tempfile()
  dir.create(cache_dir)

  # Write two fresh files (age ~ 0 days)
  for (i in 1:2) {
    medsim_cache_save(list(i = i),
                      file.path(cache_dir, sprintf("fresh_%d.rds", i)))
  }

  # max_age = 30: all files are younger, so nothing deleted
  expect_message(
    deleted <- medsim_cache_clear(cache_dir, max_age = 30, confirm = FALSE),
    "No cache files older than"
  )
  expect_equal(deleted, 0)

  # files still there
  expect_equal(length(list.files(cache_dir, pattern = "*.rds")), 2)

  unlink(cache_dir, recursive = TRUE)
})
