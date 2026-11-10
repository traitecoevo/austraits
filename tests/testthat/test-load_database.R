path = "ignore/data/austraits"

test_that("load_database is working", {
  database <- load_database(doi = "10.5281/zenodo.10156222", path = path, update = TRUE)
  
  expect_visible(database)
  expect_named(database)
  expect_type(database, "list")
  expect_equal(class(database), "traits.build")
  
  # doi url prefix is removed and cached record json is reused
  database2 <- load_database(doi = "https://doi.org/10.5281/zenodo.10156222", path = path, update = FALSE)
  expect_equal(database, database2)
})

test_that("A cached file from a different release is not reused", {
  # Some databases reuse one filename for every release (every MYtraits
  # release is MYtraits.rds), so cached files can only be told apart by md5.
  # Two fake releases, served from local files, keep this test offline.
  src <- tempfile("releases")
  path_cache <- tempfile("cache")
  dir.create(src)
  on.exit(unlink(c(src, path_cache), recursive = TRUE), add = TRUE)

  file_url <- function(file) {
    file <- normalizePath(file, winslash = "/")
    paste0(if (startsWith(file, "/")) "file://" else "file:///", file)
  }

  make_release <- function(version, n, md5 = NULL) {
    dir.create(file.path(src, version))
    file <- file.path(src, version, "MYtraits.rds")
    saveRDS(list(traits = data.frame(value = seq_len(n)), taxa = data.frame(), methods = data.frame()), file)
    list(doi = paste0("10.5281/zenodo.", version), source = "zenodo", version = version,
         files = data.frame(key = "MYtraits.rds", url = file_url(file),
                            md5 = if (is.null(md5)) unname(tools::md5sum(file)) else md5))
  }

  releases <- list(
    "10.5281/zenodo.1" = make_release("1", 3),
    "10.5281/zenodo.2" = make_release("2", 5),
    "10.5281/zenodo.3" = make_release("3", 7, md5 = strrep("0", 32))
  )
  local_mocked_bindings(resolve_doi_files = function(doi) releases[[doi]])

  expect_message(v1 <- load_database(doi = "10.5281/zenodo.1", path = path_cache), "is version 1")
  v2 <- load_database(doi = "10.5281/zenodo.2", path = path_cache)
  expect_equal(nrow(v1$traits), 3)
  expect_equal(nrow(v2$traits), 5)

  # Going back to release 1 offline replaces release 2's file of the same name
  expect_message(
    v1_again <- load_database(doi = "10.5281/zenodo.1", path = path_cache, update = FALSE),
    "does not match"
  )
  expect_equal(v1_again, v1)

  # A corrupted cached file is downloaded again, even with update = FALSE
  writeLines("not an rds", file.path(path_cache, "MYtraits.rds"))
  expect_message(
    v1_again <- load_database(doi = "10.5281/zenodo.1", path = path_cache, update = FALSE),
    "does not match"
  )
  expect_equal(v1_again, v1)

  # A download that does not match its checksum stops instead of loading
  expect_error(
    suppressMessages(load_database(doi = "10.5281/zenodo.3", path = path_cache)),
    "does not match the md5 checksum"
  )
})

test_that("download_austraits saves to the full path and restores the timeout", {
  src <- tempfile(fileext = ".rds")
  dest <- tempfile("dest")
  dir.create(dest)
  on.exit(unlink(c(src, dest), recursive = TRUE), add = TRUE)
  saveRDS(1:3, src)
  src <- normalizePath(src, winslash = "/")
  url <- paste0(if (startsWith(src, "/")) "file://" else "file:///", src)

  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)

  filename <- file.path(dest, "db.rds")
  suppressMessages(download_austraits(url, filename, path = dest, name = "test"))

  expect_equal(list.files(dest), "db.rds")
  expect_equal(readRDS(filename), 1:3)
  expect_equal(getOption("timeout"), 60)
})

test_that("Restricted Zenodo records give a clear error", {
  local_mocked_bindings(
    read_json = function(...) list(id = 22961387, metadata = list(access_right = "restricted", version = "0.0.2"), files = list()),
    .package = "jsonlite"
  )
  expect_error(resolve_doi_files("10.5281/zenodo.22961387"), "are restricted on Zenodo")
})

test_that("Right errors are tripped", {
  expect_error(load_database(path = path), "Please supply a doi")
  expect_error(load_database(doi = "10.5281/zenodo.10156222", path = NULL), "File path must be supplied")
  # AusTraits v0.1.0 has no .rds file
  expect_error(load_database(doi = "10.5281/zenodo.3568418", path = path), "No .rds file found")
  expect_error(load_database(doi = "10.1000/notarealdoi123", path = path), "Could not resolve doi")
})
