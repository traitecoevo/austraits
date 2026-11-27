test_that("load_austraits() and get_versions() need a release and a folder path", {
  expect_error(load_austraits(path = tempfile()), "Please supply a doi or version")
  expect_error(load_austraits(version = "7.0.0", path = 1), "single folder path")
  expect_error(get_versions(path = c("a", "b")), "single folder path")
  expect_error(get_versions(path = ""), "single folder path")
})

# Move the traits.use cache folder to a temporary one, and fake a release of
# AusTraits served from a local file, with Zenodo mocked, to keep this offline
local_cache_and_release <- function(env = parent.frame()) {
  cache <- tempfile("cache")
  src <- tempfile("release")
  dir.create(src)
  withr::defer(unlink(c(cache, src), recursive = TRUE), envir = env)
  withr::local_options(traits.use.cache_dir = cache, .local_envir = env)

  doi <- "10.5281/zenodo.99999997"
  rds <- file.path(src, "austraits-9.9.9.rds")
  saveRDS(list(traits = data.frame(value = 1:3), taxa = data.frame(), methods = data.frame(),
               sources = RefManageR::BibEntry(bibtype = "Misc", key = "Test_2030", title = "A", author = "B", year = 2030)),
          rds)
  local_mocked_bindings(
    list_versions = function(...) dplyr::tibble(version = "v9.9.9", doi = doi, publication_date = as.Date("2030-01-01")),
    resolve_doi_files = function(...) list(
      doi = doi, source = "zenodo", version = "9.9.9",
      files = data.frame(key = basename(rds), url = paste0("file://", normalizePath(rds, winslash = "/")),
                         md5 = unname(tools::md5sum(rds)))
    ),
    .package = "traits.use", .env = env
  )
  list(cache = cache, file = basename(rds))
}

test_that("load_austraits() and get_versions() use the traits.use cache folder by default", {
  x <- local_cache_and_release()
  expect_identical(normalizePath(traits.use::database_cache_dir(), mustWork = FALSE),
                   normalizePath(x$cache, mustWork = FALSE))

  expect_equal(get_version_latest(), "9.9.9")
  expect_true(file.exists(file.path(x$cache, "austraits_versions.json")))

  # path = NULL reaches traits.use::load_database(), which then reports what
  # the cache holds after a download...
  expect_message(db <- load_austraits(version = "9.9.9"), "now holds 1 database")
  expect_equal(db$traits$value, 1:3)
  expect_true(file.exists(file.path(x$cache, x$file)))
  # ...and lists it as a cached database, so clear_database_cache() can delete it
  expect_identical(list_cached_databases()$file, x$file)

  # A doi can be given as a url, as load_database() takes it
  expect_equal(suppressMessages(load_austraits(doi = "https://doi.org/10.5281/zenodo.99999997"))$traits$value, 1:3)

  # ...and marks it used when loaded again, for clear_database_cache(older_than)
  Sys.setFileTime(file.path(x$cache, x$file), Sys.time() - 30 * 86400)
  db <- suppressMessages(load_austraits(version = "9.9.9", update = FALSE))
  expect_lt(difftime(Sys.time(), list_cached_databases()$last_used, units = "days"), 1)
})

test_that("load_austraits() and get_versions() say once that 'data/austraits' is no longer the default", {
  x <- local_cache_and_release()
  wd <- tempfile("project")
  dir.create(file.path(wd, "data", "austraits"), recursive = TRUE)
  withr::defer(unlink(wd, recursive = TRUE))
  withr::local_dir(wd)
  rlang::reset_message_verbosity("austraits_old_default_folder")
  withr::defer(rlang::reset_message_verbosity("austraits_old_default_folder"))

  # Not when the folder is given
  expect_no_message(get_versions(path = file.path("data", "austraits")))
  expect_false(any(grepl("no longer|not in 'data/austraits'",
                         testthat::capture_messages(load_austraits(version = "9.9.9", path = "data/austraits")))))

  # By default, once a session
  expect_message(load_austraits(version = "9.9.9"), "path = \"data/austraits\"")
  expect_false(any(grepl("not in 'data/austraits'", testthat::capture_messages(load_austraits(version = "9.9.9")))))
  rlang::reset_message_verbosity("austraits_old_default_folder")
  expect_message(get_versions(), "path = \"data/austraits\"")

  # Not when data/austraits is the cache folder
  rlang::reset_message_verbosity("austraits_old_default_folder")
  withr::local_options(traits.use.cache_dir = file.path(wd, "data", "austraits"))
  expect_false(inform_old_default_folder())

  # Nor when there is no data/austraits
  withr::local_options(traits.use.cache_dir = x$cache)
  unlink(file.path(wd, "data"), recursive = TRUE)
  expect_false(inform_old_default_folder())
})

test_that("load_austraits(update = FALSE) loads a cache made by austraits 3.x offline", {
  local_test_cache()
  # austraits 3.x cached only austraits.json and the .rds, not the record
  # file traits.use::load_database() keeps; a fake release keeps this offline
  path <- tempfile("austraits")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  doi <- "10.5281/zenodo.99999999"
  rds <- file.path(path, "austraits-9.9.9.rds")
  saveRDS(list(traits = data.frame(value = 1:3), taxa = data.frame(), methods = data.frame(),
               sources = RefManageR::BibEntry(bibtype = "Misc", key = "Test_2030", title = "A", author = "B", year = 2030)),
          rds)
  md5 <- unname(tools::md5sum(rds))

  res <- list(hits = list(hits = list(list(
    doi = doi,
    metadata = list(publication_date = "2030-01-01", doi = doi, version = "9.9.9"),
    files = list(list(key = "austraits-9.9.9.rds", checksum = paste0("md5:", md5),
                      links = list(self = "https://zenodo.org/api/records/99999999/files/austraits-9.9.9.rds/content")))
  ))))
  jsonlite::write_json(res, file.path(path, "austraits.json"), auto_unbox = TRUE)

  # Any request to Zenodo for the record fails the test
  local_mocked_bindings(
    resolve_doi_files = function(...) stop("tried to reach Zenodo"),
    list_versions = function(...) stop("tried to reach Zenodo"),
    .package = "traits.use"
  )

  db <- suppressMessages(load_austraits(version = "9.9.9", path = path, update = FALSE))
  expect_s3_class(db, "traits.build")
  expect_equal(db$traits$value, 1:3)
  expect_s3_class(db$sources, "traits_build_sources")

  # The same again, now that the record file exists
  db <- suppressMessages(load_austraits(doi = doi, path = path, update = FALSE))
  expect_equal(db$traits$value, 1:3)

  # A folder as austraits 4.0.0 leaves it: the saved list of releases and the
  # record file, no austraits.json
  local({
    local_mocked_bindings(
      list_versions = function(...) dplyr::tibble(version = "v9.9.9", doi = doi, publication_date = as.Date("2030-01-01")),
      .package = "traits.use"
    )
    get_versions(path = path, update = TRUE)
  })
  unlink(file.path(path, "austraits.json"))
  expect_true(file.exists(file.path(path, "austraits_versions.json")))
  db <- suppressMessages(load_austraits(version = "9.9.9", path = path, update = FALSE))
  expect_equal(db$traits$value, 1:3)

  # update = TRUE offline falls back to the saved copies, saying so
  expect_message(db <- load_austraits(version = "9.9.9", path = path, update = TRUE), "using the list saved")
  expect_equal(db$traits$value, 1:3)

  # A release missing from the saved list suggests updating it
  expect_error(load_austraits(version = "9.9.10", path = path, update = FALSE), "try `update = TRUE`")
})

test_that("load_austraits(means = TRUE) loads the observed means published with a release", {
  local_test_cache()
  skip_if_not_installed("readr")
  # A fake release, as AusTraits publishes them after 7.0.0: the database and
  # its observed means, served from local files to keep this offline
  path <- tempfile("austraits")
  src <- tempfile("release")
  dir.create(path)
  dir.create(src)
  on.exit(unlink(c(path, src), recursive = TRUE), add = TRUE)

  database <- traits.use::extract_dataset(traits.use::austraits_lite, "Falster_2003")
  published <- summarise_trait_means(database)
  # As AusTraits 8.0.0 publishes them: the means as .rds too, listed first
  files <- c(means_rds = file.path(src, "austraits-9.9.9-observed-means.rds"),
             rds = file.path(src, "austraits-9.9.9.rds"),
             means = file.path(src, "austraits-9.9.9-observed-means.csv.gz"))
  saveRDS(published, files[["means_rds"]])
  saveRDS(database, files[["rds"]])
  readr::write_csv(published, files[["means"]], na = "")

  doi <- "10.5281/zenodo.99999998"
  file_url <- function(file) paste0("file://", normalizePath(file, winslash = "/"))
  res <- list(hits = list(hits = list(list(
    doi = doi,
    metadata = list(publication_date = "2030-01-01", doi = doi, version = "9.9.9"),
    # an array, as Zenodo gives it
    files = lapply(unname(files), function(file) list(
      key = basename(file), checksum = paste0("md5:", unname(tools::md5sum(file))),
      links = list(self = file_url(file))
    ))
  ))))
  jsonlite::write_json(res, file.path(path, "austraits.json"), auto_unbox = TRUE)
  local_mocked_bindings(
    resolve_doi_files = function(...) stop("tried to reach Zenodo"),
    list_versions = function(...) stop("tried to reach Zenodo"),
    .package = "traits.use"
  )

  means <- suppressMessages(load_austraits(version = "9.9.9", path = path, update = FALSE, means = TRUE))
  # read from the .rds (traits.use 1.0.1), so exactly as computed
  expect_identical(means, tibble::as_tibble(published))
  # only the means were downloaded
  expect_false(file.exists(file.path(path, "austraits-9.9.9.rds")))
  expect_false(file.exists(file.path(path, "austraits-9.9.9-observed-means.csv.gz")))

  # the database, not the means .rds beside it (traits.use 1.0.1)
  db <- suppressMessages(load_austraits(version = "9.9.9", path = path, update = FALSE))
  expect_s3_class(db, "traits.build")
  expect_equal(nrow(db$traits), nrow(database$traits))
})

test_that("A path keeps a copy of the release and the list, which loads offline on its own", {
  x <- local_cache_and_release()
  project <- tempfile("project")
  withr::defer(unlink(project, recursive = TRUE))

  db <- suppressMessages(load_austraits(version = "9.9.9", path = project))
  expect_setequal(list.files(project), c(x$file, "10.5281_zenodo.99999997.json", "austraits_versions.json"))
  expect_true(file.exists(file.path(x$cache, x$file)))

  # On another computer, or after clearing the cache, and offline: the
  # folder loads on its own, and the cache takes nothing Zenodo has not confirmed
  unlink(x$cache, recursive = TRUE)
  online <- list(list_versions = traits.use:::list_versions, resolve_doi_files = traits.use:::resolve_doi_files)
  local_mocked_bindings(
    list_versions = function(...) stop("tried to reach Zenodo"),
    resolve_doi_files = function(...) stop("tried to reach Zenodo"),
    download_database = function(...) stop("tried to download"),
    .package = "traits.use"
  )
  msgs <- testthat::capture_messages(offline <- load_austraits(version = "9.9.9", path = project))
  expect_equal(offline, db)
  expect_true(any(grepl("using the list saved in .*project", msgs)))
  expect_length(list.files(x$cache), 0)

  # Online again, Zenodo confirms the folder's copy, which fills the cache
  # without a download
  local_mocked_bindings(list_versions = online$list_versions, resolve_doi_files = online$resolve_doi_files,
                        .package = "traits.use")
  expect_equal(suppressMessages(load_austraits(version = "9.9.9", path = project)), db)
  expect_true(file.exists(file.path(x$cache, x$file)))
})

test_that("A release newer than the saved list is found, and loading an older one says so once", {
  x <- local_cache_and_release()
  rlang::reset_message_verbosity("austraits_not_latest_9.9.9")
  withr::defer(rlang::reset_message_verbosity("austraits_not_latest_9.9.9"))
  doi <- function(id) paste0("10.5281/zenodo.", id)
  listing <- dplyr::tibble(version = "v9.9.8", doi = doi(99999998), publication_date = as.Date("2029-01-01"))
  n <- 0
  local_mocked_bindings(list_versions = function(...) { n <<- n + 1; listing }, .package = "traits.use")

  # The saved list predates 9.9.9
  get_versions()
  expect_identical(n, 1)
  listing <- dplyr::tibble(version = c("v9.9.10", "v9.9.9", "v9.9.8"),
                           doi = doi(c(99999996, 99999997, 99999998)),
                           publication_date = as.Date(c("2031-01-01", "2030-01-01", "2029-01-01")))

  # so it is fetched afresh, and 9.9.10 is the latest
  msgs <- testthat::capture_messages(db <- load_austraits(version = "9.9.9"))
  expect_identical(n, 2)
  expect_equal(db$traits$value, 1:3)
  expect_true(any(grepl("AusTraits 9.9.10 is the latest release; loading 9.9.9", msgs)))
  msgs <- testthat::capture_messages(load_austraits(version = "9.9.9"))
  expect_false(any(grepl("is the latest release", msgs)))
  expect_identical(n, 2)

  # A release that is not in the fresh list either
  expect_error(suppressMessages(load_austraits(version = "9.9.11")), "Requested version/doi is incorrect")
  expect_identical(n, 3)
  expect_error(load_austraits(version = "9.9.11", update = FALSE), "try `update = TRUE`")
  expect_identical(n, 3)
})

test_that("A release downloaded before loads offline, however old the saved list and record", {
  x <- local_cache_and_release()
  db <- suppressMessages(load_austraits(version = "9.9.9"))

  # A year later, and offline
  old <- Sys.time() - 365 * 86400
  Sys.setFileTime(file.path(x$cache, c("austraits_versions.json", "10.5281_zenodo.99999997.json", x$file)), old)
  local_mocked_bindings(
    list_versions = function(...) stop("tried to reach Zenodo"),
    resolve_doi_files = function(...) stop("tried to reach Zenodo"),
    download_database = function(...) stop("tried to download"),
    .package = "traits.use"
  )
  msgs <- testthat::capture_messages(offline <- load_austraits(version = "9.9.9"))
  expect_equal(offline, db)
  # saying that Zenodo could not be reached, for the list and the record
  expect_true(any(grepl("Could not list the releases of AusTraits.*using the list saved", msgs)))
  expect_true(any(grepl("Could not update the record.*using the copy saved", msgs)))

  # update = FALSE loads it without trying, so without the messages
  msgs <- testthat::capture_messages(offline <- load_austraits(version = "9.9.9", update = FALSE))
  expect_equal(offline, db)
  expect_false(any(grepl("Could not", msgs)))

  # as do get_versions() and get_version_latest(), from the saved list
  expect_message(expect_equal(get_version_latest(), "9.9.9"), "using the list saved")
})
