# get_versions() over traits.use::list_versions(), with Zenodo mocked
zenodo_listing <- function() {
  dplyr::tibble(
    version = c("v7.0.0", "v6.0.0", "v3.0.2", "v3.0.1", "v0.9.0"),
    doi = paste0("10.5281/zenodo.", c(15718081, 11188867, 5112001, 5099552, 3568429)),
    publication_date = as.Date(c("2025-06-20", "2024-05-14", "2021-07-14", "2021-07-14", "2019-12-09"))
  )
}

test_that("get_versions() returns the table it always has, from list_versions()", {
  local_test_cache()
  path <- tempfile("austraits")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  asked <- NULL
  local_mocked_bindings(
    list_versions = function(doi) { asked <<- doi; zenodo_listing() },
    .package = "traits.use"
  )

  x <- get_versions(path = path)
  expect_identical(asked, "10.5281/zenodo.3568417")
  expect_named(x, c("publication_date", "doi", "version", "id"))
  # No "v", and nothing before 3.0.2
  expect_equal(x$version, c("7.0.0", "6.0.0", "3.0.2"))
  expect_equal(x$id, c("15718081", "11188867", "5112001"))
  expect_s3_class(x$publication_date, "Date")
  expect_equal(get_version_latest(path = path), "7.0.0")

  # update = FALSE uses the saved list without asking Zenodo
  local_mocked_bindings(list_versions = function(doi) stop("tried to reach Zenodo"), .package = "traits.use")
  expect_identical(get_versions(path = path, update = FALSE), x)
  expect_equal(get_version_latest(path = path, update = FALSE), "7.0.0")

  # Offline, update = TRUE falls back to the saved list, saying so
  expect_message(y <- get_versions(path = path, update = TRUE), "using the list saved")
  expect_identical(y, x)
})

test_that("get_versions() needs Zenodo when nothing is saved", {
  local_test_cache()
  path <- tempfile("austraits")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  local_mocked_bindings(list_versions = function(doi) stop("tried to reach Zenodo"), .package = "traits.use")
  expect_error(get_versions(path = path, update = FALSE), "tried to reach Zenodo")
  expect_error(get_versions(path = path), "tried to reach Zenodo")
})

test_that("get_versions(update = FALSE) reads the austraits.json saved by austraits 3.x", {
  local_test_cache()
  path <- tempfile("austraits")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  listing <- zenodo_listing()
  res <- list(hits = list(hits = lapply(seq_len(nrow(listing)), function(i) list(
    doi = listing$doi[i],
    metadata = list(version = listing$version[i], publication_date = as.character(listing$publication_date[i]))
  ))))
  jsonlite::write_json(res, file.path(path, "austraits.json"), auto_unbox = TRUE)
  local_mocked_bindings(list_versions = function(doi) stop("tried to reach Zenodo"), .package = "traits.use")

  x <- get_versions(path = path, update = FALSE)
  expect_equal(x$version, c("7.0.0", "6.0.0", "3.0.2"))
  expect_equal(x$doi[1], "10.5281/zenodo.15718081")
})

test_that("get_versions() asks Zenodo by default; update = NULL only once the saved list is 60 days old", {
  cache <- local_test_cache()
  n <- 0
  local_mocked_bindings(list_versions = function(doi) { n <<- n + 1; zenodo_listing() }, .package = "traits.use")
  saved <- file.path(cache, "austraits_versions.json")

  # Called directly, the list and the latest release are current
  x <- get_versions()
  expect_true(file.exists(saved))
  expect_identical(get_versions(), x)
  expect_equal(get_version_latest(), "7.0.0")
  expect_identical(n, 3)

  # update = NULL, as load_austraits() calls it, uses the saved list until it is old
  n <- 1
  expect_identical(get_versions(update = NULL), x)
  expect_identical(n, 1)
  Sys.setFileTime(saved, Sys.time() - 59 * 86400)
  get_version_latest(update = NULL)
  expect_identical(n, 1)
  Sys.setFileTime(saved, Sys.time() - 61 * 86400)
  expect_identical(get_versions(update = NULL), x)
  expect_identical(n, 2)

  # update = FALSE never fetches if a list is saved
  Sys.setFileTime(saved, Sys.time() - 400 * 86400)
  get_versions(update = FALSE)
  expect_identical(n, 2)

  # The number of days is traits.use's option
  Sys.setFileTime(saved, Sys.time())
  withr::local_options(traits.use.update_after_days = 0)
  get_versions(update = NULL)
  expect_identical(n, 3)

  expect_error(get_versions(update = NA), "TRUE, FALSE or NULL")
  expect_error(get_versions(update = "yes"), "TRUE, FALSE or NULL")
})

test_that("A path gets a copy of the list, and a list saved there stands in for the cache's", {
  cache <- local_test_cache()
  path <- tempfile("project")
  withr::defer(unlink(path, recursive = TRUE))
  local_mocked_bindings(list_versions = function(doi) zenodo_listing(), .package = "traits.use")

  x <- get_versions(path = path)
  expect_true(file.exists(file.path(path, "austraits_versions.json")))
  expect_true(file.exists(file.path(cache, "austraits_versions.json")))

  # e.g. a project folder from austraits 4.0, on a computer with an empty
  # cache: its list is used when Zenodo cannot be reached, or is not asked
  unlink(file.path(cache, "austraits_versions.json"))
  local_mocked_bindings(list_versions = function(doi) stop("tried to reach Zenodo"), .package = "traits.use")
  expect_message(y <- get_versions(path = path), "using the list saved in .*project")
  expect_identical(y, x)
  expect_no_message(expect_identical(get_versions(path = path, update = FALSE), x))
  expect_false(file.exists(file.path(cache, "austraits_versions.json")))
  expect_error(get_versions(), "tried to reach Zenodo")

  # Online, Zenodo's list is used, whatever the folder's says and however new
  # its file is
  jsonlite::write_json(data.frame(publication_date = "2031-01-01", doi = "10.5281/zenodo.1", version = "99.0.0", id = "1"),
                       file.path(path, "austraits_versions.json"))
  local_mocked_bindings(list_versions = function(doi) zenodo_listing(), .package = "traits.use")
  expect_identical(get_versions(path = path), x)
  expect_equal(get_version_latest(path = path), "7.0.0")
})
