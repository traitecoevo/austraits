# get_versions() over traits.use::list_versions(), with Zenodo mocked
zenodo_listing <- function() {
  dplyr::tibble(
    version = c("v7.0.0", "v6.0.0", "v3.0.2", "v3.0.1", "v0.9.0"),
    doi = paste0("10.5281/zenodo.", c(15718081, 11188867, 5112001, 5099552, 3568429)),
    publication_date = as.Date(c("2025-06-20", "2024-05-14", "2021-07-14", "2021-07-14", "2019-12-09"))
  )
}

test_that("get_versions() returns the table it always has, from list_versions()", {
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
  expect_message(y <- get_versions(path = path), "using the list saved")
  expect_identical(y, x)
})

test_that("get_versions() needs Zenodo when nothing is saved", {
  path <- tempfile("austraits")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  local_mocked_bindings(list_versions = function(doi) stop("tried to reach Zenodo"), .package = "traits.use")
  expect_error(get_versions(path = path, update = FALSE), "tried to reach Zenodo")
  expect_error(get_versions(path = path), "tried to reach Zenodo")
})

test_that("get_versions(update = FALSE) reads the austraits.json saved by austraits 3.x", {
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
