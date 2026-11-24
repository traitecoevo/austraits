test_that("load_austraits(update = FALSE) loads a cache made by austraits 3.x offline", {
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
    get_versions(path = path)
  })
  unlink(file.path(path, "austraits.json"))
  expect_true(file.exists(file.path(path, "austraits_versions.json")))
  db <- suppressMessages(load_austraits(version = "9.9.9", path = path, update = FALSE))
  expect_equal(db$traits$value, 1:3)

  # update = TRUE offline falls back to the saved copies, saying so
  expect_message(db <- load_austraits(version = "9.9.9", path = path), "using the list saved")
  expect_equal(db$traits$value, 1:3)

  # A release missing from the saved list suggests updating it
  expect_error(load_austraits(version = "9.9.10", path = path, update = FALSE), "try `update = TRUE`")
})

test_that("load_austraits(means = TRUE) loads the observed means published with a release", {
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
  files <- c(rds = file.path(src, "austraits-9.9.9.rds"),
             means = file.path(src, "austraits-9.9.9-observed-means.csv.gz"))
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
  expect_equal(as.data.frame(means), as.data.frame(published))
  # only the means were downloaded
  expect_false(file.exists(file.path(path, "austraits-9.9.9.rds")))

  db <- suppressMessages(load_austraits(version = "9.9.9", path = path, update = FALSE))
  expect_s3_class(db, "traits.build")
})
