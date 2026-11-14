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
    .package = "traits.use"
  )

  db <- suppressMessages(load_austraits(version = "9.9.9", path = path, update = FALSE))
  expect_s3_class(db, "traits.build")
  expect_equal(db$traits$value, 1:3)
  expect_s3_class(db$sources, "traits_build_sources")

  # The same again, now that the record file exists
  db <- suppressMessages(load_austraits(doi = doi, path = path, update = FALSE))
  expect_equal(db$traits$value, 1:3)
})
