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
  # Every MYtraits release ships a file called MYtraits.rds
  path_mytraits <- tempfile("mytraits")
  on.exit(unlink(path_mytraits, recursive = TRUE), add = TRUE)

  v1 <- load_database(doi = "10.5281/zenodo.22742825", path = path_mytraits) # v0.0.1
  v2 <- load_database(doi = "10.5281/zenodo.22961387", path = path_mytraits) # v0.0.2

  expect_equal(nrow(v1$traits), 39083)
  expect_equal(nrow(v2$traits), 45666)

  # A corrupted cached file is downloaded again, even with update = FALSE
  writeLines("not an rds", file.path(path_mytraits, "MYtraits.rds"))
  expect_message(
    v2_again <- load_database(doi = "10.5281/zenodo.22961387", path = path_mytraits, update = FALSE),
    "does not match"
  )
  expect_equal(v2_again, v2)
})

test_that("Right errors are tripped", {
  expect_error(load_database(path = path), "Please supply a doi")
  expect_error(load_database(doi = "10.5281/zenodo.10156222", path = NULL), "File path must be supplied")
  # AusTraits v0.1.0 has no .rds file
  expect_error(load_database(doi = "10.5281/zenodo.3568418", path = path), "No .rds file found")
  expect_error(load_database(doi = "10.1000/notarealdoi123", path = path), "Could not resolve doi")
})
