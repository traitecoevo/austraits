path = "ignore/data/austraits"

test_that("load_austraits is working", {
  austraits <- load_austraits(version = "3.0.2", path = path, update = FALSE)
  
  expect_visible(austraits)
  expect_named(austraits)
  expect_type(austraits, "list")
  expect_length(austraits, 11)
  expect_error(load_austrait())
  })

test_that("get_versions is working", {
  x <- get_versions(path = path, update = FALSE)
  
  expect_visible(x)
  expect_named(x)
  expect_type(x, "list")
  expect_length(x, ncol(x))
})

test_that("Right errors are tripped", {
  expect_error(load_austraits("3.0.6", path = path))
  expect_error(load_austraits(doi = "10.5281/zenodo.5112005",
                                   path = path))
  expect_error(load_austraits(path = NULL))
})

