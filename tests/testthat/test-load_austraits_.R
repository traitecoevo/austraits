test_that("Function is working", {
  path = "ignore/data/austraits"
  austraits <- load_austraits(version = "3.0.2", path = path, update = FALSE)
  
  expect_visible(austraits)
  expect_named(austraits)
  expect_type(austraits, "list")
  expect_length(austraits, 11)
  expect_error(load_austrait())
  })

test_that("Right errors are tripped", {
  expect_error(x <- load_austraits("3.0.6", path = "ignore/data/austraits"))
  expect_error(x <- load_austraits(doi = "10.5281/zenodo.5112005",
                                   path = "ignore/data/austraits"))
  expect_message(x <- load_austraits(version = "3.0.2",
                                   doi = "10.5281/zenodo.5099552",
                                   path = "ignore/data/austraits"))
})

