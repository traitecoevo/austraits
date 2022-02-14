path = "ignore/data/austraits"
austraits <- load_austraits(version = "3.0.2", path = path, update = FALSE)

test_that("Function is working", {
  expect_visible(austraits)
  expect_named(austraits)
  expect_type(austraits, "list")
  expect_length(austraits, 11)
})

test_that("Throws errors", {
  expect_error(load_austrait())
  })


