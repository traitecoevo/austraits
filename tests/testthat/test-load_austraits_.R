# test_that("Function created directory", {
#   austraits <- load_austraits()
#   expect_true(file.exists("data/austraits"))
# })

test_that("Load lite is working", {
  austraits_mini <- load_austraits_lite()
  expect_equal(austraits_mini, austraits::austraits)
  expect_equal(class(austraits_mini), class(austraits))
  expect_named(austraits, names(austraits_mini))
})
