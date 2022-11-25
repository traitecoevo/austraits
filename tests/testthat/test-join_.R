austraits <- austraits_lite

test_that("functions should work without warnings", {
  expect_silent(join_locations(austraits))
  expect_silent(join_methods(austraits))
  expect_silent(join_contexts(austraits))
  expect_silent(join_taxonomy(austraits))
  expect_silent(join_all(austraits))
})

test_that("structure doesn't change", {
  expect_type((join_locations(austraits)), "list")
  expect_type((join_methods(austraits)), "list")
  expect_type((join_contexts(austraits)), "list")
  expect_type((join_taxonomy(austraits)), "list")
  expect_type((join_all(austraits)), "list")
})

test_that("variables are added", {
 expect_true(ncol(join_locations(austraits)$traits) > ncol(austraits$traits))
 expect_true(ncol(join_methods(austraits)$traits) > ncol(austraits$traits))
 #expect_true(ncol(join_contexts(austraits)$traits) > ncol(austraits$traits)) #Need an example where I have context information to add
 expect_true(ncol(join_taxonomy(austraits)$traits) > ncol(austraits$traits))
 expect_true(ncol(join_all(austraits)$traits) > ncol(austraits$traits))
})

test_that("are the right variables added?", {
  expect_true(any(names(join_locations(austraits)$traits) == c("latitude (deg)","longitude (deg)")))
  expect_true(any(names(join_taxonomy(austraits)$traits) == c("family", "genus", "taxonRank", "acceptedNameUsageID")))
  expect_true(any(names(join_methods(austraits)$traits) == c("methods", "year_collected_start", "year_collected_end", "collection_type")))
 #expect_true(any(names(join_contexts(austraits)$traits) ==  c("dataset_id","context_name","context_property","value")))
})



austraits <- austraits_lite_post

test_that("functions should work without warnings", {
  expect_silent(join_locations(austraits))
  expect_silent(join_methods(austraits))
  expect_silent(join_contexts(austraits))
  expect_silent(join_taxonomy(austraits))
  expect_silent(join_all(austraits))
})

test_that("structure doesn't change", {
  expect_type((join_locations(austraits)), "list")
  expect_type((join_methods(austraits)), "list")
  expect_type((join_contexts(austraits)), "list")
  expect_type((join_taxonomy(austraits)), "list")
  expect_type((join_all(austraits)), "list")
})

test_that("variables are added", {
  expect_true(ncol(join_locations(austraits)$traits) > ncol(austraits$traits))
  expect_true(ncol(join_methods(austraits)$traits) > ncol(austraits$traits))
  #expect_true(ncol(join_contexts(austraits)$traits) > ncol(austraits$traits)) #Need an example where I have context information to add
  expect_true(ncol(join_taxonomy(austraits)$traits) > ncol(austraits$traits))
  expect_true(ncol(join_all(austraits)$traits) > ncol(austraits$traits))
})
