not_supported_austraits <- list(austraits_3.0.2_lite,
                                austraits_4.2.0_lite)

test_join_error <- function(austraits){
  test_that("old versions will complain", {
    expect_error(join_location_coordinates(austraits))
    expect_error(join_methods(austraits))
    expect_error(join_context_properties(austraits))
    expect_error(join_taxa(austraits))
    expect_error(join_taxonomic_updates(austraits))
  })
}

purrr::walk(not_supported_austraits,
            test_join_error)

test_that("functions should work without warnings", {
  expect_silent(join_location_coordinates(austraits_5.0.0_lite))
  expect_silent(join_methods(austraits_5.0.0_lite))
  expect_silent(join_context_properties(austraits_5.0.0_lite))
  expect_silent(join_taxa(austraits_5.0.0_lite))
  expect_silent(join_taxonomic_updates(austraits_5.0.0_lite))
})

test_that("structure doesn't change", {
  expect_type(join_location_coordinates(austraits_5.0.0_lite), "list")
  expect_type(join_methods(austraits_5.0.0_lite), "list")
  expect_type(join_context_properties(austraits_5.0.0_lite), "list")
  expect_type(join_taxa(austraits_5.0.0_lite), "list")
  expect_type(join_taxonomic_updates(austraits_5.0.0_lite), "list")
})

test_that("variables are added", {
  expect_true(ncol(join_location_coordinates(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  expect_true(ncol(join_methods(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  expect_true(ncol(join_context_properties(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits)) #Need an example where I have context information to add
  expect_true(ncol(join_taxa(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  expect_true(ncol(join_taxonomic_updates(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  
  expect_true(any(names(join_location_coordinates(austraits_5.0.0_lite)$traits) %in% c("latitude (deg)","longitude (deg)")))
  expect_true(any(names(join_taxa(austraits_5.0.0_lite)$traits) %in%c("family", "genus")))
  expect_true(any(names(join_methods(austraits_5.0.0_lite)$traits) %in%c("methods")))
})
