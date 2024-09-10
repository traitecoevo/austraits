not_supported_austraits <- list(austraits_3.0.2_lite, 
                                austraits_4.2.0_lite) 

test_join_error <- function(austraits){
  test_that("old versions will complain", {
    # expect_error(join_locations(austraits)
    expect_error(join_methods(austraits))
    # expect_error(join_contexts(austraits))
    expect_error(join_taxonomy(austraits))
    # expect_error(join_all(austraits))
  })
}

purrr::walk(not_supported_austraits,
            test_join_error)

test_that("functions should work without warnings", {
  # expect_silent(join_locations(austraits))
  expect_silent(join_methods(austraits_5.0.0_lite))
  # expect_silent(join_contexts(austraits))
  expect_silent(join_taxonomy(austraits_5.0.0_lite))
  # expect_silent(join_all(austraits))
})

test_that("structure doesn't change", {
  # expect_type((join_locations(austraits)), "list")
  expect_type((join_methods(austraits_5.0.0_lite)), "list")
  # expect_type((join_contexts(austraits)), "list")
  expect_type((join_taxonomy(austraits_5.0.0_lite)), "list")
  # expect_type((join_all(austraits)), "list")
})

test_that("variables are added", {
  # expect_true(ncol(join_locations(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  expect_true(ncol(join_methods(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  # expect_true(ncol(join_contexts(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits)) #Need an example where I have context information to add
  # expect_true(ncol(join_taxonomy(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  # expect_true(ncol(join_all(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  # 
  # expect_true(any(names(join_locations(austraits_5.0.0_lite)$traits) %in% c("latitude (deg)","longitude (deg)")))
  expect_true(any(names(join_taxonomy(austraits_5.0.0_lite)$traits) %in%c("family", "genus")))
  # expect_true(any(names(join_methods(austraits_5.0.0_lite)$traits) %in%c("methods")))
})



