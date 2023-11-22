library(purrr)

austraits <- list(austraits_3.0.2_lite,
                  austraits_4.2.0_lite, 
                  austraits_5.0.0_lite)

test_join_success <- function(austraits){
  test_that("functions should work without warnings", {
    expect_silent(join_locations(austraits))
    expect_silent(join_methods(austraits))
    expect_silent(join_contexts(austraits))
    expect_silent(join_taxonomy(austraits))
    expect_silent(join_all(austraits))
  })
}

map(austraits, 
    test_join_success)

test_join_str <- function(austraits){
  test_that("structure doesn't change", {
    expect_type((join_locations(austraits)), "list")
    expect_type((join_methods(austraits)), "list")
    expect_type((join_contexts(austraits)), "list")
    expect_type((join_taxonomy(austraits)), "list")
    expect_type((join_all(austraits)), "list")
  })
}

map(austraits, 
    test_join_str)

test_join_output <- function(austraits){
  test_that("variables are added", {
    expect_true(ncol(join_locations(austraits)$traits) > ncol(austraits$traits))
    expect_true(ncol(join_methods(austraits)$traits) > ncol(austraits$traits))
    expect_true(ncol(join_contexts(austraits)$traits) > ncol(austraits$traits)) #Need an example where I have context information to add
    expect_true(ncol(join_taxonomy(austraits)$traits) > ncol(austraits$traits))
    expect_true(ncol(join_all(austraits)$traits) > ncol(austraits$traits))
    
    expect_true(any(names(join_locations(austraits)$traits) %in% c("latitude (deg)","longitude (deg)")))
    expect_true(any(names(join_taxonomy(austraits)$traits) %in%c("family", "genus")))
    expect_true(any(names(join_methods(austraits)$traits) %in%c("methods")))
  })
}

map(austraits, 
    test_join_output)



