library(purrr)

austraits <- list(austraits_lite,
                  austraits_lite_post)

test_widetable_success <- function(austraits){
  test_that("Function is working", {
    expect_visible(austraits %>% as_wide_table())
    expect_named(austraits %>% as_wide_table())
    expect_type(austraits %>% as_wide_table(), "list")
    
    expect_visible(austraits %>% as_wide_table())
    expect_named(austraits %>% as_wide_table())
    expect_type(austraits %>% as_wide_table(), "list")
  })
}

map(austraits, 
    test_widetable_success)

test_widetable_output <-function(austraits){
  test_that("Output is correct", {
    expect_gt(austraits %>% as_wide_table() %>% ncol(), expected = austraits$traits %>% ncol())
    expect_gt(austraits %>% as_wide_table() %>% ncol(), expected = austraits$traits %>% ncol())
  })
}

map(austraits, 
    test_widetable_output)

test_widetable_errors <- function(austraits){
  test_that("Complains at the right time", {
    expect_error(as_wide_table())
  })
}

map(austraits, 
    test_widetable_errors)
  