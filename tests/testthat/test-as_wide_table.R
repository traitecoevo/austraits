library(purrr)

austraits <- list(austraits_3.0.2_lite,
                  austraits_4.2.0_lite, 
                  austraits_5.0.0_lite)

test_widetable_success <- function(austraits){
  test_that("Function is working", {
    expect_visible(austraits %>% as_wide_table())
    out <- austraits %>% as_wide_table()
    expect_named(out)
    expect_type(out, "list")
 
    # "Output is correct"
    expect_gt(out %>% ncol(), expected = austraits$traits %>% ncol())
  })
}

walk(austraits, 
      test_widetable_success)
  
expect_error(as_wide_table())
