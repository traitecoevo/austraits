austraits <- list(austraits_4.2.0_lite, 
                  austraits_5.0.0_lite)

test_check_compatibility <- function(austraits){
  test_that("check_compatibiity successfully detects if the dataframe is compatible with austraits 3.0", {
    expect_silent(check_compatibility(austraits_5.0.0_lite))
    expect_true(check_compatibility(austraits_5.0.0_lite))
    expect_false(check_compatibility(austraits_4.2.0_lite))
  })
}
