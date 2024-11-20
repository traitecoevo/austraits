austraits <- list(austraits_3.0.2_lite, 
                  austraits_5.0.0_lite)

test_check_compatibility <- function(austraits){
  test_that("check_compatibiity successfully detects if the dataframe is compatible with austraits 3.0", {
    expect_silent(check_compatibility(austraits_5.0.0_lite))
    expect_true(check_compatibility(austraits_5.0.0_lite))
    expect_message(check_compatibility(austraits_3.0.2_lite))
    expect_false(check_compatibility(austraits_3.0.2_lite))
    
    expect_error(austraits_3.0.2_lite %>% join_location_coordinates())
    expect_error(austraits_3.0.2_lite %>% join_location_properties())
    expect_error(austraits_3.0.2_lite %>% join_contributors())
    expect_error(austraits_3.0.2_lite %>% join_methods())
    expect_error(austraits_3.0.2_lite %>% join_taxa())
    expect_error(austraits_3.0.2_lite %>% join_context_properties())
    expect_error(austraits_3.0.2_lite %>% extract_data("traits", "wood_density"))
  })
}
