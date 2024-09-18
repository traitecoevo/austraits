# test lookup_ functions

test_that("the lookup_ functions are working as intended", {

  expect_gt(length(austraits_5.0.0_lite %>% lookup_trait("leaf")), 50)
  expect_error(austraits_5.0.0_lite %>% lookup_trait("this is not a trait"))
  
  expect_gt(length(austraits_5.0.0_lite %>% lookup_location_property("soil")), 10)
  expect_vector(austraits_5.0.0_lite %>% lookup_location_property("precipitation"))
  expect_error(austraits_5.0.0_lite %>% lookup_location_property("this is not a location property"))
  
  expect_gt(length(austraits_5.0.0_lite %>% lookup_context_property("temperature")), 1)
  expect_vector(austraits_5.0.0_lite %>% lookup_context_property("season"))
  expect_error(austraits_5.0.0_lite %>% lookup_context_property("this is not a context property"))

})
