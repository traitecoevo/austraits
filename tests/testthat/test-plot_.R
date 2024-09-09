
test_that("Function doesn't throw error", {
  expect_invisible(austraits_5.0.0_lite %>% plot_trait_distribution_beeswarm("leaf_mass_per_area", "dataset_id", "Bloomfield_2018"))
  
  # this function is currently really slow, blokcing effective testing
  expect_invisible((austraits_5.0.0_lite %>% extract_trait("wood_density") %>% join_locations())$trait %>% plot_locations())
  expect_invisible((austraits_5.0.0_lite %>% extract_trait("wood_density") %>% join_locations())$trait %>% plot_locations())
 }) 
 
