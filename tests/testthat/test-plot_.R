austraits <- austraits_lite

test_that("Function doesn't throw error", {
expect_invisible(austraits %>% plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Westoby_2014"))
expect_invisible((austraits %>% join_locations())$trait %>% plot_locations())
expect_silent(austraits %>% plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Westoby_2014"))
expect_silent((austraits %>% join_locations())$trait %>% plot_locations())
}) 

austraits <- austraits_lite_post

test_that("Function doesn't throw error", {
  expect_invisible(austraits %>% plot_trait_distribution_beeswarm("leaf_mass_per_area", "dataset_id", "Bloomfield_2018"))
  expect_invisible((austraits %>% join_locations())$trait %>% plot_locations())
  expect_silent(austraits %>% plot_trait_distribution_beeswarm("leaf_mass_per_area", "dataset_id", "Bloomfield_2018"))
  expect_silent((austraits %>% join_locations())$trait %>% plot_locations())
}) 

