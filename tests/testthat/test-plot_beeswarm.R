test_that("Function doesn't throw error", {
expect_silent(austraits %>% plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Westoby_2014"))
}) 

