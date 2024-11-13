not_supported_austraits <- list(austraits_3.0.2_lite, austraits_4.2.0_lite)


test_that("Function doesn't throw error", {
  
  #expect_invisible(austraits_5.0.0_lite %>% plot_trait_distribution_beeswarm("leaf_mass_per_area", "dataset_id", "Bloomfield_2018"))
  
  expect_invisible(austraits_5.0.0_lite %>% plot_trait_distribution_beeswarm("leaf_mass_per_area", "dataset_id"))
  
  # this function is currently really slow, blocking effective testing
  expect_invisible((austraits_5.0.0_lite %>% extract_trait("wood_density") %>% join_location_coordinates())$trait %>% plot_locations())
  expect_invisible(austraits_5.0.0_lite %>% extract_trait("wood_density") %>% plot_locations())
 }) 



test_non_compatibile <- function(austraits){
  test_that("Throws correct errors for deprecated or non-compatible",{
    expect_error(austraits %>% 
                   extract_dataset("Falster_2003") %>% 
                   purrr::pluck("traits") %>% plot_locations()
    )
    
    expect_error(austraits %>% 
                   extract_dataset("Falster_2003") %>% 
                   purrr::pluck("traits") %>% plot_lplot_site_locationsocations()
    )
  })
}

purrr::walk(not_supported_austraits,
            test_non_compatibile)

# Tear down code
unlink("Rplots.pdf")