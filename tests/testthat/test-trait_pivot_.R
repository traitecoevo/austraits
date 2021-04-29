data <- austraits$traits %>% 
  dplyr::filter(dataset_id == "Falster_2003")

#Should run smoothly
expect_silent(traits_spread <- trait_pivot_wider(data))

#Should throw error
expect_error(trait_pivot_wider(austraits), label = "The austraits object is a list. Try austraits$trait")
expect_error(trait_pivot_wider(austraits$sites), label = "Relevent column names from trait data frame not found")
expect_error(trait_pivot_wider(austraits$methods), label = "Relevent column names from trait data frame not found")
