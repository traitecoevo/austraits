data(austraits)
data <- dplyr::filter(austraits$traits, dataset_id == "Falster_2003")

#Should run smoothly
expect_silent(trait_pivot_wider(data))
#expect_silent(trait_pivot_wider(data) %>% trait_pivot_longer()) This produces messages but I can't see them?! 

#Should throw error
expect_error(trait_pivot_wider(austraits), label = "The austraits object is a list. Try austraits$trait")
expect_error(trait_pivot_wider(austraits$sites), label = "Relevent column names from trait data frame not found")
expect_error(trait_pivot_wider(austraits$methods), label = "Relevent column names from trait data frame not found")

#Structural tests
expect_match(class(trait_pivot_wider(data)), "list")
expect_equal(trait_pivot_wider(data) %>% length, 5)

