library(austraits.R)
data <- austraits$traits %>% 
  filter(dataset_id == "Falster_2003")

#Should run smoothly
expect_silent(traits_spread <- spread_trait_data(data))

#Should throw error
expect_error(spread_trait_data(austraits), label = "The austraits object is a list. Try austraits$trait")
expect_error(spread_trait_data(austraits$sites), label = "Relevent column names from trait data frame not found")
expect_error(spread_trait_data(austraits$methods), label = "Relevent column names from trait data frame not found")
