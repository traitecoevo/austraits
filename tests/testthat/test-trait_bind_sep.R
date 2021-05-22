#Pull in data
data(austraits)

#Extract a dataset
dataset_id <- c("Falster_2005_2")
subset <- extract_dataset(austraits, dataset_id = dataset_id)
bounded <- bind_trait_values(subset$traits)
separate_trait_values(data = bounded, austraits$definitions)

test_that("binding/seperating was successful", {
  expect_true(grep("--", bounded$value) %>% any())
})
