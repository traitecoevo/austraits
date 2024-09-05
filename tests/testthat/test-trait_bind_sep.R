#Extract a dataset
dataset_id <- c("ABRS_1981")
subset <- extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id)
bounded <- bind_trait_values(subset$traits)
seperated <-separate_trait_values(data = bounded, austraits_5.0.0_lite$definitions)

test_that("binding/seperating was successful", {
  expect_true(grep("--", bounded$value) %>% any())
  expect_false(grep("--", seperated$value) %>% any())
})

test_that("structure of dataframes is what we expect", {
  expect_equal(nrow(subset$traits), nrow(seperated))
  expect_equal(ncol(subset$traits), ncol(seperated))
  expect_equal(colnames(subset$traits), colnames(seperated))
  # Check datasets have the same structure. 
  #This works for all cols except levels in value type, so we'll remove that for the test
  expect_equal(subset$traits %>% select(-value_type) %>% arrange(dataset_id, observation_id, trait_name, value), seperated  %>% select(-value_type)%>% arrange(dataset_id, observation_id, trait_name, value))
})

test_that("Errors with incorrect argument inputs", {
  expect_error(bind_trait_values(subset), label = "Input is a list, not trait dataframe")
  expect_error(bind_trait_values(austraits_5.0.0_lite), label = "Input is a list, not trait dataframe")
  expect_error(separate_trait_values(austraits_5.0.0_lite), label = "Input is a list, not bounded dataframe")
})
