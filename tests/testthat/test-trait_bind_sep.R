#Pull in data
data(austraits)

#Extract a dataset
dataset_id <- c("Falster_2005_2")
subset <- extract_dataset(austraits, dataset_id = dataset_id)
bounded <- bind_trait_values(subset$traits)
seperated <-separate_trait_values(data = bounded, austraits$definitions)

test_that("binding/seperating was successful", {
  expect_true(grep("--", bounded$value) %>% any())
  expect_false(grep("--", seperated$value) %>% any())
})

test_that("structure of dataframes is what we expect", {
  expect_equal(nrow(subset$traits), nrow(seperated))
  expect_equal(ncol(subset$traits), ncol(seperated))
  expect_equal(colnames(subset$traits), colnames(seperated))
  expect_equal(str(subset$traits), str(seperated))
})

test_that("Errors with incorrect argument inputs", {
  expect_error(bind_trait_values(subset), label = "Input is a list, not trait dataframe")
  expect_error(bind_trait_values(austraits), label = "Input is a list, not trait dataframe")
  expect_error(separate_trait_values(austraits), label = "Input is a list, not bounded dataframe")
})