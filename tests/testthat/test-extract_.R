#Extract a dataset
austraits <- load_austraits_lite()

dataset_id <- c("Falster_2003")
subset <- extract_dataset(austraits, dataset_id = dataset_id)

#Extract a trait
trait <- c("wood_density")
trait_subset <- extract_trait(austraits, trait_names = trait)

test_that("extracted dataset has some structure as austraits build", {
  expect_equal(length(austraits), length(subset))
  expect_equal(class(austraits), class(subset))
  expect_equal(names(austraits), names(subset))
  expect_named(austraits, names(subset))
  expect_equal(length(austraits), length(trait_subset))
  expect_equal(class(austraits), class(trait_subset))
  expect_equal(names(austraits), names(trait_subset))
  expect_named(austraits, names(trait_subset))  
})

test_that("extraction of dataset was successful", {
  expect_match(dataset_id, unique(subset$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
  expect_match(trait, unique(trait_subset$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name))  
})

