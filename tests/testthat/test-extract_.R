#Extract a dataset
# data <- list(austraits = austraits_lite,
#              austraits_post = austraits_lite_post)
# test_that("Function works", {
#   expect_no_error(purrr::map(.x =data,
#                              .f = ~extract_dataset(., dataset_id = "Falster_2003")))
# })

austraits <- austraits_lite
austraits_post <- austraits_lite_post

subset <- extract_dataset(austraits, dataset_id = "Falster_2003")
subset_post <- extract_dataset(austraits_post, dataset_id = "Bloomfield_2018")

#Extract a trait
trait_subset <- extract_trait(austraits, trait_names = "wood_density")
trait_subset_post <- extract_trait(austraits_post, trait_names = "leaf_mass_per_area")

test_that("extracted dataset has some structure as austraits build", {
  expect_s3_class(austraits, "austraits")
  expect_equal(length(austraits), length(subset))
  expect_equal(sort(names(austraits)), sort(names(subset)))
  
  expect_equal(length(austraits), length(trait_subset))
  expect_equal(sort(names(austraits)), sort(names(trait_subset)))
  expect_named(austraits, names(trait_subset))  
  
  expect_s3_class(austraits_post, "austraits")
  expect_s3_class(austraits_post, "austraits")
  expect_equal(length(austraits_post), length(subset_post))
  expect_equal(sort(names(austraits_post)), sort(names(subset_post)))
  
  expect_equal(length(austraits_post), length(trait_subset_post))
  expect_equal(sort(names(austraits_post)), sort(names(trait_subset_post)))
  expect_named(austraits_post, names(trait_subset_post))
})

test_that("extraction of dataset was successful", {
  expect_match("Falster_2003", unique(subset$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
  expect_match("wood_density", unique(trait_subset$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name))  
  
  expect_match("Falster_2003", unique(subset$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
  expect_match("wood_density", unique(trait_subset$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name)) 
  
  expect_match("Bloomfield_2018", unique(subset_post$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset_post$traits$dataset_id))
  expect_match("leaf_mass_per_area", unique(trait_subset_post$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset_post$traits$trait_name))  
  
  expect_match("Bloomfield_2018", unique(subset_post$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset_post$traits$dataset_id))
  expect_match("leaf_mass_per_area", unique(trait_subset_post$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset_post$traits$trait_name))  
})

