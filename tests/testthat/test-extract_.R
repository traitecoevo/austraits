library(purrr)

austraits <- list(austraits_lite,
                  austraits_lite_post)

dataset_id = "Wright_2019"
trait_name = "leaf_area"

test_extract_str <- function(austraits, dataset_id, trait_name) {
  test_that("extracted dataset has some structure as austraits build", {
    subset <- extract_dataset(austraits, dataset_id = dataset_id)
    trait_subset <- extract_trait(austraits, trait_names = trait_name)
    
    expect_s3_class(austraits, "austraits")
    expect_equal(length(austraits), length(subset))
    expect_equal(sort(names(austraits)), sort(names(subset)))
    
    expect_equal(length(austraits), length(trait_subset))
    expect_equal(sort(names(austraits)), sort(names(trait_subset)))
    expect_named(austraits, names(trait_subset))  
  })
}

map(austraits, 
    ~ test_extract_str(.x, dataset_id, trait_name))

test_extract_output <- function(austraits, dataset_id, trait_name){
  subset <- extract_dataset(austraits, dataset_id = dataset_id)
  trait_subset <- extract_trait(austraits, trait_names = trait_name)
  
  test_that("extraction of dataset was successful", {
    expect_match(dataset_id, unique(subset$traits$dataset_id))
    expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
    expect_match(trait_name, unique(trait_subset$traits$trait_name))
    expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name))  
    
    expect_match(dataset_id, unique(subset$traits$dataset_id))
    expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
    expect_match(trait_name, unique(trait_subset$traits$trait_name))
    expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name)) 
  })
}

map(austraits, 
    ~ test_extract_output(.x, dataset_id, trait_name))

