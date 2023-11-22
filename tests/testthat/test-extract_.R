library(purrr)
library(stringr)

austraits <- list(austraits_3.0.2_lite,
                  austraits_4.2.0_lite, 
                  austraits_5.0.0_lite)

dataset_id = "Falster_2003"
trait_name = "leaf_area"
family = "Rubiaceae"
genus = "Eucalyptus"

test_extract_error <- function(austraits){
  test_that("Error triggered", {
    expect_error(austraits %>% extract_taxa())
    expect_error(austraits %>% extract_dataset())
    expect_error(austraits %>% extract_trait())
  })
}

walk(austraits, 
    ~ test_extract_error(.x))

test_extract_runs <- function(austraits, family, genus, dataset_id, trait_name){
  test_that("Function runs", {
    expect_visible(austraits %>% extract_taxa(family = family))
    expect_visible(austraits %>% extract_taxa(genus = genus))
    expect_visible(extract_dataset(austraits, dataset_id = dataset_id))
    expect_visible(extract_trait(austraits, trait_names = trait_name))
  })
}

pmap(list(austraits = austraits, 
          family = family, 
          genus = genus, 
          dataset_id = dataset_id, 
          trait_name = trait_name),
     test_extract_runs)

test_extract_str <- function(austraits, family, genus, dataset_id, trait_name){
  test_that("extracted dataset has some structure as austraits build", {
    subset <- extract_dataset(austraits, dataset_id = dataset_id)
    trait_subset <- extract_trait(austraits, trait_names = trait_name)
    
    expect_s3_class(austraits, "austraits")
    expect_equal(length(austraits), length(subset))
    expect_equal(sort(names(austraits)), sort(names(subset)))
    
    expect_equal(length(austraits), length(trait_subset))
    expect_equal(sort(names(austraits)), sort(names(trait_subset)))
    expect_named(austraits, names(trait_subset))  
    
    expect_type(austraits %>% extract_taxa(family = family), "list")
    expect_type(austraits %>% extract_taxa(genus = genus), "list")           
    
    test_genus <- austraits %>% extract_taxa(genus = genus)
    expect_equal(test_genus$taxa$genus %>% unique(), genus)
    expect_equal(word(test_genus$taxa$taxon_name, 1)[1], genus)
    expect_equal(word(test_genus$traits$taxon_name, 1)[1], genus)
    
    test_fam <- austraits %>% extract_taxa(family = family)
    expect_equal(test_fam$taxa$family %>% unique(), family)
  })
}

pmap(list(austraits = austraits, 
          family = family, 
          genus = genus, 
          dataset_id = dataset_id, 
          trait_name = trait_name),
     test_extract_str)

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

walk(austraits, 
    ~ test_extract_output(.x, dataset_id, trait_name))
