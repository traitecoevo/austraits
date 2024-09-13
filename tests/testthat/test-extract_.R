not_supported_austraits <- list(austraits_3.0.2_lite, austraits_4.2.0_lite) 

dataset_id = "Falster_2003"
trait_name = "leaf_area"
family = "Rubiaceae"
genus = "Eucalyptus"

test_that("Error message is triggered", {
  expect_error(austraits_5.0.0_lite |> extract_taxa())
})

test_extract_error <- function(austraits){
  test_that("Compatability message is triggered", {
    expect_error(austraits %>% extract_taxa())
    expect_error(austraits %>% extract_dataset())
    expect_error(austraits %>% extract_trait())
  })
}

purrr::walk(not_supported_austraits, 
    ~ test_extract_error(.x))

test_that("Function runs", {
  expect_visible(austraits_5.0.0_lite %>% extract_taxa(family = family))
  expect_visible(austraits_5.0.0_lite %>% extract_taxa(genus = genus))
  expect_visible(extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id))
  expect_visible(extract_trait(austraits_5.0.0_lite, trait_names = trait_name))
}
)

test_that("extracted dataset has some structure as austraits build", {
  subset <- extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id)
  trait_subset <- extract_trait(austraits_5.0.0_lite, trait_names = trait_name)
  
  expect_s3_class(austraits_5.0.0_lite, "austraits")
  expect_equal(length(austraits_5.0.0_lite), length(subset))
  expect_equal(sort(names(austraits_5.0.0_lite)), sort(names(subset)))
  
  expect_equal(length(austraits_5.0.0_lite), length(trait_subset))
  expect_equal(sort(names(austraits_5.0.0_lite)), sort(names(trait_subset)))
  expect_named(austraits_5.0.0_lite, names(trait_subset))  
  
  expect_type(austraits_5.0.0_lite %>% extract_taxa(family = family), "list")
  expect_type(austraits_5.0.0_lite %>% extract_taxa(genus = genus), "list")           
  
  test_genus <- austraits_5.0.0_lite %>% extract_taxa(genus = genus)
  expect_equal(test_genus$taxa$genus %>% unique(), genus)
  expect_equal(stringr::word(test_genus$taxa$taxon_name, 1)[1], genus)
  expect_equal(stringr::word(test_genus$traits$taxon_name, 1)[1], genus)
  
  test_fam <- austraits_5.0.0_lite %>% extract_taxa(family = family)
  expect_equal(test_fam$taxa$family %>% unique(), family)
})


test_that("Extraction of dataset was successful", {
  subset <- extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id)
  trait_subset <- extract_trait(austraits_5.0.0_lite, trait_names = trait_name)
  
  expect_match(dataset_id, unique(subset$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
  expect_match(trait_name, unique(trait_subset$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name))  
  
  expect_match(dataset_id, unique(subset$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
  expect_match(trait_name, unique(trait_subset$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name)) 
})


