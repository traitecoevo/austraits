not_supported_austraits <- list(austraits_3.0.2_lite, austraits_4.2.0_lite) 

dataset_id = "Falster_2003"
dataset_id2 = "Cernusak_2006"
dataset_id3 = "Wright_2019"
trait_name = "leaf_area"
family = "Rubiaceae"
genus = "Eucalyptus"
taxon_name = "Banskia serrata"

test_that("Error message is triggered", {
  expect_error(austraits_5.0.0_lite %>% extract_taxa())
  expect_error(extract_taxa())
  expect_error(extract_data(at_six))
  expect_error(extract_data(at_six,
                            table = "taxonomy",
                            col = "genus", 
                            col_value = "Acacia"))
})

test_extract_error <- function(austraits){
  test_that("Compatability message is triggered", {
    expect_error(austraits %>% extract_taxa())
    expect_error(austraits %>% extract_dataset())
    expect_error(austraits %>% extract_trait())
    expect_error(austraits_3.0.2_lite %>% extract_data())
    expect_error(austraits_4.2.0_lite %>% extract_data())
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

test_that("Function runs", {
  expect_visible(austraits_5.0.0_lite %>% extract_taxa(family = family))
  expect_visible(austraits_5.0.0_lite %>% extract_taxa(genus = genus))
  expect_visible(extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id))
  expect_visible(extract_trait(austraits_5.0.0_lite, trait_names = trait_name))
}
)


test_that("extracted dataset has some structure as austraits build", {
  expect_no_error(subset <- extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id))
  expect_no_error(trait_subset <- extract_trait(austraits_5.0.0_lite, trait_names = trait_name))
  test_database_structure(subset, dataset_id = dataset_id)
  
  expect_s3_class(austraits_5.0.0_lite, "traits.build")
  expect_equal(length(subset), length(austraits_5.0.0_lite))
  expect_equal(sort(names(subset)), sort(names(austraits_5.0.0_lite)))
  
  expect_equal(length(trait_subset), length(austraits_5.0.0_lite))
  expect_equal(sort(names(trait_subset)), sort(names(austraits_5.0.0_lite)))
  expect_equal(names(trait_subset), names(austraits_5.0.0_lite))  
  
  expect_type(austraits_5.0.0_lite %>% extract_taxa(family = family), "list")
  expect_type(austraits_5.0.0_lite %>% extract_taxa(genus = genus), "list")           
  
  expect_no_error(test_genus <- austraits_5.0.0_lite %>% extract_taxa(genus = genus))
  expect_equal(test_genus$taxa$genus %>% unique(), genus)
  expect_equal(stringr::word(test_genus$taxa$taxon_name, 1)[1], genus)
  expect_equal(stringr::word(test_genus$traits$taxon_name, 1)[1], genus)
  test_database_structure(test_genus)
  
  expect_no_error(test_fam <- austraits_5.0.0_lite %>% extract_taxa(family = family))
  expect_equal(test_fam$taxa$family %>% unique(), family)
})

test_that("extracts using generalised extract function behaves as expected - extracting by dataset_id", {
  
  expect_no_error(subset_by_dataset_id <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "dataset_id", col_value = dataset_id))
  expect_equal(length(austraits_5.0.0_lite), length(subset_by_dataset_id))
  expect_equal(nrow(subset_by_dataset_id$locations), nrow(austraits_5.0.0_lite$locations %>% dplyr::filter(dataset_id == "Falster_2003")))
  expect_equal(nrow(subset_by_dataset_id$contexts), nrow(austraits_5.0.0_lite$contexts %>% dplyr::filter(dataset_id == "Falster_2003")))
  test_database_structure(subset_by_dataset_id, dataset_id = dataset_id)
  
  expect_no_error(subset_by_dataset_id2 <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "dataset_id", col_value = dataset_id2))
  expect_equal(length(austraits_5.0.0_lite), length(subset_by_dataset_id2))
  expect_equal(nrow(subset_by_dataset_id2$locations), nrow(austraits_5.0.0_lite$locations %>% dplyr::filter(dataset_id == "Cernusak_2006")))
  expect_equal(nrow(subset_by_dataset_id2$contexts), nrow(austraits_5.0.0_lite$contexts %>% dplyr::filter(dataset_id == "Cernusak_2006")))
  expect_equal(nrow(subset_by_dataset_id2$methods), nrow(austraits_5.0.0_lite$methods %>% dplyr::filter(dataset_id == "Cernusak_2006")))
  expect_equal(nrow(subset_by_dataset_id2$contributors), nrow(austraits_5.0.0_lite$contributors %>% dplyr::filter(dataset_id == "Cernusak_2006")))
  expect_equal(names(subset_by_dataset_id2), names(austraits_5.0.0_lite))
  
  expect_no_error(subset_by_dataset_id3 <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "dataset_id", col_value = dataset_id3))
  expect_equal(length(austraits_5.0.0_lite), length(subset_by_dataset_id3))
  expect_equal(nrow(subset_by_dataset_id3$locations), nrow(austraits_5.0.0_lite$locations %>% dplyr::filter(dataset_id == "Wright_2019")))
  expect_equal(nrow(subset_by_dataset_id3$contexts), nrow(austraits_5.0.0_lite$contexts %>% dplyr::filter(dataset_id == "Wright_2019")))
  expect_equal(nrow(subset_by_dataset_id3$methods), nrow(austraits_5.0.0_lite$methods %>% dplyr::filter(dataset_id == "Wright_2019")))
  expect_equal(nrow(subset_by_dataset_id3$contributors), nrow(austraits_5.0.0_lite$contributors %>% dplyr::filter(dataset_id == "Wright_2019")))
  expect_equal(names(subset_by_dataset_id3), names(austraits_5.0.0_lite))
  test_database_structure(subset_by_dataset_id3, dataset_id = dataset_id3)
  })


test_that("that you can link two calls of `extract_data` together", {
  expect_no_error(subset_by_dataset_id2 <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "dataset_id", col_value = dataset_id2))
  expect_no_error(extract_data(database = subset_by_dataset_id2, table = "traits", col = "trait_name", col_value = "leaf_mass_per_area"))
  test_database_structure(subset_by_dataset_id2, dataset_id = dataset_id2)
  
  expect_no_error(subset_by_dataset_id3 <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "trait_name", col_value = "leaf_mass_per_area"))
  expect_no_error(extract_data(database = subset_by_dataset_id3, table = "contexts", col = "context_property", col_value = "age"))
  expect_no_error(subset_by_dataset_id_and_context <- extract_data(database = subset_by_dataset_id3, table = "contexts", col = "context_property", col_value = "age"))
  expect_gt(nrow(subset_by_dataset_id3$contexts), nrow(subset_by_dataset_id_and_context$contexts))
  })
  
test_that("extracts using generalised extract function behaves as expected - extracting by `life_stage", {
  
  life_stage_test <- "sapling"
  
  expect_no_error(subset_by_age_class <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "life_stage", col_value = life_stage_test))
  expect_no_error(datasets_in_subset <- subset_by_age_class$traits %>% dplyr::distinct(dataset_id))
  
  expect_lt(nrow(subset_by_age_class$locations), nrow(austraits_5.0.0_lite$locations))
  expect_lt(nrow(subset_by_age_class$contexts), nrow(austraits_5.0.0_lite$contexts))
  expect_contains(datasets_in_subset$dataset_id, subset_by_age_class$contexts$dataset_id)
  expect_contains(datasets_in_subset$dataset_id, subset_by_age_class$locations$dataset_id)
  expect_equal(names(subset_by_age_class), names(austraits_5.0.0_lite))
}) 

test_that("extracts using generalised extract function behaves as expected - extracting by `location_property", {
  
  location_property_test <- "temperature"
  
  expect_no_error(subset_by_location_property <- extract_data(database = austraits_5.0.0_lite, table = "locations", col = "location_property", col_value = location_property_test))
  expect_no_error(datasets_in_subset <- subset_by_location_property$locations %>% dplyr::distinct(dataset_id))
  
  expect_lt(nrow(subset_by_location_property$traits), nrow(austraits_5.0.0_lite$traits))
  expect_lt(nrow(subset_by_location_property$locations), nrow(austraits_5.0.0_lite$locations))
  expect_lt(nrow(subset_by_location_property$contexts), nrow(austraits_5.0.0_lite$contexts))
  expect_contains(datasets_in_subset$dataset_id, subset_by_location_property$locations$dataset_id)
  # for locations, all datasets in subset, since it is the starting point
  expect_contains(subset_by_location_property$locations$dataset_id, datasets_in_subset$dataset_id)
  expect_contains(datasets_in_subset$dataset_id, subset_by_location_property$traits$dataset_id)
  # similarly for traits, all datasets from locations must be in traits
  expect_contains(subset_by_location_property$traits$dataset_id, datasets_in_subset$dataset_id)
  # however contexts will only be a subset of dataset_ids, so only 1 direction is true
  expect_contains(datasets_in_subset$dataset_id, subset_by_location_property$contexts$dataset_id)
  expect_equal(names(subset_by_location_property), names(austraits_5.0.0_lite))
})  


test_that("extracts for which there are no matches work`", {
  context_property_test <- "platypus"
  expect_message(extract_data(database = austraits_5.0.0_lite,  table = "contexts", col = "context_property", col_value = context_property_test))
  expect_equal(nrow(extract_data(database = austraits_5.0.0_lite,  table = "contexts", col = "context_property", col_value = context_property_test)$traits), 0)
  
  location_property_test <- "green flowers"
  expect_message(extract_data(database = austraits_5.0.0_lite,  table = "locations", col = "location_property", col_value = location_property_test))
  expect_equal(nrow(extract_data(database = austraits_5.0.0_lite,  table = "locations", col = "location_property", col_value = location_property_test)$traits), 0)
})
  
test_that("extracts using generalised extract function behaves as expected - extracting by `context_property`", {
  
  context_property_test <- "fire"
  
  expect_no_error(subset_by_context_property <- extract_data(database = austraits_5.0.0_lite,  table = "contexts", col = "context_property", col_value = context_property_test))

  expect_no_error(datasets_in_subset <- subset_by_context_property$contexts %>% dplyr::distinct(dataset_id))
  expect_no_error(subset_using_filter <- austraits_5.0.0_lite$contexts %>% dplyr::filter(stringr::str_detect(context_property, context_property_test)))
  
  expect_lt(nrow(subset_by_context_property$traits), nrow(austraits_5.0.0_lite$traits))
  expect_lt(nrow(subset_by_context_property$locations), nrow(austraits_5.0.0_lite$locations))
  expect_lt(nrow(subset_by_context_property$contexts), nrow(austraits_5.0.0_lite$contexts))
  # for locations, all datasets in subset, since it is the starting point
  expect_contains(datasets_in_subset$dataset_id, subset_by_context_property$contexts$dataset_id)
  expect_contains(subset_by_context_property$contexts$dataset_id, datasets_in_subset$dataset_id)
  # similarly for traits, all datasets from locations must be in traits
  expect_contains(datasets_in_subset$dataset_id, subset_by_context_property$traits$dataset_id)
  expect_contains(subset_by_context_property$traits$dataset_id, datasets_in_subset$dataset_id)
  # however contexts will only be a subset of dataset_ids, so only 1 direction is true
  expect_contains(datasets_in_subset$dataset_id, subset_by_context_property$locations$dataset_id)
  expect_contains(datasets_in_subset$dataset_id, unique(subset_by_context_property$locations$dataset_id))
  expect_equal(names(subset_by_context_property), names(austraits_5.0.0_lite))
  
  # this should be true, because the proper extract function also retains other context property data linked to the same observation
  expect_gte(nrow(subset_by_context_property$contexts), nrow(subset_using_filter))
  # however both methods should be including the same dataset_id's 
  expect_equal(
    subset_using_filter %>% dplyr::distinct(dataset_id) %>% dplyr::arrange(dataset_id), 
    subset_by_context_property$contexts %>% dplyr::distinct(dataset_id) %>% dplyr::arrange(dataset_id)
    )
})  

test_that("Extraction of dataset was successful", {
  expect_no_error(subset <- extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id))
  test_database_structure(subset, dataset_id = dataset_id)
  expect_no_error(trait_subset <- extract_trait(austraits_5.0.0_lite, trait_names = trait_name))
  expect_match(dataset_id, unique(subset$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))
  # this isn't an exact match, because the matches use partial string matches and for this example, both `leaf_area` and `leaf_area_ratio` being matched
  expect_match(trait_name, unique(trait_subset$traits$trait_name))
  expect_equal(1, dplyr::n_distinct(trait_subset$traits$trait_name))
  # Works when taxon names also supplied
  taxa <- c("Acacia floribunda", "Acacia myrtifolia", "Acacia suaveolens")
  expect_no_error(subset2 <- extract_trait(subset, trait_names = "leaf_area", taxon_name = taxa))
  expect_equal(subset2$traits$taxon_name |> unique() |> sort(), taxa)
  expect_equal(subset2$traits$trait_name |> unique(), "leaf_area")
})

test_that("Expect error if taxon_name column missing", {
  expect_no_error(traits_without_taxon_name <- extract_dataset(austraits_5.0.0_lite, dataset_id = dataset_id))
  traits_without_taxon_name$traits <- traits_without_taxon_name$traits %>% dplyr::select(-taxon_name)
  expect_error((traits_without_taxon_name %>% extract_trait("leaf_area"))$traits)
})

test_that("Extraction of dataset was successful using `extract_data`", {
  expect_no_error(subset <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "dataset_id", col_value = dataset_id))
  expect_no_error(trait_subset <- extract_data(database = austraits_5.0.0_lite, table = "traits", col = "trait_name", col_value = trait_name))
  expect_match(dataset_id, unique(subset$traits$dataset_id))
  expect_equal(dplyr::n_distinct(subset$traits$dataset_id), 1)
  expect_contains(unique(trait_subset$traits$trait_name), trait_name)
  expect_equal(dplyr::n_distinct(trait_subset$traits$trait_name), 1)
  expect_equal(trait_subset$traits %>% dplyr::distinct(dataset_id) %>% nrow(), 8) #something weird here, keeps isolating between 8 & 5
})

test_that("Extract function works when just traits table is read in", {
  expect_silent(extract_data(database = austraits_5.0.0_lite$traits, col = "dataset_id", col_value = dataset_id))
  expect_equal(length(extract_data(database = austraits_5.0.0_lite$traits, col = "dataset_id", col_value = dataset_id)), 26)
  expect_silent(extract_dataset(database = austraits_5.0.0_lite$traits, dataset_id = dataset_id))  
  expect_equal(length(extract_dataset(database = austraits_5.0.0_lite$traits, dataset_id = dataset_id)), 26)
  expect_silent(extract_taxa(database = austraits_5.0.0_lite$traits, genus = "Banksia"))  
  expect_equal(length(extract_taxa(database = austraits_5.0.0_lite$traits, genus = "Banksia")), 26)
  expect_silent(extract_trait(database = austraits_5.0.0_lite$traits, trait_name = "photosyn"))  
  expect_equal(length(extract_trait(database = austraits_5.0.0_lite$traits, trait_name = "photosyn")), 26)
  expect_silent(join_then_extract <- (austraits_5.0.0_lite %>% join_location_coordinates())$traits)
  expect_silent(extract_data(database = join_then_extract, col = "dataset_id", col_value = dataset_id))
  expect_silent(extract_data(database = join_then_extract, col = "longitude (deg)", col_value = "145"))
})

