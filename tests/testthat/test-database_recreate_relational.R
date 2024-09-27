dataset_id <- "Falster_2003"
database <- extract_dataset(austraits_5.0.0_lite, dataset_id)
combined_table <- database_create_combined_table(database)

# Crous_2013 has many context properties, from 4 of 5 possible categories
dataset_id_2 <- "Crous_2013"
database_2 <- extract_dataset(austraits_5.0.0_lite, dataset_id_2)
combined_table_2 <- database_create_combined_table(database_2)

# location unpacking tests

unpacked_locations <- unpack_location_properties(combined_table)

test_that("`unpack_location_properties` is working", {
  expect_gt(ncol(unpacked_locations), 66)
  expect_gt(ncol(unpacked_locations %>% dplyr::select(contains("location_property:"))), 0)
})

recreated_locations_tibble <- recreate_locations_dataframe(combined_table) %>%
  dplyr::arrange(location_id, location_property, value)

starting_locations_table <- database$locations %>%
  dplyr::arrange(location_id, location_property, value)

test_that("`recreate_locations_dataframe` is working", {
  expect_equal(recreated_locations_tibble, starting_locations_table)
  expect_length(recreated_locations_tibble, 5)
})

# context unpacking tests

# Use Crous_2013 for context property tests - 4 types on contexts, some with, some without descriptions

unpacked_contexts <- unpack_context_properties(combined_table_2)

test_that("`unpack_context_properties` is working", {
  expect_equal(ncol(unpacked_contexts), 65)
  expect_length(unpacked_contexts %>% select(contains("method_context_property:")), 1)
  expect_length(unpacked_contexts %>% select(contains("treatment_context_property:")), 2)
  expect_length(unpacked_contexts %>% select(contains("plot_context_property:")), 1)
  expect_length(unpacked_contexts %>% select(contains("temporal_context_property:")), 1)
  expect_length(unpacked_contexts %>% select(contains("entity_context_property:")), 0)
  expect_equal(nrow(unpacked_contexts), 391)
})

# recreate context tibble from combined table
recreated_contexts_tibble <- recreate_contexts_dataframe(combined_table_2) %>%
  arrange(category, link_id, link_vals, value, context_property, description)

# manipulations required for actual contexts table, because the link_vals haven't been sorted before being collapsed
starting_contexts <- database_2$contexts %>%
  tidyr::separate_longer_delim(link_vals, delim = ", ") %>%
  dplyr::arrange(context_property, category, link_id, link_vals) %>%
  dplyr::group_by(context_property, category, link_id, value, description) %>%
  dplyr::mutate(link_vals = paste0(link_vals, collapse = ", ")) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(category, link_id, link_vals, value, context_property, description) %>%
  dplyr::distinct()

test_that("`recreate_locations_dataframe` is working", {
  expect_equal(recreated_contexts_tibble, starting_contexts)
  expect_length(recreated_contexts_tibble, 7)
})


test_that("Various datasets can be successful combined into a single table, with the relational tables then recreated", {
  
  dataset_ids <- c("Falster_2003", "ABRS_1981", "Crous_2013", "Prior_2003", "Bloomfield_2018") #  "Gallagher_2015"
  
  for (k in seq_along(1:length(dataset_ids))) {
    dataset_id <- dataset_ids[k]
    database <- extract_dataset(austraits_5.0.0_lite, dataset_id)
    combined_table_by_dataset <- database_create_combined_table(database)

    locations <- recreate_locations_dataframe(combined_table_by_dataset)
    contexts <- recreate_contexts_dataframe(combined_table_by_dataset)
    contributors <- recreate_contributors_dataframe(combined_table_by_dataset)
    methods <- recreate_methods_dataframe(combined_table_by_dataset)

    taxa <- combined_table_by_dataset %>%
     dplyr::select(taxon_name, taxon_rank, taxonomic_status, taxonomic_dataset, taxon_name_alternatives, genus, family, binomial, trinomial,
     taxon_distribution, establishment_means, scientific_name, taxon_id, taxon_id_genus, taxon_id_family, taxon_id_family, scientific_name_id) %>%
     dplyr::distinct() %>%
     dplyr::arrange(taxon_name)

    taxonomic_updates <- combined_table_by_dataset %>%
     dplyr::select(dataset_id, original_name, aligned_name, taxonomic_resolution, taxon_name, aligned_name_taxon_id, aligned_name_taxonomic_status) %>%
     dplyr::distinct() %>%
     dplyr::arrange(original_name, aligned_name, taxon_name, taxonomic_resolution)

    traits <- combined_table_by_dataset %>%
     dplyr::select(dataset_id,  taxon_name, observation_id, trait_name, value, unit, entity_type, value_type, basis_of_value, replicates,
                   basis_of_record, life_stage, population_id, individual_id, repeat_measurements_id, temporal_context_id, source_id,
                   location_id, entity_context_id, plot_context_id, treatment_context_id, collection_date, measurement_remarks, method_id, method_context_id, original_name)

    austraits_rebuilt <- list(traits, locations, contexts, methods, taxonomic_updates, taxa, contributors)

    database$contexts <- database$contexts %>%
      tidyr::separate_longer_delim(link_vals, delim = ", ") %>%
      dplyr::arrange(dataset_id, category, link_id, context_property, link_vals) %>%
      dplyr::distinct() %>%
      dplyr::group_by(dataset_id, category, link_id, context_property, value, description) %>%
      dplyr::mutate(link_vals = paste0(link_vals, collapse = ", ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    expect_equal(locations, database$locations)
    expect_equal(contexts, database$contexts)
    expect_equal(contributors, database$contributors)
    #need to filter original tibble to only include taxa for which there are trait measurements
    expect_equal(taxonomic_updates, database$taxonomic_updates %>% filter(taxon_name %in% combined_table_by_dataset$taxon_name))
    expect_equal(taxa, database$taxa)
    #need to filter original tibble to only include merthods for which there are trait measurements
    expect_equal(methods, database$methods %>% arrange(trait_name, method_id) %>% filter(trait_name %in% combined_table_by_dataset$trait_name))
    expect_equal(traits, database$traits)
  }
  
})
