#dataset_ids <- c("Falster_2003", "Gallagher_2015", "ABRS_1981", "Crous_2013", "Prior_2003")  #even more?? , "Rye_2006"


  # for (i in seq_along(1:length(dataset_ids))) {
  #   dataset_id <- dataset_ids[i]
  #   database <- extract_dataset(austraits_5.0.0_lite, dataset_id)
  #   combined_table_by_dataset <- database_create_combined_table(database)
  #   
  #   locations <- recreate_locations_dataframe(combined_table_by_dataset)
  #   contexts <- recreate_contexts_dataframe(combined_table_by_dataset)
  #   contributors <- recreate_contributors_dataframe(combined_table_by_dataset)
  #   methods <- recreate_methods_dataframe(combined_table_by_dataset)
  #   
  #   taxa <- combined_table_by_dataset %>%
  #    dplyr::select(taxon_name, taxon_rank, taxonomic_status, taxonomic_dataset, taxon_name_alternatives, genus, family, binomial, trinomial,
  #    taxon_distribution, establishment_means, scientific_name, taxon_id, taxon_id_genus, taxon_id_family, taxon_id_family, scientific_name_id) %>%
  #    dplyr::distinct() %>%
  #    dplyr::arrange(taxon_name)
  #   
  #   taxonomic_updates <- combined_table_by_dataset %>%
  #    dplyr::select(dataset_id, original_name, aligned_name, taxonomic_resolution, taxon_name, aligned_name_taxon_id, aligned_name_taxonomic_status) %>%
  #    dplyr::distinct() %>%
  #    dplyr::arrange(original_name, aligned_name, taxon_name, taxonomic_resolution)
  #   
  #   traits <- combined_table_by_dataset %>%
  #    dplyr::select(dataset_id,  taxon_name, observation_id, trait_name, value, unit, entity_type, value_type, basis_of_value, replicates, 
  #                  basis_of_record, life_stage, population_id, individual_id, repeat_measurements_id, temporal_context_id, source_id, 
  #                  location_id, entity_context_id, plot_context_id, treatment_context_id, collection_date, measurement_remarks, method_id, method_context_id, original_name)
  #   
  #   austraits_rebuilt <- list(traits, locations, contexts, methods, excluded_data, taxonomic_updates, taxa, contributors)
  #              
  #   database$contexts <- database$contexts %>%
  #     tidyr::separate_longer_delim(link_vals, delim = ", ") %>%
  #     dplyr::arrange(dataset_id, category, link_id, context_property, link_vals) %>%
  #     dplyr::distinct() %>%
  #     dplyr::group_by(dataset_id, category, link_id, context_property, value, description) %>%
  #     dplyr::mutate(link_vals = paste0(link_vals, collapse = ", ")) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct()
  #   
  #   #expect_equal(locations, database$locations)
  #   #expect_equal(contexts, database$contexts)
  #   #expect_equal(contributors, database$contributors)
  #   # need to filter original tibble to only include taxa for which there are trait measurements
  #   #expect_equal(taxonomic_updates, database$taxonomic_updates %>% filter(taxon_name %in% combined_table_by_dataset$taxon_name))
  #   #expect_equal(taxa, database$taxa)
  #   # need to filter original tibble to only include merthods for which there are trait measurements
  #   #expect_equal(methods, database$methods %>% arrange(trait_name, method_id) %>% filter(trait_name %in% combined_table_by_dataset$trait_name))
  #   #expect_equal(traits, database$traits)
  # }
