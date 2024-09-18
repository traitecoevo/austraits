# tests for combined table

# Falster_2003 has many location properties
dataset_id <- "Falster_2003"
database <- extract_dataset(austraits_5.0.0_lite, dataset_id)
combined_table <- database_create_combined_table(database)

# Crous_2013 has many context properties, from 4 of 5 possible categories
dataset_id_2 <- "Crous_2013"
database_2 <- extract_dataset(austraits_5.0.0_lite, dataset_id_2)
combined_table_2 <- database_create_combined_table(database_2)

expected_output <- readr::read_csv("Falster_2003_combined_format.csv", show_col_types = FALSE)

test_that("`database_create_combined_table` is working with format = single_column_pretty", {
    expect_equal(combined_table$location_properties, expected_output$location_properties)
    #expect_equal(combined_table$data_contributors, expected_output$data_contributors)
    expect_length(combined_table, 66)
    expect_true(stringr::str_detect(combined_table$location_properties[1], "=="))
    expect_true(stringr::str_detect(combined_table$data_contributors[1], "<"))
})

# test different packing formats for locations

location_vars <- (database$locations %>% distinct(location_property))$location_property

many_location_columns_default_vars <- (database %>% join_location_properties(format = "many_columns"))$traits
many_location_columns_all_vars <- (database %>% join_location_properties(format = "many_columns", vars = "all"))$traits
locations_single_column_pretty <- (database %>% join_location_properties(format = "single_column_pretty"))$traits
locations_single_column_json <- (database %>% join_location_properties(format = "single_column_json"))$traits
locations_default_subset_vars <- (database %>% join_location_properties(format = "many_columns", vars = location_vars[4:6]))$traits

test_that("`join_locations` is working with different formats, vars", {
  expect_length(many_location_columns_default_vars, 32)
  expect_equal(ncol(many_location_columns_default_vars %>% dplyr::select(dplyr::contains("location_property"))), 5)
  expect_equal(ncol(locations_default_subset_vars %>% dplyr::select(dplyr::contains("location_property"))), 3)
  expect_equal(ncol(many_location_columns_default_vars), ncol(many_location_columns_all_vars))
  expect_equal(names(many_location_columns_default_vars), names(many_location_columns_all_vars))
  expect_equal(intersect(names(many_location_columns_all_vars), c("latitude (deg")), character(0))
  expect_equal(ncol(locations_single_column_pretty %>% dplyr::select(dplyr::contains("location_prop"))), 1)
  expect_equal(ncol(locations_single_column_json %>% dplyr::select(dplyr::contains("location_prop"))), 1)
  expect_equal(nrow(locations_single_column_pretty %>% distinct(location_properties) %>% filter(!is.na(location_properties))), 2)
  expect_equal(nrow(locations_single_column_json %>% distinct(location_properties) %>% filter(!is.na(location_properties))), 2)
  expect_true(stringr::str_detect(locations_single_column_pretty$location_properties[1], "volcanic dyke;;"))
  expect_true(stringr::str_detect(locations_single_column_json$location_properties[1], "volcanic dyke\"\\}"))
})

# test different vars options for contributors

contributors_no_ORCID <- (database %>% join_contributors(vars = c("affiliation", "additional_role")))$traits
contributors_with_ORCID <- (database %>% join_contributors(vars = "all"))$traits
contributors_default <- (database %>% join_contributors())$traits

test_that("`join_contributors` is working with vars options", {
  expect_equal(length(contributors_no_ORCID), length(contributors_default))
  expect_equal(contributors_with_ORCID, contributors_default)
  expect_true(stringr::str_detect(contributors_with_ORCID$data_contributors[3], "ORCID"))
  expect_false(stringr::str_detect(contributors_no_ORCID$data_contributors[3], "ORCID"))
})

# test different packing formats & `include_description` for contexts

context_vars <- (database_2$contexts %>% distinct(context_property))$context_property

contexts_default_no_desc <- (database_2 %>% join_context_properties(include_description = FALSE))$traits
contexts_default_yes_desc <- (database_2 %>% join_context_properties(include_description = TRUE))$traits
contexts_single_column_pretty_no_desc <- (database_2 %>% join_context_properties(include_description = FALSE, format = "single_column_pretty"))$traits
contexts_single_column_json_yes_desc <- (database_2 %>% join_context_properties(include_description = TRUE, format = "single_column_json"))$traits
contexts_many_columns_no_desc <- (database_2 %>% join_context_properties(include_description = FALSE, format = "many_columns"))$traits
contexts_default_no_desc_subset_vars <- (database_2 %>% join_context_properties(include_description = FALSE, vars = context_vars[1:3]))$traits

test_that("join_context_properties arguments are working", {
  expect_equal(contexts_default_no_desc, contexts_single_column_pretty_no_desc)
  expect_true(stringr::str_detect(contexts_default_yes_desc$temporal_context_properties[1], "<<"))
  expect_false(stringr::str_detect(contexts_default_no_desc$temporal_context_properties[1], "<<"))
  expect_true(stringr::str_detect(contexts_default_yes_desc$temporal_context_properties[1], "<<December"))
  expect_true(stringr::str_detect(contexts_single_column_json_yes_desc$temporal_context_properties[1], "description\":\"December"))
  expect_equal(nrow(filter(contexts_default_no_desc_subset_vars, !is.na(contexts_default_no_desc_subset_vars$method_context_properties))), 0)
  expect_equal(nrow(filter(contexts_default_no_desc, !is.na(contexts_default_no_desc$method_context_properties))), 196)
  expect_equal(intersect(names(contexts_default_no_desc), "entity_context_properties"), "entity_context_properties")
  expect_equal(intersect(names(contexts_single_column_json_yes_desc), "entity_context_properties"), "entity_context_properties")
  expect_equal(intersect(names(contexts_many_columns_no_desc), "entity_context_properties"), character(0))
  })

# test vars options in other join_functions - other than contexts, contributors which are tested above

location_vars <- (database$locations %>% distinct(location_property))$location_property
method_vars <- c("dataset_id", "method_id", "sampling_strategy", "assistants")
method_vars2 <- c("assistants")
method_vars3 <- names(database$methods)
taxonomic_updates_vars <- c("original_name")
taxonomic_updates_vars2 <- c("aligned_name_taxonomic_status", "aligned_name")
taxonomic_updates_vars3 <- names(database$taxonomic_updates)
taxa_vars <- c("taxon_id")
taxa_vars2 <- c("taxon_id", "binomial", "trinomial")
taxa_vars3 <- names(database$taxa)

test_that("join_ functions all a diversity of vars strings - 1 value, many values, all values", {
  expect_silent((database %>% join_location_properties(vars = location_vars))$traits)
  expect_silent((database %>% join_location_properties(vars = location_vars[1]))$traits)
  expect_silent((database %>% join_location_properties(vars = "all"))$traits)
  expect_silent((database %>% join_location_properties)$traits)
  expect_silent((database %>% join_methods(vars = method_vars))$traits)
  expect_silent((database %>% join_methods(vars = method_vars2))$traits)
  expect_silent((database %>% join_methods(vars = method_vars3))$traits)
  expect_silent((database %>% join_methods(vars = method_vars3[5]))$traits)
  expect_silent((database %>% join_methods(vars = "all"))$traits)
  expect_silent((database %>% join_methods)$traits)
  expect_silent((database %>% join_taxonomic_updates(vars = taxonomic_updates_vars))$traits)
  expect_silent((database %>% join_taxonomic_updates(vars = taxonomic_updates_vars2))$traits)
  expect_silent((database %>% join_taxonomic_updates(vars = taxonomic_updates_vars3))$traits)
  expect_silent((database %>% join_taxonomic_updates(vars = taxonomic_updates_vars3[5]))$traits)
  expect_silent((database %>% join_taxonomic_updates(vars = "all"))$traits)
  expect_silent((database %>% join_taxonomic_updates)$traits)
  expect_silent((database %>% join_taxa(vars = taxa_vars))$traits)
  expect_silent((database %>% join_taxa(vars = taxa_vars2))$traits)
  expect_silent((database %>% join_taxa(vars = taxa_vars3))$traits)
  expect_silent((database %>% join_taxa(vars = taxa_vars3[5]))$traits)
  expect_silent((database %>% join_taxa(vars = "all"))$traits)
  expect_silent((database %>% join_taxa)$traits)
})
  
  
test_that("join_ functions given expected output", {
  expect_equal(intersect(names((database %>% join_location_properties(vars = location_vars, format = "many_columns"))$traits), "location_property: leaf area index"), "location_property: leaf area index")
  expect_equal(intersect(names((database %>% join_location_properties(vars = location_vars[1], format = "many_columns"))$traits), "location_property: leaf area index"), character(0))
  expect_equal(ncol((database %>% join_location_properties(format = "many_columns"))$traits), 32)
  expect_equal(ncol((database %>% join_location_properties(format = "many_columns"))$traits), 
               ncol((database %>% join_location_properties(vars = location_vars, format = "many_columns"))$traits))
  expect_equal(ncol((database %>% join_location_properties(vars = location_vars[1], format = "many_columns"))$traits), 28)
  expect_equal(ncol((database %>% join_location_properties(vars = "all"))$traits), 28)
  expect_equal(ncol((database %>% join_location_properties)$traits), 28)
  expect_equal(names((database %>% join_methods(vars = method_vars))$traits), 
               union(names(database$traits), method_vars))
  expect_equal(ncol((database %>% join_methods(vars = method_vars2))$traits), 26 + length(method_vars2))
  expect_equal(ncol((database %>% join_methods(vars = method_vars3[5]))$traits), 27)
  expect_equal(names((database %>% join_methods(vars = "all"))$traits),
               union(names(database$traits), names(database$methods)))
  expect_equal(ncol((database %>% join_taxonomic_updates(vars = taxonomic_updates_vars3[6]))$traits), 27)
  expect_equal(ncol((database %>% join_taxonomic_updates(vars = taxonomic_updates_vars3[2]))$traits), 26)
  expect_equal(names((database %>% join_taxonomic_updates(vars = taxonomic_updates_vars2))$traits),
               union(names(database$traits), taxonomic_updates_vars2))
  expect_equal(names((database %>% join_taxonomic_updates(vars = "all"))$traits),
               union(names(database$traits), names(database$taxonomic_updates)))  
  expect_equal((database %>% join_taxonomic_updates(vars = "all"))$traits,
               (database %>% join_taxonomic_updates(vars = taxonomic_updates_vars3))$traits)
  expect_equal((database %>% join_taxa(vars = "all"))$traits,
               (database %>% join_taxa(vars = taxa_vars3))$traits)
  expect_equal(ncol((database %>% join_taxa(vars = taxa_vars3[5]))$traits), 27)
  expect_equal(ncol((database %>% join_taxa(vars = taxa_vars3[1]))$traits), 26)
  expect_equal(ncol((database %>% join_taxa(vars = taxa_vars2))$traits), 29)
})

# location unpacking tests

unpacked_locations <- unpack_location_properties(combined_table)

 test_that("`unpack_location_properties` is working", {
     expect_gt(ncol(unpacked_locations), 66)
     expect_gt(ncol(unpacked_locations %>% select(contains("location_property:"))), 0)
 })
 
recreated_locations_tibble <- recreate_locations_dataframe(combined_table) %>%
  arrange(location_id, location_property, value)

starting_locations_table <- database$locations %>%
  arrange(location_id, location_property, value)

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
