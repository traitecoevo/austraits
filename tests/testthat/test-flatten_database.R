# tests for combined table

# Falster_2003 has many location properties
dataset_id <- "Falster_2003"
database <- extract_dataset(austraits_5.0.0_lite, dataset_id)
combined_table <- flatten_database(database)

# Crous_2013 has many context properties, from 4 of 5 possible categories
dataset_id_2 <- "Crous_2013"
database_2 <- extract_dataset(austraits_5.0.0_lite, dataset_id_2)

#expected_output <- readr::read_csv("tests/testthat/Falster_2003_combined_format.csv", show_col_types = FALSE)
expected_output <- readr::read_csv("Falster_2003_combined_format.csv", show_col_types = FALSE)

test_that("`flatten_database` is working with format = single_column_pretty", {
    expect_equal(combined_table$location_properties, expected_output$location_properties)
    expect_equal(combined_table$data_contributors, expected_output$data_contributors)
    expect_length(combined_table, 66)
    expect_true(stringr::str_detect(combined_table$location_properties[1], "=="))
    expect_true(stringr::str_detect(combined_table$data_contributors[1], "<"))
})

database_3 <- extract_dataset(austraits_5.0.0_lite, "Bloomfield_2018")
combined_table_3a <- flatten_database(database_3, format = "single_column_pretty")
combined_table_3b <- flatten_database(database_3, format = "many_columns")

test_that("`flatten_database` defaults to `single_column_pretty` for contributors", {
  expect_equal(combined_table_3a$data_contributors, combined_table_3b$data_contributors)
})

test_that("`flatten_database` is working with format = single_column_json", {
  expect_no_error(combined_table_json <- flatten_database(database_2, format = "single_column_json"))
  expect_contains(names(combined_table_json), "location_name")
  expect_length(combined_table_json, 66)
  expect_true(stringr::str_detect(combined_table_json$location_properties[1], "^\\[\\{"))
  expect_true(stringr::str_detect(combined_table_json$data_contributors[1], "^\\[\\{"))
})

test_that("`flatten_database` is working with format = many_columns", {
  expect_no_error(combined_table_many <- flatten_database(database_2, format = "many_columns"))
  expect_contains(names(combined_table_many), "location_name")
  expect_contains(names(combined_table_many), "temporal_context: sampling season")
})

# test that join_location_coordinates works as intended

database_no_coord <- austraits_5.0.0_lite %>% extract_dataset("Kooyman_2011")

test_that("`join_location_coordinate` works as intended", {
  
expect_contains(names(combined_table), c("latitude (deg)", "longitude (deg)")) # proper columns present in full combined table
expect_contains(names(join_location_coordinates(database)$traits), c("latitude (deg)", "longitude (deg)"))  # proper columns present when just `join_location_coordinate` used
expect_contains(names(join_location_coordinates(database_no_coord)$traits), c("latitude (deg)", "longitude (deg)"))  # proper columns present if dataset without location coordinates is used
                
})


# test different packing formats for locations

location_vars <- (database$locations %>% dplyr::distinct(location_property))$location_property

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
  expect_equal(nrow(locations_single_column_pretty %>% dplyr::distinct(location_properties) %>% dplyr::filter(!is.na(location_properties))), 2)
  expect_equal(nrow(locations_single_column_json %>% dplyr::distinct(location_properties) %>% dplyr::filter(!is.na(location_properties))), 2)
  expect_true(stringr::str_detect(locations_single_column_pretty$location_properties[1], "volcanic dyke;;"))
  expect_true(stringr::str_detect(locations_single_column_json$location_properties[1], "volcanic dyke\"\\}"))
})

# test different vars options for contributors

contributors_no_ORCID <- (database %>% join_contributors(vars = c("affiliation", "additional_role")))$traits
contributors_with_ORCID <- (database %>% join_contributors(vars = "all"))$traits
contributors_default <- (database %>% join_contributors())$traits
contributors_json <- (database %>% join_contributors(format = "single_column_json"))$traits

test_that("`join_contributors` is working with vars options", {
  expect_equal(length(contributors_no_ORCID), length(contributors_default))
  expect_equal(contributors_with_ORCID, contributors_default)
  expect_true(stringr::str_detect(contributors_with_ORCID$data_contributors[3], "ORCID"))
  expect_false(stringr::str_detect(contributors_no_ORCID$data_contributors[3], "ORCID"))
  expect_equal(stringr::str_extract(contributors_json$data_contributors[[1]], "^[:punct:]+"), "[{\"")
})

# test different packing formats & `include_description` for contexts

context_vars <- (database_2$contexts %>% dplyr::distinct(context_property))$context_property

contexts_default_no_desc <- (database_2 %>% join_context_properties(include_description = FALSE))$traits
contexts_default_yes_desc <- (database_2 %>% join_context_properties(include_description = TRUE))$traits
contexts_single_column_pretty_no_desc <- (database_2 %>% join_context_properties(include_description = FALSE, format = "single_column_pretty"))$traits
contexts_single_column_json_yes_desc <- (database_2 %>% join_context_properties(include_description = TRUE, format = "single_column_json"))$traits
contexts_many_columns_no_desc <- (database_2 %>% join_context_properties(include_description = FALSE, format = "many_columns"))$traits
contexts_default_no_desc_subset_vars <- (database_2 %>% join_context_properties(include_description = FALSE, vars = c("sampling season", "temperature treatment", "CO2 treatment")))$traits

test_that("join_context_properties arguments are working", {
  expect_equal(contexts_default_no_desc, contexts_single_column_pretty_no_desc)
  expect_true(stringr::str_detect(contexts_default_yes_desc$temporal_context_properties[1], "<<"))
  expect_false(stringr::str_detect(contexts_default_no_desc$temporal_context_properties[1], "<<"))
  expect_true(stringr::str_detect(contexts_default_yes_desc$temporal_context_properties[1], "<<December"))
  expect_true(stringr::str_detect(contexts_single_column_json_yes_desc$temporal_context_properties[1], "description\":\"December"))
  expect_equal(nrow(dplyr::filter(contexts_default_no_desc_subset_vars, !is.na(contexts_default_no_desc_subset_vars$method_context_properties))), 0)
  expect_equal(nrow(dplyr::filter(contexts_default_no_desc, !is.na(contexts_default_no_desc$method_context_properties))), 196)
  expect_equal(intersect(names(contexts_default_no_desc), "entity_context_properties"), "entity_context_properties")
  expect_equal(intersect(names(contexts_single_column_json_yes_desc), "entity_context_properties"), "entity_context_properties")
  expect_equal(intersect(names(contexts_many_columns_no_desc), "entity_context_properties"), character(0))
  })

# test vars options in other join_functions - other than contexts, contributors which are tested above

location_vars <- (database$locations %>% dplyr::distinct(location_property))$location_property
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
