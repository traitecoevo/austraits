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

many_location_columns <- (database %>% join_location_properties(format = "many_columns"))$traits

test_that("`join_locations` is working with format = many_columns", {
  expect_length(many_location_columns, 32)
  expect_equal(ncol(many_location_columns %>% dplyr::select(dplyr::contains("location_property"))), 5)
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
  # failing - something is compacting from with JSON - merging in wrong description
  expect_true(stringr::str_detect(contexts_single_column_json_yes_desc$temporal_context_properties[1], "description\":\"December"))
  expect_equal(nrow(filter(contexts_default_no_desc_subset_vars, !is.na(contexts_default_no_desc_subset_vars$method_context_properties))), 0)
  expect_equal(nrow(filter(contexts_default_no_desc, !is.na(contexts_default_no_desc$method_context_properties))), 196)
  expect_equal(intersect(names(contexts_default_no_desc), "entity_context_properties"), "entity_context_properties")
  expect_equal(intersect(names(contexts_single_column_json_yes_desc), "entity_context_properties"), "entity_context_properties")
  expect_equal(intersect(names(contexts_many_columns_no_desc), "entity_context_properties"), character(0))
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
