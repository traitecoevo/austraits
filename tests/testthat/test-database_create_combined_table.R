# tests for combined table

dataset_id <- "Falster_2003"
database <- extract_dataset(austraits_5.0.0_lite, dataset_id)
combined_table <- database_create_combined_table(database)

expected_output <- readr::read_csv("Falster_2003_combined_format.csv", show_col_types = FALSE)

test_that("`database_create_combined_table` is working", {
    #expect_equal(combined_table$location_properties, expected_output$location_properties)
    #expect_equal(combined_table$data_contributors, expected_output$data_contributors)
    expect_length(combined_table, 66)
    expect_true(stringr::str_detect(combined_table$location_properties[1], "=="))
    expect_true(stringr::str_detect(combined_table$data_contributors[1], "<"))
})

# location unpacking tests

#unpacked_locations <- unpack_location_properties(combined_table)

# test_that("`unpack_location_properties` is working", {
#     expect_gt(ncol(unpacked_locations), 66)
#     expect_gt(ncol(unpacked_locations %>% select(contains("location_property:"))), 0)
# })
# 
# recreated_locations_tibble <- recreate_locations_dataframe(combined_table) %>%
#   arrange(location_id, location_property, value)
# 
# starting_locations_table <- database$locations %>%
#   arrange(location_id, location_property, value)
# 
# test_that("`recreate_locations_dataframe` is working", {
#   expect_equal(recreated_locations_tibble, starting_locations_table)
#   expect_length(recreated_locations_tibble, 5)
# })
# 
# # context unpacking tests
# 
# # select a tricky context table - 4 types on contexts, some with, some without descriptions
# dataset_id <- "Crous_2013"
# database <- extract_dataset(austraits_5.0.0_lite, dataset_id)
# combined_table <- database_create_combined_table(database)
# 
# unpacked_contexts <- unpack_context_properties(combined_table)
# 
# test_that("`unpack_context_properties` is working", {
#   expect_gt(ncol(unpacked_contexts), 66)
#   expect_length(unpacked_contexts %>% select(contains("method_context_property:")), 1)
#   expect_length(unpacked_contexts %>% select(contains("treatment_context_property:")), 2)
#   expect_length(unpacked_contexts %>% select(contains("plot_context_property:")), 1)
#   expect_length(unpacked_contexts %>% select(contains("temporal_context_property:")), 1)
#   expect_length(unpacked_contexts %>% select(contains("entity_context_property:")), 0)
#   expect_equal(nrow(unpacked_contexts), 391)
# })
# 
# # recreate context tibble from combined table
# recreated_contexts_tibble <- recreate_contexts_dataframe(combined_table) %>%
#   arrange(context_property, category, link_id, link_vals, value, description)
# 
# # manipulations required for actual contexts table, because the link_vals haven't been sorted before being collapsed
# starting_contexts <- database$contexts %>%
#   tidyr::separate_longer_delim(link_vals, delim = ", ") %>%
#   dplyr::arrange(context_property, category, link_id, link_vals) %>%
#   dplyr::group_by(context_property, category, link_id, value, description) %>%
#   dplyr::mutate(link_vals = paste0(link_vals, collapse = ", ")) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(context_property, category, link_id, link_vals, value, description) %>%
#   dplyr::distinct()
# 
# test_that("`recreate_locations_dataframe` is working", {
#   expect_equal(recreated_contexts_tibble, starting_contexts)
#   expect_length(recreated_locations_tibble, 7)
# })
