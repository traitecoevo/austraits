# This top bit of information is just for testing until everything is working
# First focusing on each column unpacking to recreate various tables, then will amend functions to loop through all datasets

# starting by ensuring the function works for a single dataset
# afterwards, needs to work

#database <- austraits::extract_dataset(austraits, c("Gallagher_2015", "Falster_2003", "Prior_2003", "Rye_2006", "Crous_2013")) 
#combined_table <- database_create_combined_table(database)


# XX- Daniel, this simple loop works and I think this should be wrapped around each of the functions below.
# There is no point creating functions that will fail if a dataframe is 
recreate_traits.build_locations <- function(combined_table) {
  
  combined_split <- split(combined_table, combined_table$dataset_id)
  recreated_locations <- tibble()  

  for (i in seq_along(1:length(combined_split))) {
    
    recreated_locations_i <- recreate_locations_dataframe(combined_split[[i]])
    recreated_locations <- recreated_locations %>% bind_rows(recreated_locations_i)
    
  }
  
  recreated_locations

}
  

#' Unpack condensed location properties
#' 
#' @description
#' The function `unpack_location_properties` takes the packed locations properties column in the combined table output 
#' of a traits.build database and splits the information into separate columns, one for each location property.
#' 
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
unpack_location_properties <- function(combined_table) {
  
  combined_table <- combined_table %>% arrange(dataset_id)
  
  combined_split <- split(combined_table, combined_table$dataset_id)
  recreated_locations_all <- tibble()  
  
  for (i in seq_along(1:length(combined_split))) {
    
    ## once working will replace all `combined_table_by_dataset` with `combined_split[[i]]`
    
    combined_table_by_dataset <- combined_split[[i]]
  
    if (!all(is.na(combined_table_by_dataset$location_properties))) {
      
      packed_column_unpacked <- combined_table_by_dataset %>% 
        dplyr::select(dplyr::all_of(c("dataset_id", "observation_id", "location_id", "location_properties"))) %>%
        distinct() 
      
      packed_column_unpacked <- packed_column_unpacked %>%
        tidyr::separate_longer_delim(location_properties, delim = ";; ") %>%
        tidyr::separate_wider_delim(location_properties, delim = "==", names_sep = "_") %>%
        dplyr::filter(!is.na(location_properties_1)) %>%
        dplyr::mutate(location_properties_1 = paste0("location_property: ", location_properties_1)) %>%
        tidyr::pivot_wider(names_from = location_properties_1, values_from = location_properties_2)
      
      unpacked_locations_table <- 
        combined_table_by_dataset %>% 
        dplyr::left_join(packed_column_unpacked, 
                  by = c("dataset_id", "observation_id", "location_id")) %>% 
        dplyr::select(-location_properties)
      
    } else {
      
      unpacked_locations_table <- 
        combined_table_by_dataset %>% 
        dplyr::select(-location_properties)
      
    }
    
    recreated_locations_all <- recreated_locations_all %>% bind_rows(unpacked_locations_table)
  
  }
  
  recreated_locations_all
  
}

#' Recreate long-format locations tibble
#' 
#' @description
#' The function `recreate_locations_dataframe` takes the packed locations properties column in the combined table output 
#' of a traits.build database and reformats it into a long-format table, identical to what existed in the initial relational database.
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
recreate_locations_dataframe <- function(combined_table) {
  
  if (!all(is.na(combined_table$location_properties))) {
    
    unpacked_locations_table <- unpack_location_properties(combined_table)
  
  } else {
    
    unpacked_locations_table <- combined_table %>%
      dplyr::select(dataset_id, observation_id, location_id, location_name, `latitude (deg)`, `longitude (deg)`)
    
  }
  
    unpacked_locations_table <- unpacked_locations_table %>% arrange(dataset_id)
    unpacked_locations_table_split <- split(unpacked_locations_table, unpacked_locations_table$dataset_id)
    long_locations_all <- tibble()  
    
    for (i in seq_along(1:length(unpacked_locations_table_split))) {
    
      ## once working will replace all `combined_table_by_dataset` with `unpacked_locations_table_split[[i]]`
    
      combined_table_by_dataset <- unpacked_locations_table_split[[i]]
    
      long_output <- combined_table_by_dataset %>%
        dplyr::select(dplyr::any_of(c("dataset_id", "observation_id", "location_id")), dplyr::contains("location_property")) %>%
        #this works but also removes location_id which it shouldn't be - I want to select columns with location property that are not all NA's...
        # otherwise I can use the filters below
        #dplyr::select(where(~ any(!is.na(.)))) %>%
        dplyr::left_join(combined_table %>%
                           dplyr::select(dataset_id, observation_id, location_id, location_name, `latitude (deg)`, `longitude (deg)`) %>%
                           dplyr::distinct(),
                         by = c("dataset_id", "observation_id", "location_id")) %>% 
        dplyr::select(-observation_id) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(location_id)) 
      
      long_locations_output <- long_output %>%
        dplyr::select(dataset_id, location_id, location_name, everything()) %>%
        tidyr::pivot_longer(cols = 4:ncol(long_output)) %>%
        dplyr::rename(location_property = name) %>%
        dplyr::mutate(location_property = stringr::str_replace(location_property, "location_property: ", "")) %>%
        # Reorder using same code as process.R in traits.build, to ensure description, latitude, longitude, if present, as the first 3 location properties listed
        dplyr::mutate(
          j = dplyr::case_when(
            location_property == "description" ~ 1,
            location_property == "latitude (deg)" ~ 2,
            location_property == "longitude (deg)" ~ 3,
            TRUE ~ 4)
        ) %>%
        dplyr::arrange(location_id, location_name, j, location_property) %>%
        dplyr::select(-dplyr::all_of(c("j"))) %>%
        dplyr::filter(!is.na(value)) 
      
      long_locations_all <- long_locations_all %>% bind_rows(long_locations_output)
      
  }
  
  long_locations_all  
  
} 

# for now create a single table, but later rework this to cycle through all datasets using `combined_split[[n]]`
# at that point replace `combined_table_by_dataset` with `combined_split[[n]]` in function

#combined_split <- split(combined_table, combined_table$dataset_id)
#n <- 1
#combined_table_by_dataset <- combined_split[[n]]

#' Unpack condensed context properties
#' 
#' @description
#' The function `unpack_context_properties` takes the 5 packed context properties columns in the combined table output 
#' of a traits.build database and splits the information into separate columns, one for each context property.
#' 
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
unpack_context_properties <- function(combined_table_by_dataset) {
  
  context_variables <- tibble(
    context_properties = c("plot_context_properties", "treatment_context_properties", 
                           "entity_context_properties", "temporal_context_properties", "method_context_properties"),
    context_property_id = c("plot_context_id", "treatment_context_id", 
                            "entity_context_id", "temporal_context_id", "method_context_id"),
    context_dataframe = c("unpacked_plot_contexts_table", "unpacked_treatment_contexts_table", 
                          "unpacked_entity_contexts_table", "unpacked_temporal_contexts_table", "unpacked_method_contexts_table")
  )
  
  for (i in seq_along(1:5)) { 
    
    if (!all(is.na((combined_table_by_dataset[[context_variables[[i,1]]]])))) {
      
      packed_column_unpacked <- combined_table_by_dataset %>% 
        dplyr::select("observation_id", dplyr::contains(context_variables[[i,2]]), dplyr::contains(context_variables[[i,1]])) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(.[[2]]))
      
      packed_column_unpacked <- packed_column_unpacked %>%
        tidyr::separate_longer_delim(context_variables[[i,1]], delim = ";; ") %>%
        tidyr::separate_wider_delim(context_variables[[i,1]], delim = "==", names_sep = "_") %>%
        dplyr::mutate(context_property_1 = paste0(context_variables[[i,1]] %>% stringr::str_replace("properties","property"),
                                                              ": ", .[[3]])) %>%
        dplyr::select(-3) %>%
        tidyr::pivot_wider(names_from = dplyr::contains("_1"), values_from = dplyr::contains("_2"))
      
      unpacked_contexts_table <- 
        combined_table_by_dataset %>% 
        dplyr::left_join(packed_column_unpacked, 
                  by = c("observation_id", context_variables[[i,2]]))# %>%
        #dplyr::select(-dplyr::contains(context_variables[[i,1]]))
      
    } else {
      
      unpacked_contexts_table <- 
        combined_table_by_dataset #%>% 
        #dplyr::select(-dplyr::contains(context_variables[[i,1]]))
      
    }
    
  new_table_name <- context_variables[[i,3]]
  assign(new_table_name, unpacked_contexts_table)
  
  }
  
  all_unpacked_contexts <- unpacked_plot_contexts_table %>% 
    dplyr::left_join(unpacked_treatment_contexts_table) %>%
    dplyr::left_join(unpacked_entity_contexts_table) %>%
    dplyr::left_join(unpacked_temporal_contexts_table) %>%
    dplyr::left_join(unpacked_method_contexts_table) %>%
    dplyr::select(-dplyr::contains("properties"))
  
  all_unpacked_contexts
}


#' Recreate long-format contexts tibble
#' 
#' @description
#' The function `recreate_contexts_dataframe` takes the 5 packed context properties columns in the combined table output 
#' of a traits.build database and reformats it into a long-format table, identical to what existed in the initial relational database.
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
recreate_contexts_dataframe <- function(combined_table_by_dataset) {

  unpacked_contexts_table <- unpack_context_properties(combined_table_by_dataset)
  
  context_variables_2 <- tibble(
    contexts = c("plot_context", "treatment_context", 
                           "entity_context", "temporal_context", "method_context"),
    context_property_id = c("plot_context_id", "treatment_context_id", 
                            "entity_context_id", "temporal_context_id", "method_context_id"),
    context_property = c("plot_context_property", "treatment_context_property", 
                           "entity_context_property", "temporal_context_property", "method_context_property"),
    contexts_long = c("plot_contexts_long", "treatment_contexts_long", 
                         "entity_contexts_long", "temporal_contexts_long", "method_contexts_long"),
    context_properties = c("plot_context_properties", "treatment_context_properties", 
                         "entity_context_properties", "temporal_context_properties", "method_context_properties")
  )
  
  for (i in seq_along(1:5)) {
  
    if (!all(is.na((combined_table_by_dataset[[context_variables_2[[i,5]]]])))) {
      
      long_output <- unpacked_contexts_table %>%
        dplyr::select("dataset_id", "observation_id", dplyr::contains(context_variables_2[[i,2]]), dplyr::contains(context_variables_2[[i,3]])) %>%
        dplyr::left_join(combined_table_by_dataset %>%
                           dplyr::select(dataset_id, observation_id, dplyr::contains(context_variables_2[[i,2]])) %>%
                           dplyr::distinct(),
                         by = c("dataset_id", "observation_id", context_variables_2[[i,2]])) %>% 
        dplyr::select(-observation_id) %>%
        dplyr::distinct() %>%
        filter(!is.na(.[[2]]))

      long_output <- long_output %>%
        dplyr::select(dataset_id, dplyr::contains(context_variables_2[[i,2]]), everything()) %>%
        tidyr::pivot_longer(cols = 3:ncol(long_output)) %>%
        tidyr::separate_wider_delim(value, delim = " <<", names_sep = "_", too_few = "align_start") %>%
        dplyr::rename(link_vals = 2) %>%
        dplyr::rename(dplyr::any_of(c(context_property = "name", value = "value_1", description = "value_2")))

      if (!any(stringr::str_detect(names(long_output), "description"))) {
        long_output <- long_output %>%
          dplyr::mutate(description = NA)
      }
      
      long_output <- long_output %>%
        dplyr::mutate(
          context_property = stringr::str_replace(context_property, paste0(context_variables_2[[i,3]],": "), ""),
          description = ifelse(is.na(description), NA, stringr::str_replace(description, ">>", "")),
          link_id = context_variables_2[[i,2]],
          category = context_variables_2[[i,1]]
          ) %>%
        dplyr::select(dataset_id, context_property, category, value, description, link_id, link_vals) %>%
        dplyr::arrange(link_id, link_vals) %>%
        dplyr::group_by(dataset_id, context_property, category, value, description, link_id) %>%
          dplyr::mutate(link_vals = paste0(link_vals, collapse = ", ")) %>%
        dplyr::ungroup() %>% 
        dplyr::distinct()
      
    } else {
      
      long_output <- tibble(
        dataset_id = character(),
        context_property = character(),
        category = character(),
        value = character(),
        description = character(),
        link_id = character(),
        link_vals = character()
      )
      
    }
  
      new_table_name <- context_variables_2[[i,4]]
      assign(new_table_name, long_output)
      
  }
  
  long_contexts_list <- list(plot_contexts_long, treatment_contexts_long, 
                      entity_contexts_long, temporal_contexts_long, method_contexts_long)

  long_contexts_output <- purrr::map_dfr(long_contexts_list, ~ .x)
  
  long_contexts_output <- 
    long_contexts_output %>% 
    dplyr::filter(!is.na(value)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(category, context_property, link_vals)
  
}


#' Unpack condensed data collectors
#' 
#' @description
#' The function `unpack_data_collectors` takes the packed data collectors column in the combined table output 
#' of a traits.build database and splits the information into separate columns, one for each data collector.
#' 
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
unpack_data_collectors <- function(combined_table_by_dataset) {
  
  packed_column_unpacked <- combined_table_by_dataset %>% 
    dplyr::select(dplyr::all_of(c("dataset_id", "observation_id", "data_collectors"))) %>%
    distinct() 
  
  packed_column_unpacked <- packed_column_unpacked %>%
    tidyr::separate_longer_delim(data_collectors, delim = ";; ") %>% 
    tidyr::separate_wider_delim(data_collectors, delim = " <<", names_sep = "_") %>%
    dplyr::filter(!is.na(data_collectors_1)) %>%
    dplyr::mutate(data_collectors_1 = paste0("data_collector: ", data_collectors_1)) %>%
    tidyr::pivot_wider(names_from = data_collectors_1, values_from = data_collectors_2)
  
  
  unpacked_collectors_table <- 
    combined_table_by_dataset %>% 
    dplyr::left_join(packed_column_unpacked, 
                     by = c("dataset_id", "observation_id")) %>% 
    dplyr::select(-data_collectors)
  
  remove(packed_column_unpacked)
  
  unpacked_collectors_table
  
}

#' Recreate long-format contributors tibble
#' 
#' @description
#' The function `recreate_contributors_dataframe` takes the packed `data_collectors` column in the combined table output 
#' of a traits.build database and reformats it into a long-format table, identical to what existed in the initial relational database.
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
recreate_contributors_dataframe <- function(combined_table_by_dataset) {
  
  unpacked_contributors_table <- unpack_data_collectors(combined_table_by_dataset)
  
  long_output <- unpacked_contributors_table %>%
    dplyr::select("dataset_id", "observation_id", dplyr::contains("data_collector")) %>% 
    dplyr::select(-observation_id) %>%
    dplyr::distinct()
  
  long_output <- long_output %>%
    dplyr::select(dataset_id, everything()) %>%
    tidyr::pivot_longer(cols = 2:ncol(long_output)) %>%
    dplyr::rename(data_collector = name) %>%
    dplyr::mutate(data_collector = stringr::str_replace(data_collector, "data_collector: ", "")) %>%
    tidyr::separate_wider_delim(data_collector, delim = ", ", names_sep = "_") %>%
    dplyr::rename(last_name = data_collector_1,  given_name = data_collector_2) %>%
    tidyr::separate_wider_delim(value, delim = " \\ ", names_sep = "_", too_few = "align_start")
  
  long_contributors_output <- long_output %>%
    tidyr::pivot_longer(cols = 4:ncol(long_output)) %>%
    dplyr::select(-name) %>%
    tidyr::separate_wider_delim(value, delim = "==", names_sep = "_") %>%
    dplyr::filter(!is.na(value_1)) %>%
    dplyr::mutate(
      value_2 = ifelse(is.na(value_2), NA, stringr::str_replace(value_2, ">>", ""))
    ) %>%
    tidyr::pivot_wider(names_from = value_1, values_from = value_2) %>%
    dplyr::select(dplyr::any_of(c("dataset_id", "last_name", "given_name", "ORCID", "affiliation", "additional_role")))
  
  if (!any(stringr::str_detect(names(long_contributors_output), "additional_role"))) {
    long_contributors_output <- long_contributors_output %>%
      mutate(additional_role = NA_character_)
  }
  
  long_contributors_output
  
} 

#' Recreate long-format methods tibble
#' 
#' @description
#' The function `recreate_methods_dataframe` reformats the methods information included in 
#' a traits.build combined output table into the original relational table format. 
#' This mostly just requires selecting specific columns from the combined table, 
#' but the data collectors column needs to be recreated.
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
recreate_methods_dataframe <- function(combined_table_by_dataset) {
  
  recreated_contributors_dataframe <- recreate_contributors_dataframe(combined_table_by_dataset)
  
  data_collectors <- recreated_contributors_dataframe %>%
    dplyr::mutate(
      data_collectors = paste0(given_name, " ", last_name),
      data_collectors = ifelse(is.na(additional_role), data_collectors, paste0(data_collectors, " (", additional_role, ")")),
      data_collectors = paste0(data_collectors, collapse =", ")
    ) %>%
    dplyr::select(dataset_id, data_collectors) %>%
    dplyr::distinct()
  
  
  methods <- combined_table_by_dataset %>%
    dplyr::select(-data_collectors) %>%
    dplyr::left_join(data_collectors) %>%
    dplyr::select(dataset_id, trait_name, methods, method_id, description, sampling_strategy, source_primary_key, source_primary_citation, source_secondary_key,
                  source_secondary_citation, source_original_dataset_key, source_original_dataset_citation, data_collectors, assistants, dataset_curators) %>%
    dplyr::distinct() %>%
    dplyr::arrange(trait_name, method_id)
  
}

