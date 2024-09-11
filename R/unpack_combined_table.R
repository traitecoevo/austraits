# This top bit of information is just for testing until everything is working
# First focusing on each column unpacking to recreate various tables, then will amend functions to loop through all datasets

# starting by ensuring the function works for a single dataset
# afterwards, needs to work
#combined_split <- split(combined_table, combined_table$dataset_id)
#n <- 1
#dataset_id <- "Crous_2013"
#Crous_2013 <- austraits::extract_dataset(austraits, "Crous_2013")
#combined_table_by_dataset <- austraits::extract_dataset(austraits, dataset_id) %>% database_create_combined_table()


#combined_table_by_dataset <- combined_split[[n]]

#' Unpack condensed location properties
#' 
#' @description
#' The function `unpack_location_properties` takes the packed locations properties column in the combined table output 
#' of a traits.build database and splits the information into separate columns, one for each location property.
#' 
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return
#' @export
#'
#' @examples
unpack_location_properties <- function(combined_table_by_dataset) {
  
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
      left_join(packed_column_unpacked, 
                by = c("dataset_id", "observation_id", "location_id")) %>% 
      select(-location_properties)
    
  } else {
    
    unpacked_locations_table <- 
      combined_table_by_dataset %>% 
      select(-location_properties)
    
  }
  
  unpacked_locations_table
  
}

#' Recreate long-format locations tibble
#' 
#' @description
#' The function `recreate_locations_dataframe` takes the packed locations properties column in the combined table output 
#' of a traits.build database and reformats it into a long-format table, identical to what existed in the initial relational database.
#'
#' @param combined_table_by_dataset The combined table format of a traits.build built database.
#'
#' @return
#' @export
#'
#' @examples
recreate_locations_dataframe <- function(combined_table_by_dataset) {
  
  if (!all(is.na(combined_table_by_dataset$location_properties))) {
    
    unpacked_locations_table <- unpack_location_properties(combined_table_by_dataset)
    
    long_output <- unpacked_locations_table %>%
      dplyr::select("dataset_id", "observation_id", "location_id", dplyr::contains("location_property")) %>%
      dplyr::left_join(combined_table_by_dataset %>%
                         dplyr::select(dataset_id, observation_id, location_id, location_name, `latitude (deg)`, `longitude (deg)`) %>%
                         dplyr::distinct(),
                       by = c("dataset_id", "observation_id", "location_id")) %>% 
      dplyr::select(-observation_id) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(location_id))
    
    long_locations_output <- long_output %>%
      dplyr::select(dataset_id, location_id, location_name, everything()) %>%
      tidyr::pivot_longer(cols = 4:ncol(long_output)) %>%
      rename(location_property = name) %>%
      mutate(location_property = stringr::str_replace(location_property, "location_property: ", ""))
    
  }
  
  long_locations_output
  
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
#' @return
#' @export
#'
#' @examples
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
        distinct() %>%
        filter(!is.na(.[[2]]))
      
      packed_column_unpacked <- packed_column_unpacked %>%
        tidyr::separate_longer_delim(context_variables[[i,1]], delim = ";; ") %>%
        tidyr::separate_wider_delim(context_variables[[i,1]], delim = "==", names_sep = "_") %>%
        dplyr::mutate(context_property_1 = paste0(context_variables[[i,1]] %>% stringr::str_replace("properties","property"),
                                                              ": ", .[[3]])) %>%
        dplyr::select(-3) %>%
        tidyr::pivot_wider(names_from = dplyr::contains("_1"), values_from = dplyr::contains("_2"))
      
      unpacked_contexts_table <- 
        combined_table_by_dataset %>% 
        left_join(packed_column_unpacked, 
                  by = c("observation_id", context_variables[[i,2]])) %>%
        select(-dplyr::contains(context_variables[[i,1]]))
      
    } else {
      
      unpacked_contexts_table <- 
        combined_table_by_dataset %>% 
        select(-dplyr::contains(context_variables[[i,1]]))
      
    }
    
  new_table_name <- context_variables[[i,3]]
  assign(new_table_name, unpacked_contexts_table)
  
  }
  
  all_unpacked_contexts <- unpacked_plot_contexts_table %>% 
    left_join(unpacked_treatment_contexts_table) %>%
    left_join(unpacked_entity_contexts_table) %>%
    left_join(unpacked_temporal_contexts_table) %>%
    left_join(unpacked_method_contexts_table)
  
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
#' @return
#' @export
#'
#' @examples
recreate_contexts_dataframe <- function(combined_table_by_dataset) {
  
  unpacked_contexts_table <- unpack_context_properties(combined_table_by_dataset)
  
  context_variables_2 <- tibble(
    context_properties = c("plot_context", "treatment_context", 
                           "entity_context", "temporal_context", "method_context"),
    context_property_id = c("plot_context_id", "treatment_context_id", 
                            "entity_context_id", "temporal_context_id", "method_context_id"),
    context_property = c("plot_context_property", "treatment_context_property", 
                           "entity_context_property", "temporal_context_property", "method_context_property"),
    contexts_long = c("plot_contexts_long", "treatment_contexts_long", 
                         "entity_contexts_long", "temporal_contexts_long", "method_contexts_long")
  )
  
  for (i in seq_along(1:5)) {
  
    if (!all(is.na((combined_table_by_dataset[[context_variables[[i,1]]]])))) {
      
      long_output <- unpacked_contexts_table %>%
        dplyr::select("dataset_id", "observation_id", dplyr::contains(context_variables_2[[i,2]]), dplyr::contains(context_variables_2[[i,3]])) %>%
        dplyr::left_join(combined_table_by_dataset %>%
                           dplyr::select(dataset_id, observation_id, dplyr::contains(context_variables_2[[i,2]])) %>%
                           dplyr::distinct(),
                         by = c("dataset_id", "observation_id", context_variables[[i,2]])) %>% 
        dplyr::select(-observation_id) %>%
        dplyr::distinct() %>%
        filter(!is.na(.[[2]]))

      long_output <- long_output %>%
        dplyr::select(dataset_id, dplyr::contains(context_variables_2[[i,2]]), everything()) %>%
        tidyr::pivot_longer(cols = 3:ncol(long_output)) %>%
        tidyr::separate_wider_delim(value, delim = " <<", names_sep = "_") %>%
        rename(link_vals = 2) %>%
        rename(any_of(c(context_property = "name", value = "value_1", description = "value_2")))

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
      
    }
  
      new_table_name <- context_variables_2[[i,4]]
      assign(new_table_name, long_output)
      
  }
  
  long_contexts_list <- list(plot_contexts_long, treatment_contexts_long, 
                      entity_contexts_long, temporal_contexts_long, method_contexts_long)

  long_contexts_output <- purrr::map_dfr(long_contexts_list, ~ .x)
  
  long_contexts_output <- long_contexts_output %>% dplyr::distinct()
  
}
