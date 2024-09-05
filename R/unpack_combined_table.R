#combined_split <- split(combined_table, combined_table$dataset_id)

unpack_location_properties <- function(combined_table_by_dataset) {
  
  if (!all(is.na(combined_table_by_dataset$location_properties))) {
    
    packed_column_unpacked <- combined_table_by_dataset %>% 
      dplyr::select(dplyr::all_of(c("dataset_id", "observation_id", "location_id", "location_properties"))) %>%
      distinct() 
    
    packed_column_unpacked <- packed_column_unpacked %>%
      tidyr::separate_longer_delim(location_properties, delim = "; ") %>%
      tidyr::separate_wider_delim(location_properties, delim = "=", names_sep = "_") %>%
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
    
    long_output <- long_output %>%
      dplyr::select(dataset_id, location_id, location_name, everything()) %>%
      tidyr::pivot_longer(cols = 4:ncol(long_output)) %>%
      rename(location_property = name) %>%
      mutate(location_property = stringr::str_replace(location_property, "location_property: ", ""))
    
  }
  
  long_output
  
} 

# for now create a single table, but later rework this to cycle through all datasets using `combined_split[[n]]`
# at that point replace `combined_table_by_dataset` with `combined_split[[n]]` in function

combined_table_by_dataset <- combined_split[[n]]

# I cannot get code to work where I'm running tidyr and dplyr functions using a variable instead of a fixed value
# as in replacing "treatment_context_properties" with "packed_columns[i]"
# you can use dplyr::select if you use `dplyr::contains(packed_columns[i])`, but I can't get others to work 
# 

#packed_columns <- c("plot_context_properties", "treatment_context_properties", 
#                    "entity_context_properties", "temporal_context_properties", "method_context_properties")

unpack_context_properties <- function(combined_table_by_dataset) {
  #for (i in seq_along(packed_columns)) { 
    
    if (!all(is.na((combined_table_by_dataset[[packed_columns[i]]])))) {
      
      packed_column_unpacked <- combined_table_by_dataset %>% 
        #dplyr::select("observation_id", "treatment_context_id", "treatment_context_properties") %>%
        # preferable formulation in the future
        dplyr::select("observation_id", "treatment_context_id", dplyr::contains(packed_columns[i])) %>%
        distinct() 
      
      packed_column_unpacked <- packed_column_unpacked %>%
        tidyr::separate_longer_delim(packed_columns[i], delim = ";") %>%
        tidyr::separate_wider_delim(packed_columns[i], delim = ":", names_sep = "_")  %>%
        dplyr::mutate(context_property_1 = paste0(packed_columns[i] %>% stringr::str_replace("properties","property"),
                                                              ": ", .[[3]])) %>%
        dplyr::select(-3) %>%
        tidyr::pivot_wider(names_from = dplyr::contains("_1"), values_from = dplyr::contains("_2"))
      
      # XX TODO: need to assign to a name that is created from the specific column in question
      unpacked_treatment_contexts_table <- 
        combined_table_by_dataset %>% 
        left_join(packed_column_unpacked, 
                  by = c("observation_id", "treatment_context_id")) %>% 
        select(-dplyr::contains(packed_columns[i]))
      
    } else {
      
      unpacked_treatment_contexts_table <- 
        combined_table_by_dataset %>% 
        select(-dplyr::contains(packed_columns[i]))
      
    }
  unpacked_treatment_contexts_table
  #}
}


recreate_contexts_dataframe <- function(combined_table_by_dataset) {
  
  if (!all(is.na((combined_table_by_dataset[[packed_columns[i]]])))) {
    
    unpacked_contexts_table <- unpack_context_properties(combined_table_by_dataset)
    
    
    # XX TODO: right now I have explicitly listed treatment_context_id, treatment_context_property - this needs to be a variable as well. 
    # need to work out a simple method - right now all I can think of is instead of having a single vector, have a dataframe with three columns for different formulations of "treatment_context"
    
    long_output <- unpacked_contexts_table %>%
      dplyr::select("dataset_id", "observation_id", "treatment_context_id", dplyr::contains("treatment_context_property")) %>%
      dplyr::left_join(combined_table_by_dataset %>%
                         dplyr::select(dataset_id, observation_id, treatment_context_id) %>%
                         dplyr::distinct(),
                       by = c("dataset_id", "observation_id", "treatment_context_id")) %>% 
      dplyr::select(-observation_id) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(treatment_context_id))
    
    long_output <- long_output %>%
      dplyr::select(dataset_id, treatment_context_id, everything()) %>%
      tidyr::pivot_longer(cols = 3:ncol(long_output)) %>%
      tidyr::separate_wider_delim(value, delim = "<", names_sep = "_") %>%
      rename(context_property = name, value = value_1, description = value_2, link_vals = treatment_context_id) %>%
      mutate(
        context_property = stringr::str_replace(context_property, "treatment_context_property: ", ""),
        description = stringr::str_replace(description, ">", ""),
        link_id = "treatment_context_id",
        category = "treatment_context"
        ) %>%
      select(dataset_id, context_property, category, value, description, link_id, link_vals) %>%
      arrange(link_id, link_vals) %>%
      group_by(dataset_id, context_property, category, value, description, link_id) %>%
      mutate(link_vals = paste0(link_vals, collapse = ", ")) %>%
      ungroup() %>% distinct()
    
  }
  
  long_output
  
} 
