#' @title Pivot long format austrait data into a wide format
#'
#' @description trait_pivot_wider "widens" long format data ("tidy data")
#' austraits data is organised in a long format where observations are on different rows and the type of observation is denoted by trait name
#' This function converts the data into wide format so that each trait in it's own column. 
#' Note that some studies have multiple rows of data for each observation_id, so this function will create four lists (value, unit, value_type,date and replicates) with the identifying columns as well as trait data arranged in columns. 
#' @usage trait_pivot_wider(data)
#' @param traits The traits table from austraits list
#' @return list of five tibbles in wide format
#'
#' @examples 
#' \dontrun{
#' data <- austraits$traits %>% filter(dataset_id == "Falster_2003")
#' data #long format 
#' traits_wide <- trait_pivot_wider(data) 
#' traits_wide #wide format
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
#' @importFrom rlang .data

trait_pivot_wider <- function(traits){
  # Determine version using col names of traits table
  if(any(str_detect(names(traits), "entity"))){
    version = "newer"
  } else(
    version = "older"
  )
  
  # Switch how traits are pivoted wider based on version
  switch (version,
          'newer' = trait_pivot_wider2(traits),
          'older' = trait_pivot_wider1(traits))
}


trait_pivot_wider2 <- function(traits){ # UNDER CONSTRUCTION
  data <- traits
  
  meta_data_cols <- c("unit", "replicates", "measurement_remarks", "basis_of_record", "basis_of_value")
  
  # Option to remove methods for users that have join_methods to the traits table?
  
  # A check for if there are more than 1 value_type for a given taxon_name, observation_id and method
  data %>% 
    select(.data$taxon_name, .data$trait_name, .data$value_type, .data$value, .data$observation_id, .data$method_id) %>% 
    group_by(.data$taxon_name, .data$observation_id, .data$method_id) %>% 
    summarise(n_value_type = length(unique(.data$value_type))) %>% 
    arrange(.data$observation_id) %>% 
    filter(n_value_type > 1) -> check_value_type
  
  if(nrow(check_value_type) > 1){
    meta_data_cols <- c(meta_data_cols, "value_type")
    traits %>% 
      select(- all_of(meta_data_cols)) %>% 
      group_by(dataset_id, source_id, taxon_name, original_name, observation_id, method_id) %>% 
      pivot_wider(names_from = trait_name,
                  values_from = value) 
  } else{
    
    traits %>% 
      select(- all_of(meta_data_cols)) %>% 
      group_by(dataset_id, source_id, taxon_name, original_name, observation_id, method_id, value_type) %>% 
      pivot_wider(names_from = trait_name,
                  values_from = value) 
  }
}

trait_pivot_wider1 <- function(traits){
  data <- traits
  
  check_obs <- data %>% 
    dplyr::group_by(.data$trait_name, .data$observation_id) %>% 
    dplyr::summarise(dplyr::n()) %>% 
    dplyr::filter(`dplyr::n()` > 1) %>%
    dplyr::select(.data$trait_name, .data$observation_id)
  
  if(nrow(check_obs) >1){
    rlang::abort("There are multiple data points for the same observation - try summarise_trait_means() before widening!")
  }
  
  vars <- c("value", "unit", "date", "value_type", "replicates")
  
  ret  <- purrr::map(vars, piv_wide, data = data)
  
  names(ret) <- vars
  
  ret
}

#' Helper function to pivot wider for AusTraits <= v3.0.2
#' @keywords internal
#' @noRd

piv_wide <- function(data, var_to_spread){
  ret <- data %>% 
    dplyr::select(.data$dataset_id:.data$trait_name, {{var_to_spread}}, .data$original_name) %>% 
    tidyr::pivot_wider(id_cols = c(.data$dataset_id:.data$observation_id, .data$original_name), names_from = .data$trait_name, values_from = {{var_to_spread}}) %>% 
    dplyr::select(-.data$original_name, .data$original_name) # Moving original name to the end
  
  ret
}
