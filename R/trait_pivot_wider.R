#' @title Pivot long format traits table into wide format
#'
#' @description Function to "widen" long format data ("tidy data").
#' 
#' Data in a traits.build databases' traits table are organised in a long format where each trait measurement is on a different row and measurement metadata is recorded in other columns. Multiple traits may be measured as part of a single observation and this function pivots the data wider, such that each trait is its own column. <i>Note that if two trait measurements have the same observation_id but different value types (min, mean, mode, etc.) these will be on separate rows.</i>
#' 
#' The function austraits::trait_pivot_longer reverts the actions of this function.
#' 
#' @param traits The traits table from traits.build database list object
#' @return traits.build traits table in wide format
#' @details
#' `trait_pivot_wider`` has been developed to pivot the traits table for a database build using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build] &
#'   [https://github.com/traitecoevo/traits.build-book]
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits] for how to install old versions of the package or download a newer version of the database."
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

trait_pivot_wider <- function(traits){
  # Determine version using col names of traits table
  if(any(names(traits) %in% "treatment_context_id")){
    version = "5-series"
  }
  
  if(any(str_detect(names(traits), "entity")) & any(names(traits) %in% "treatment_id")){
    version = "4-series"
  } 
  
  if(! any(str_detect(names(traits), "entity")))
    version = "3-series-earlier"

  # Switch how traits are pivoted wider based on version
  switch (version,
          "5-series" = trait_pivot_wider3(traits),
          "4-series" = trait_pivot_wider2(traits),
          "3-series-earlier" = trait_pivot_wider1(traits))
}


#' Pivot wider for >v5.0.0
#' @noRd
#' @keywords internal
trait_pivot_wider3 <- function(traits){ 
  data <- traits
  
  meta_data_cols <- c("unit", "replicates", "measurement_remarks", "basis_of_value")
  
  # A check for if there are more than 1 value_type for a given taxon_name, observation_id and method
  data %>% 
    select(trait_name, value, dataset_id, observation_id, method_id, method_context_id, repeat_measurements_id, value_type) %>% 
    group_by(dataset_id,  observation_id, method_id, method_context_id, repeat_measurements_id) %>% 
    summarise(n_value_type = length(unique(value_type))) %>% 
    arrange(observation_id) %>% 
    dplyr::filter(n_value_type > 1) -> check_value_type
  
  if(nrow(check_value_type) > 1){
    
    traits %>% 
      select(- all_of(meta_data_cols)) %>% 
      group_by(dataset_id,  observation_id, method_id, method_context_id, repeat_measurements_id, value_type) %>% 
      pivot_wider(names_from = trait_name,
                  values_from = value) |> 
      dplyr::ungroup()
  } else{
    
    meta_data_cols <- c(meta_data_cols, "value_type")
    
    traits %>% 
      select(- all_of(meta_data_cols)) %>% 
      group_by(dataset_id,  observation_id, method_id, method_context_id, repeat_measurements_id) %>% 
      pivot_wider(names_from = trait_name,
                  values_from = value) |> 
      dplyr::ungroup()
  }
}

#' Pivot wider for >v3.0.2 & <5.0.0
#' @noRd
#' @keywords internal
#' @importFrom dplyr select group_by arrange filter summarise
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect

trait_pivot_wider2 <- function(traits){ 
  data <- traits
  
  meta_data_cols <- c("unit", "replicates", "measurement_remarks", "basis_of_record", "basis_of_value")
  
  # A check for if there are more than 1 value_type for a given taxon_name, observation_id and method
  data %>% 
    dplyr::select(taxon_name, trait_name, value_type, value, observation_id, method_id) %>% 
    dplyr::group_by(taxon_name, observation_id, method_id) %>% 
    dplyr::summarise(n_value_type = length(unique(value_type))) %>% 
    dplyr::arrange(observation_id) %>% 
    dplyr::filter(n_value_type > 1) -> check_value_type
  
  if(nrow(check_value_type) > 1){
  
    traits %>% 
      dplyr::select(-all_of(meta_data_cols)) %>% 
      dplyr::group_by(dataset_id, source_id, taxon_name, original_name, observation_id, method_id, value_type) %>% 
      tidyr::pivot_wider(names_from = trait_name,
                  values_from = value) |> 
      dplyr::ungroup()
    
  } else{
    
    meta_data_cols <- c(meta_data_cols, "value_type")
    
    traits %>% 
      dplyr::select(- all_of(meta_data_cols)) %>% 
      dplyr::group_by(dataset_id, source_id, taxon_name, original_name, observation_id, method_id) %>% 
      tidyr::pivot_wider(names_from = trait_name,
                  values_from = value) |> 
      dplyr::ungroup()
  }
}

#' Pivot wider for <=v3.0.2
#' @noRd
#' @keywords internal
trait_pivot_wider1 <- function(traits){
  data <- traits
  
  check_obs <- data %>% 
    dplyr::group_by(trait_name, observation_id) %>% 
    dplyr::summarise(dplyr::n()) %>% 
    dplyr::filter(`dplyr::n()` > 1) %>%
    dplyr::select(trait_name, observation_id)
  
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
    dplyr::select(dataset_id:trait_name, {{var_to_spread}}, original_name) %>% 
    tidyr::pivot_wider(id_cols = c(dataset_id:observation_id, original_name), names_from = trait_name, values_from = {{var_to_spread}}) %>% 
    dplyr::select(-original_name, original_name) # Moving original name to the end
  
  ret
}
