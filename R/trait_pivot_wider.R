#' @title Pivot long format traits table into wide format
#'
#' @description Function to "widen" long format data ("tidy data").
#' 
#' Data in a traits.build databases' traits table are organised in a long format where each trait measurement is on a different row and measurement metadata is recorded in other columns. Multiple traits may be measured as part of a single observation and this function pivots the data wider, such that each trait is its own column. <i>Note that if two trait measurements have the same observation_id but different value types (min, mean, mode, etc.) these will be on separate rows.</i>
#' 
#' The function austraits::trait_pivot_longer reverts the actions of this function.
#' 
#' @param database The traits tibble from a traits.build database
#' @return traits.build traits table in wide format
#' @details
#' `trait_pivot_wider`` has been developed to pivot the traits table for a database build using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits) for how to install old versions of the package or download a newer version of the database.
#' 

#' @examples 
#' \dontrun{
#' 
#' data <- austraits_5.0.0_lite$traits %>% filter(dataset_id == "Falster_2003")
#' data #long format 
#' traits_wide <- trait_pivot_wider(data) 
#' traits_wide #wide format
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export

trait_pivot_wider <- function(database){
  # Extract traits table if needed
  traits <- get_traits_table(database)
  
  # Check compatibility
  status <- check_traits_compatibility(traits)
  
  # If compatible
  if(!status){
    function_not_supported(database)
  }

  metadata_cols <- c("unit", "replicates", "measurement_remarks", "basis_of_value")
  
  # A check for if there are more than 1 value_type for a given taxon_name, observation_id and method
  check_value_type <- traits %>%
    dplyr::select(dplyr::all_of(c(
      "trait_name", "value", "dataset_id", "observation_id", "method_id", "method_context_id",
      "repeat_measurements_id", "value_type"))) %>%
    dplyr::group_by(
      .data$dataset_id, .data$observation_id, .data$method_id,
      .data$method_context_id, .data$repeat_measurements_id) %>%
    dplyr::summarise(n_value_type = length(unique(.data$value_type))) %>%
    dplyr::arrange(.data$observation_id) %>%
    dplyr::filter(.data$n_value_type > 1)
  
  if (nrow(check_value_type) > 1) {
    
    traits %>%
      tidyr::pivot_wider(
        names_from = "trait_name",
        values_from = "value",
        id_cols = -dplyr::all_of(metadata_cols)
      )
    
  } else {
    
    metadata_cols <- c(metadata_cols, "value_type")
    
    traits %>%
      tidyr::pivot_wider(
        names_from = "trait_name",
        values_from = "value",
        id_cols = -dplyr::all_of(metadata_cols)
      )
  }
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
