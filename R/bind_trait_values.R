#' @title Bind trait values
#'
#' @description This function condenses data for studies that have multiple observations for a given trait into a single row. 
#' This function concatenates multiple values into a single cell
#' @usage bind_trait_values(trait_data)
#' @param trait_data the traits table in a traits.build database -- see example
#' @return tibble that is condensed down where multiple observations in value, value_type and replicates are collapsed down and separated by '--' 
#'
#' @examples 
#' \dontrun{
#' traits <- austraits$traits %>% 
#' dplyr::filter(dataset_id == "ABRS_1981")
#' traits
#' traits_bind <- bind_trait_values(traits)
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export


bind_trait_values <- function(trait_data) {
  
  bind_x <- function(x) paste0(x, collapse = "--")
  
  bind_values_worker <- function(.data) {
    # If more than one value per group need to combine
    if(nrow(.data) > 1) {
      return(
        .data %>% 
          dplyr::mutate(value = bind_x(.data$value),
                        value_type = bind_x(value_type),
                        basis_of_value = bind_x(basis_of_value),
                        replicates = bind_x(replicates)) %>%
          dplyr::filter(dplyr::row_number()==1) 
      )
    }
    .data
  }
  
  trait_data  %>% 
    dplyr::group_by(dataset_id, observation_id, trait_name, method_id, method_context_id, repeat_measurements_id) %>% 
    bind_values_worker() %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(dataset_id, observation_id, trait_name, value_type, method_id, method_context_id, repeat_measurements_id)
}
