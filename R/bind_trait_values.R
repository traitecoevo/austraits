#' @title Bind trait values
#'
#' @description This function condenses data for studies that have multiple observations for a given trait into a single row. 
#' This function concatenates multiple values into a single cell
#' @usage bind_trait_values(trait_data)
#' @param trait_data The trait data frame generated from austraits - see example
#' @return tibble that is condensed down where multiple observations in value, value_type and replicates are collapsed down and separated by '--' 
#'
#' @examples 
#' \dontrun{
#' traits <- austraits$traits %>% 
#' dplyr::filter(dataset_id == "Falster_2005_1")
#' traits
#' traits_bind <- bind_trait_values(traits)
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
#' @importFrom rlang .data

bind_trait_values <- function(trait_data) {
  
  bind_x <- function(x) paste0(x, collapse = "--")
  
  bind_values_worker <- function(.data) {
    # If more than one value per group need to combine
    if(nrow(.data) > 1) {
      return(
        .data %>% 
          dplyr::mutate(value = bind_x(.data$value),
                        value_type = bind_x(.data$value_type),
                        replicates = bind_x(.data$replicates)) %>%
          dplyr::filter(dplyr::row_number()==1) 
      )
    }
    .data
  }
  
  trait_data  %>% 
    dplyr::group_by(.data$observation_id, .data$trait_name) %>% 
    bind_values_worker() %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type)
}