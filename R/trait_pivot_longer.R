#' @title Pivot wide format ausTrait data into a long format
#'
#' @description trait_pivot_longer "gathers" wide format data into a "tidy" format
#' This function converts the data into long format where observations are on different rows and the type of observation is denoted by trait name
#' @usage trait_pivot_longer(data)
#' @param data A tibble in wide format generated from trait_pivot_wider - see example
#' @param definitions Not needed? 
#' @return A tibble in long format
#'
#' @examples 
#' \dontrun{
#' data <- austraits$traits %>% 
#' filter(dataset_id == "Falster_2003")
#' data #long format 
#' traits_wide <- trait_pivot_wider(data) 
#' traits_wide #wide format
#' 
#' values_long <- trait_pivot_longer(traits_wide, data$definitions)
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
#' @importFrom rlang .data
#
trait_pivot_longer <- function(data, definitions) {
  
  id_variables <- c("dataset_id", "taxon_name", "site_name", "observation_id", "trait_name", "value", "unit", "date", "value_type", "replicates", "original_name")
  
  traits <- names(data$value)[!(names(data$value) %in% id_variables)]
  
  vars <- names(data)
  
  gather_f <- function(df, v) {
    df[[v]] %>% tidyr::pivot_longer(cols = tidyselect::any_of(traits), names_to = "trait_name", values_to = {{v}})
  }
  
  ret <- gather_f(data, vars[1])
  
  for(v in vars[-c(1)])
    ret <- ret %>% 
    dplyr::left_join(
      gather_f(data, v), 
      by = dplyr::setdiff(id_variables, vars)
    )
  
  ret <- ret %>% 
    dplyr::mutate(value = dplyr::na_if(value, y = "NA")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(observation_id, trait_name) %>%
    dplyr::select(id_variables)
  
  ret
}
