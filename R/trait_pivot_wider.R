#' @title Pivot long format austrait data into a wide format
#'
#' @description trait_pivot_wider "widens" long format data ("tidy data")
#' austraits data is organised in a long format where observations are on different rows and the type of observation is denoted by trait name
#' This function converts the data into wide format so that each trait in it's own column. 
#' Note that some studies have multiple rows of data for each observation_id, so this function will create four lists (value, unit, value_type,date and replicates) with the identifying columns as well as trait data arranged in columns. 
#' @usage trait_pivot_wider(data)
#' @param data The traits table from austraits list
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

trait_pivot_wider <- function(data){
  
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

piv_wide <- function(data, var_to_spread){
  ret <- data %>% 
    dplyr::select(.data$dataset_id:.data$trait_name, {{var_to_spread}}, .data$original_name) %>% 
    tidyr::pivot_wider(id_cols = c(.data$dataset_id:.data$observation_id, .data$original_name), names_from = .data$trait_name, values_from = {{var_to_spread}}) %>% 
    dplyr::select(-.data$original_name, .data$original_name) # Moving original name to the end
  
  ret
}
