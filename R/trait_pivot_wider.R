#' @title Pivot long format ausTrait data into a wide format
#'
#' @description trait_pivot_wider "widens" long format data ("tidy data")
#' ausTraits data is organised in a long format where observations are on different rows and the type of observation is denoted by trait name
#' This function converts the data into wide format so that each trait in it's own column. 
#' Note that some studies have multiple rows of data for each observation_id, so this function will create four lists (value, unit, value_type and replicates) while retaining the other columns in its exisiting format
#' @usage trait_pivot_wider(data)
#' @param data A tibble generated from ausTraits - see example
#' @return list of five tibbles in wide format
#'
#' @examples 
#' \dontrun{
#' data <- austraits$traits %>% 
#' filter(dataset_id == "Falster_2003")
#' data #long format 
#' traits_wide <- trait_pivot_wider(data) 
#' traits_wide #wide format
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
#' @importFrom rlang .data

trait_pivot_wider <- function(data) {
  
  vars <- c("value", "unit", "date", "value_type", "replicates")
  ret <- list()
  for(v in vars) {
    ret[[v]] <- data %>% 
      dplyr::rename(to_spread = !!v) %>%
      dplyr::select(.data$dataset_id, .data$taxon_name, .data$site_name, .data$observation_id, .data$trait_name, .data$to_spread, .data$original_name) %>%
      tidyr::pivot_wider(names_from = .data$trait_name, values_from = .data$to_spread)
  }
  
  ret
}
