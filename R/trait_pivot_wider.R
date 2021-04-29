#' @title Pivot long format ausTrait data into a wide format
#'
#' @description trait_pivot_wider "widens" long format data ("tidy data")
#' Long format data has measurements on different rows and the type of measurement denoted in a single column.
#' This function manipulates the data into wide format so that each trait in it's own column.
#' @usage trait_pivot_wider(data)
#' @param data A data object generated from ausTraits - see example
#' @return data
#' @export
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
#' 
trait_pivot_wider <- function(data) {
  
  vars <- c("value", "unit", "value_type", "replicates")
  ret <- list()
  for(v in vars) {
    ret[[v]] <- data %>% 
      rename(to_spread = !!v) %>%
      select(dataset_id, taxon_name, site_name, observation_id, trait_name, to_spread, original_name) %>%
      pivot_wider(names_from = trait_name, values_from = to_spread)
  }
  
  ret
}