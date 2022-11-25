#' @title Pivot wide format AusTrait data into a long format
#'
#' @description trait_pivot_longer "gathers" wide format data into a "tidy" format
#' This function converts the data into long format where observations are on different rows and the type of observation is denoted by trait name.
#' In other words, trait_pivot_longer reverts the actions of trait_pivot_wider
#' @param wide_data output from trait_pivot_wider. For <= v3.0.2 list object containing wide data generated,For > v3.0.2 a tibble of wide data  
#' @return A tibble in long format
#' @details 
#' - If `bind_trait_values` or `summarise_trait_means` was applied prior to `trait_pivot_wider` for AusTraits
#' <= v3.0.2, `trait_pivot_longer` will return a tibble with fewer observations than the original traits table. 
#' - For AusTraits version >3.0.2,  `trait_pivot_longer` will return a tibble with fewer columns than that original traits table
#'    - The excluded columns include: "unit", "replicates", "measurement_remarks", "basis_of_record", "basis_of_value"
#' 
#' @examples 
#' \dontrun{
#' data <- austraits$traits %>% 
#' filter(dataset_id == "Falster_2003")
#' data #long format 
#' traits_wide <- trait_pivot_wider(data) 
#' traits_wide #wide format
#' 
#' values_long <- trait_pivot_longer(traits_wide)
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @author Fonti Kar - fonti.kar@unsw.edu.au
#' @export

#
trait_pivot_longer <- function(wide_data){
  # Determine version using col names of traits table
  if(any(str_detect(names(wide_data), "entity"))){
    version = "newer"
  } else(
    version = "older"
  )
  
  # Switch how traits are pivoted wider based on version
  switch (version,
          'newer' = trait_pivot_longer2(wide_data),
          'older' = trait_pivot_longer1(wide_data))
}

#' Gathers 'widened' data for > v3.0.2
#' @noRd
#' @keywords internal
trait_pivot_longer2 <- function(wide_data) {
  wide_data %>%
    pivot_longer(cols = 18:ncol(.), names_to = "trait_name", values_drop_na = TRUE) 
}

#' Gathers 'widened' data for <= v3.0.2
#' @noRd
#' @keywords internal
trait_pivot_longer1 <- function(wide_data) {
  data <- wide_data
  
  id_variables <- c("dataset_id", "taxon_name", "site_name", "context_name", "observation_id", "trait_name", "value", "unit", "date", "value_type", "replicates", "original_name")
  
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
    dplyr::distinct() %>% 
    dplyr::arrange(observation_id, trait_name) %>%
    dplyr::select(tidyselect::all_of(id_variables))
  
  ret
}
