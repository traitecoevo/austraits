#' @title Separate bounded trait values
#'
#' @description This function reverts the action of bind_trait_values. 
#' This function separates values that were concatenated so that studies that have multiple observations for a given trait will have seperate row for each observation.
#' @usage separate_trait_values(data, definitions)
#' @param data The trait data frame generated from austraits - see example
#' @param definitions The austraits definitions data frame
#' @return trait tibble
#' @examples 
#' \dontrun{
#' traits <- austraits$traits %>% 
#' dplyr::filter(dataset_id == "Falster_2005_1")
#' traits
#' traits_bind <- bind_trait_values(traits)
#' separate_trait_values(traits_bind)
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
#' @importFrom rlang .data
#' 
separate_trait_values <- function(data, definitions) {
  
  separate_x <- function(x) strsplit(x, "--")[[1]]
  
  separate_values_worker <- function(df) {
    
    df[rep(1, df$n_vals[1]),] %>%
      dplyr::mutate(
        value = separate_x(.data$value[1]),
        value_type = separate_x(.data$value_type[1]),
        replicates = separate_x(.data$replicates[1])
      )
  }
  
  # record the number of values in each row of data
  data$n_vals <- 1 + stringr::str_count(data$value_type, "--")
  
  # separate out those rows requiring no modification
  out_1 <- data %>% 
    dplyr::filter(.data$n_vals == 1)
  
  # separate out those rows requiring modification & modify
  out_2 <- data %>% 
    dplyr::filter(.data$n_vals > 1) %>% 
    dplyr::group_split(stringr::str_c(.data$observation_id, .data$trait_name, sep = " ")) %>%    
    lapply(separate_values_worker) %>% 
    dplyr::bind_rows() %>% 
    dplyr::select(.data$dataset_id:.data$n_vals)
  
  # join it all back together, clean up and sort as in original
  dplyr::bind_rows(out_1, out_2) %>% 
    dplyr::select(-.data$n_vals) %>% 
    dplyr::mutate(replicates = clean_NA(.data$replicates),
                  value_type = factor(clean_NA(.data$value_type), levels = names(definitions$definitions$value_type$values))
    ) %>% 
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type)
}