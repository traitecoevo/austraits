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

#' 
separate_trait_values <- function(data, definitions) {
  
  separate_x <- function(x) strsplit(x, "--")[[1]]
  
  separate_values_worker <- function(df) {
    
    df[rep(1, df$n_vals[1]),] %>%
      dplyr::mutate(
        value = separate_x(value[1]),
        value_type = separate_x(value_type[1]),
        basis_of_value = separate_x(basis_of_value[1]),
        replicates = separate_x(replicates[1])
      )
  }
  
  # record the number of values in each row of data
  data$n_vals <- 1 + stringr::str_count(data$value_type, "--")
  
  # separate out those rows requiring no modification
  out_1 <- data %>% 
    dplyr::filter(n_vals == 1)
  
  if (nrow(dplyr::filter(data, n_vals > 1)) > 0) {
    # separate out those rows requiring modification & modify
    out_2 <- data %>% 
      dplyr::filter(n_vals > 1) %>% 
      dplyr::group_split(stringr::str_c(dataset_id, observation_id, trait_name, method_id, method_context_id, repeat_measurements_id, sep = " ")) %>%    
      lapply(separate_values_worker) %>% 
      dplyr::bind_rows() %>% 
      dplyr::select(dataset_id:n_vals)
    
    # join it all back together, clean up and sort as in original
    dplyr::bind_rows(out_1, out_2) %>% 
      dplyr::select(-n_vals) %>% 
      dplyr::mutate(replicates = clean_NA(replicates),
                    value_type = factor(clean_NA(value_type), levels = names(definitions$definitions$value_type$values))
      ) %>% 
      dplyr::arrange(observation_id, trait_name, value_type)
  }
}
