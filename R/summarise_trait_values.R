#' Compute mean trait values for studies that have multiple observations for a given trait
#'
#' @param trait_data trait table for austraits list
#'
#' @return A reduced trait table, mean values are flagged with the suffix '_summarised' in value_type
#' @export
#'
#' @examples
#' \dontrun{
#' data <- austraits$traits %>% filter(dataset_id == "Falster_2003")
#' data %>% summarise_trait_means()
#' }


summarise_trait_means <- function(trait_data){
  suppressWarnings(
    trait_data  %>% 
      dplyr::mutate(value = as.numeric(value),
                    replicates = as.numeric(replicates)) -> trait_data
  )
  
  # Identify which ones need summarising
  target <- trait_data %>% 
    dplyr::group_by(trait_name, observation_id) %>% 
    dplyr::summarise(dplyr::n()) %>% 
    dplyr::filter(`dplyr::n()` > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  # # Identify which ones that don't need to change
  original <- trait_data %>%
    dplyr::group_by(trait_name, observation_id) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::filter(! `dplyr::n()`  > 1) %>%
    dplyr::select(trait_name, observation_id)

  original_df <- purrr::map2_dfr(original$trait_name, original$observation_id,
                                 ~ dplyr::filter(trait_data, trait_name == .x & observation_id == .y))

  # Filter out the ones where nrows is > 1
  target_ls <- purrr::map2(target$trait_name, target$observation_id,
                           ~ dplyr::filter(trait_data, trait_name == .x & observation_id == .y)
  )

  # Manipulate: Compute means, update value type and replicates
  target_summarised <- purrr::map(target_ls,
                                  ~ .x %>% dplyr::mutate(value = mean(value, na.rm = TRUE),
                                                         value_type = paste0(value_type, "_summarised"),
                                                         replicates = sum(replicates)) %>%
                                    dplyr::filter(dplyr::row_number() == 1)

  )


  target_bound <- target_summarised %>% dplyr::bind_rows()

  # Append back to the ones where nrows = 1
  ret <- dplyr::bind_rows(original_df, target_bound)

  # Sort by observation_id and return
  ret %>% dplyr::arrange(observation_id)
}