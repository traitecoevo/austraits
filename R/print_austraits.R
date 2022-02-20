#' Summarise counts for a particular variable of interest
#'
#' @param austraits A large list of tibbles built from austraits
#' @param var variable you use wish to see summary of
#'
#' @return dataframe of unique levels of variable with counts and percentage
#' @export
#' @examples
#' \dontrun{
#' print_austraits(austraits, "trait_name")
#' print_austraits(austraits, family)
#' }
#' @importFrom rlang .data

print_austraits <- function(austraits, var){
  switch(var,
         trait_name = print_austraits_traits(austraits, var),
         dataset_id = print_austraits_traits(austraits, var),
         genus =  print_austraits_taxa(austraits, var),
         family = print_austraits_taxa(austraits, var)
  )
}

#' @rdname print_austraits
         
print_austraits_traits <-function(austraits, var) {

  ret <- 
    austraits[["traits"]] %>% 
    dplyr::pull({{var}}) %>% 
    sort() %>% 
    janitor::tabyl() 
  
  # Fix first column name
  names(ret)[1] <- var
  
  # Renaming
  ret <- ret %>% dplyr::mutate(n_records = .data$n,
                               n = NULL,
                               percent_total = signif(.data$percent, 3),
                               percent = NULL)
  # Summary statistics
  sum_stats <- austraits[["traits"]] %>% 
    dplyr::group_by(.data$trait_name) %>% 
    dplyr::summarise(n_dataset = length(unique(.data$dataset_id)),
                     n_taxa = length(unique(.data$taxon_name))) 
  
  ret <- dplyr::left_join(ret, sum_stats, by = "trait_name")
  
  # Organise
  ret %>% dplyr::select(1, dplyr::starts_with("n_"), .data$percent_total)
}

#' @rdname print_austraits

print_austraits_taxa <-function(austraits, var) {
  
  #Join taxonomic info
  austraits <- austraits %>% join_taxonomy()
  
  # Create table
  ret <- austraits[["traits"]] %>% 
    dplyr::pull(var) %>% 
    sort() %>% 
    janitor::tabyl() 
  
  # Fix first column name
  suppressWarnings(names(ret)[1] <- var
  )
  
  # Renaming
  ret <- ret %>% dplyr::mutate(n_records = .data$n,
                               n = NULL,
                               percent_total = signif(.data$percent, 3),
                               percent = NULL)


  # Summary statistics
  sum_stats <- austraits[["traits"]] %>% 
    dplyr::group_by(!!var) %>% 
    dplyr::summarise(n_dataset = length(unique(.data$dataset_id)),
                     n_taxa = length(unique(.data$taxon_name)))
  
  names(sum_stats)[1] <- sum_stats[1,1]
  
  ret <- dplyr::left_join(ret, sum_stats, by = var)
  
  # Organise
  ret %>% dplyr::select(1, dplyr::starts_with("n_"), .data$percent_total)
  
}
