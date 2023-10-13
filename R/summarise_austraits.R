#' @title Summarise counts for a particular variable of interest
#'
#' @name summarise_austraits
#' @param austraits A large list of tibbles built from austraits
#' @param var variable you use wish to see summary of (trait_name, genus, family)
#'
#' @return dataframe of unique levels of variable with counts and percentage
#' @export
#' @examples
#' \dontrun{
#' summarise_austraits(austraits, "trait_name")
#' summarise_austraits(austraits, "family")
#' }


summarise_austraits <- function(austraits, var){
  
  if(!var %in% c("trait_name", "family", "genus")){
    stop(paste0("Print summary for ", var, " has not been implemented! see examples)"))
  }
  
  switch(var,
         trait_name = summarise_austraits_traits(austraits, var),
         genus =  summarise_austraits_taxa(austraits, var),
         family = summarise_austraits_taxa(austraits, var)
  )
}

#' @noRd
#' @keywords internal
         
summarise_austraits_traits <-function(austraits, var) {

  ret <- 
    austraits[["traits"]] %>% 
    dplyr::pull({{var}}) %>% 
    sort() %>% 
    janitor::tabyl() 
  
  # Fix first column name
  names(ret)[1] <- var
  
  # Renaming
  ret <- ret %>% dplyr::mutate(n_records = n,
                               n = NULL,
                               percent_total = signif(percent, 3),
                               percent = NULL)
  # Summary statistics
  sum_stats <- austraits[["traits"]] %>% 
    dplyr::group_by(.data$trait_name) %>% 
    dplyr::summarise(n_dataset = length(unique(.data$dataset_id)),
                     n_taxa = length(unique(.data$taxon_name))) 
  
  ret <- dplyr::left_join(ret, sum_stats, by = "trait_name")
  
  # Organise
  ret %>% dplyr::select(1, dplyr::starts_with("n_"), percent_total)
}

#' @noRd
#' @keywords internal

summarise_austraits_taxa <-function(austraits, var) {
  
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
  ret <- ret %>% dplyr::mutate(n_records = n,
                               n = NULL,
                               percent_total = signif(percent, 3),
                               percent = NULL)


  
  # Summary statistics (https://stackoverflow.com/questions/55425976/use-quoted-variable-in-group-by-mutate-function-call)
  sum_stats <- austraits[["traits"]] %>% 
    dplyr::group_by(!!rlang::sym(var)) %>% 
    dplyr::summarise(n_dataset = length(unique(.data$dataset_id)),
                     n_taxa = length(unique(.data$taxon_name)))
  
  ret <- dplyr::left_join(ret, sum_stats, by = var)
  
  # Organise
  ret %>% dplyr::select(1, dplyr::starts_with("n_"), percent_total)
  
}
