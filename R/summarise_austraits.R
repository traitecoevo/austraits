#' @title Summarise counts for a particular variable of interest
#'
#' @name summarise_database
#' @param database traits.build database (list object)
#' @param var variable you use wish to see summary of (trait_name, genus, family)
#'
#' @return dataframe of unique levels of variable with counts and percentage
#' @export
#' @examples
#' \dontrun{
#' summarise_database(database = austraits, "trait_name")
#' summarise_database(database = austraits, "family")
#' }


summarise_database <- function(database, var){
  
  if(!var %in% c("trait_name", "family", "genus")){
    stop(paste0("Print summary for ", var, " has not been implemented! see examples)"))
  }
  
  switch(var,
         trait_name = summarise_database_traits(database, var),
         genus =  summarise_database_taxa(database, var),
         family = summarise_database_taxa(database, var)
  )
}

#' @noRd
#' @keywords internal
         
summarise_database_traits <-function(database, var) {

  ret <- 
    database[["traits"]] %>% 
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
  sum_stats <- database[["traits"]] %>% 
    dplyr::group_by(trait_name) %>% 
    dplyr::summarise(n_dataset = length(unique(dataset_id)),
                     n_taxa = length(unique(taxon_name))) 
  
  ret <- dplyr::left_join(ret, sum_stats, by = "trait_name")
  
  # Organise
  ret %>% dplyr::select(1, dplyr::starts_with("n_"), percent_total) %>% tibble::tibble()
}

#' @noRd
#' @keywords internal

summarise_database_taxa <-function(database, var) {
  
  #Join taxonomic info
  database <- database %>% join_taxa()
  
  # Create table
  ret <- database[["traits"]] %>% 
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
  sum_stats <- database[["traits"]] %>% 
    dplyr::group_by(!!rlang::sym(var)) %>% 
    dplyr::summarise(n_dataset = length(unique(dataset_id)),
                     n_taxa = length(unique(taxon_name)))
  
  ret <- dplyr::left_join(ret, sum_stats, by = var)
  
  # Organise
  ret %>% dplyr::select(1, dplyr::starts_with("n_"), percent_total) %>% tibble::tibble()
  
}
