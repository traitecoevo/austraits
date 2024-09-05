#' @title Extract all data for a particular dataset
#'
#' @description Function to subset of all data associated with a particular dataset from austraits
#' @usage extract_dataset(austraits, dataset_id)
#' @param austraits - A large list of tibbles built from austraits
#' @param dataset_id - character string that matches a dataset_id in the data
#' @return A large list of tibbles containing all austraits information for one particular dataset
#' @details `extract_dataset()` no longer supports AusTraits version <= 5.0.0, see [this page](https://github.com/traitecoevo/austraits) for details. `r lifecycle::badge("deprecated")` 
#' @examples
#' \dontrun{
#' extract_dataset(austraits, "Falster_2003")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export


extract_dataset <- function(austraits, dataset_id) {
  
  # Check compatability
  status <- suppressMessages(check_compatibility(austraits))
  
  # Switch for different versions
  version <- what_version(austraits)
  
  if(what_version(austraits) %in% "5-series" | status){
    version <- "new" 
  } else
    version <- "old"
  
  switch (version,
          'new' = extract_dataset2(austraits, dataset_id),
          'old' = extract_dataset1(austraits, dataset_id),
  )
}

#' @title Extract specific dataset from austraits object for versions >3.0.2
#' @noRd 

extract_dataset2 <- function(austraits, dataset_id){
  austraits$taxonomic_updates <-
    tidyr::separate_rows(austraits$taxonomic_updates, dataset_id, sep=" ")
  
  ret <- list()
  for(v in c("traits", "locations", "contexts", "methods",
             "excluded_data", "taxonomic_updates",  "contributors"))
    ret[[v]] <- austraits[[v]][ austraits[[v]][["dataset_id"]] %in% dataset_id,]
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]])
  
  ret[["definitions"]] <- austraits[["definitions"]]
  ret[["build_info"]] <- austraits[["build_info"]]
  ret[["schema"]] <- austraits[["schema"]]
  ret[["metadata"]] <- austraits[["metadata"]]
  
  keys <- dplyr::union(ret$methods$source_primary_key, 
                       ret$methods$source_secondary_key %>% strsplit("; ") %>% unlist()) %>% 
    unique() %>% stats::na.omit() %>% as.character()
  
  ret[["sources"]] <- austraits$sources[keys]
  
  ret[["sources"]] <- austraits[["sources"]][keys]
  
  assertthat::are_equal(sort(names(austraits)), sort(names(ret)))
  
  ret[names(austraits)]
  
  # Assign class
  attr(ret, "class") <- "austraits"
  
  ret
}


#' @title Extract specific dataset from austraits object for versions <=3.0.2
#' @noRd

extract_dataset1 <- function(austraits, dataset_id){

  function_not_supported(austraits)

  }


