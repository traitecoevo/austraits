#' @title Extract all data for a particular dataset
#'
#' @description Function to subset of all data associated with a particular dataset from austraits
#' @usage extract_dataset(data, dataset_id), 
#' @param data - A large list of tibbles build from austraits
#' @param dataset_id - character string that matches a dataset_id in the data
#' @return A large list of tibbles containing all austraits information for one particular dataset
#'
#' @examples 
#' \dontrun{
#'extract_dataset(austraits, "Falster_2003")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
#' @importFrom rlang .data

extract_dataset <- function(austraits, dataset_id) {
  
  austraits$taxonomic_updates <-
    tidyr::separate_rows(austraits$taxonomic_updates, dataset_id, sep=" ")
  
  ret <- list()
  for(v in c("traits", "sites", "contexts", "methods", "excluded_data", "taxonomic_updates",  "contributors"))
    ret[[v]] <- austraits[[v]][ austraits[[v]][["dataset_id"]] %in% dataset_id,]
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(.data$taxon_name %in% ret[["traits"]][["taxon_name"]])
  
  ret[["definitions"]] <- austraits[["definitions"]]
  ret[["build_info"]] <- austraits[["build_info"]]
  
  keys <- ret$methods %>% dplyr::select(.data$source_primary_key,.data$source_secondary_key) %>% dplyr::na_if("") %>% unlist() %>% stats::na.omit() %>% unique()
  
  ret[["sources"]] <- austraits[["sources"]][keys]
  
  assertthat::are_equal(sort(names(austraits)), sort(names(ret)))
  
  ret[names(austraits)]
}

trait_type  <- function(trait_name, definitions) {
  extract_list_element(trait_name, definitions$traits$elements, "type")
}

trait_is_numeric <- function(trait_name, definitions) {
  trait_type(trait_name, definitions) == "numeric"
}

trait_is_categorical <- function(trait_name, definitions) {
  !trait_is_numeric(trait_name, definitions)
}