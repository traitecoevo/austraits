#' @title Extract data for specific taxa
#'
#' @description Function to subset of all data associated with a particular dataset from austraits
#' @usage extract_taxa(austraits, family, genus)
#' @param austraits - A large list of tibbles built from austraits
#' @param family - character string of family
#' @param genus - character string of genus
#' @param taxon_name - character string of taxon name
#' @return A large list of tibbles containing all austraits information for specificied taxa
#'
#' @examples 
#' \dontrun{
#'extract_taxa(austraits, family = "Proteaceae")
#'extract_taxa(austraits, genus = "Acacia")
#' }
#' @author Fonti Kar - f.kar@unsw.edu.au
#' @export
#' @importFrom rlang .data abort
#' 
extract_taxa <- function(austraits, family = NULL, genus = NULL, taxon_name = NULL){
  
  ret <- austraits
  
  if(missing(family) & missing(genus) & missing(taxon_name)){
    abort("Either `family`, `genus` or `taxon_name`, must be supplied!")
  }
  
  if(! missing(family) & ! missing(genus)){
    abort("Can extract one level of taxa at a time! - supply either `family` or `genus` or `taxon_name`")
  }
  
  if( ! is.null(family) ){
    # Retrieving all taxon name that falls under family
    target_in <- stringr::str_which(austraits$taxa$family, paste(family, collapse = "|"))
    target_taxa <- austraits$taxa %>% dplyr::slice(target_in) %>% dplyr::pull(.data$taxon_name) 
  }
  
  if( ! is.null(genus) ){
    # Retrieving all taxon name that falls under genus
    target_in <- stringr::str_which(austraits$taxa$genus, paste(genus, collapse = "|"))
    target_taxa <- austraits$taxa %>% dplyr::slice(target_in) %>% dplyr::pull(.data$taxon_name) 
  }
  
  if( ! is.null(taxon_name)){
    target_taxa <- taxon_name
  }
  
  # Extract data for target_sp
  ret[["traits"]] <- ret[["traits"]] %>% 
    dplyr::filter(.data$taxon_name %in% target_taxa)
  
  ids <- ret[["traits"]][["dataset_id"]] %>% unique() %>% sort()
  
  ret[["sites"]] <- austraits[["sites"]] %>% dplyr::filter(.data$site_name %in% ret[["traits"]][["site_name"]], .data$dataset_id %in% ids)
  
  ret[["contexts"]] <- austraits[["contexts"]] %>% dplyr::filter(.data$context_name %in% ret[["traits"]][["context_name"]], .data$dataset_id %in% ids)
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(.data$taxon_name %in% ret[["traits"]][["taxon_name"]])
  
  ret[["taxonomic_updates"]] <- austraits[["taxonomic_updates"]] %>% dplyr::filter(.data$taxon_name %in% ret[["traits"]][["taxon_name"]])
  # Fix formatting for dataset ids
  ret$taxonomic_updates <-
    tidyr::separate_rows(austraits$taxonomic_updates, dataset_id, sep=" ")
  
  ret[["excluded_data"]] <- austraits[["excluded_data"]] %>% dplyr::filter(.data$taxon_name %in% target_taxa)
  
  ret[["contributors"]] <- austraits[["contributors"]] %>% dplyr::filter(.data$dataset_id %in% ids)
  
  ret[["methods"]] <- austraits[["methods"]] %>% dplyr::filter(.data$dataset_id %in%ids)
  
  ret[["definitions"]] <- austraits[["definitions"]]
  
  ret[["build_info"]] <- austraits[["build_info"]]
  
  # if numeric, convert to numeric
  suppressWarnings(
    ret[["traits"]][["value"]] <- ifelse(! is.na(ret[["traits"]][["unit"]]), 
                                       as.numeric(ret[["traits"]][["value"]]),
                                       ret[["traits"]][["value"]])
    
  )
  
  # Assign class
  attr(ret, "class") <- "austraits"
  
  ret
}

  


  
  
  
  
  
 