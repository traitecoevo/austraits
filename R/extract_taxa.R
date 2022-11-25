#' @title Extract data for one specific taxa
#'
#' @description Function to subset of all data associated with a particular dataset from austraits
#' @param austraits austraits list object
#' @param family character string of family
#' @param genus character string of genus
#' @param taxon_name character string of taxon name
#' @return A large list of tibbles containing all austraits information for specificied taxa
#'
#' @examples 
#' \dontrun{
#'extract_taxa(austraits, family = "Proteaceae")
#'extract_taxa(austraits, genus = "Acacia")
#' }
#' @author Fonti Kar - f.kar@unsw.edu.au
#' @export

extract_taxa <- function(austraits, family = NULL, genus = NULL, taxon_name = NULL){
  # Switch for different versions
  version <- austraits$build_info$version %>% as.character()
  
  switch (version,
          '3.0.2.9000' = extract_taxa2(austraits,  family, genus, taxon_name),
          '3.0.2' = extract_taxa1(austraits, family, genus, taxon_name),
          '3.0.1' = extract_taxa1(austraits, family, genus, taxon_name),
          '3.0.0' = extract_taxa1(austraits, family, genus, taxon_name),
          '2.1.0' = extract_taxa1(austraits, family, genus, taxon_name),
          '2.0.0' = extract_taxa1(austraits, family, genus, taxon_name)
  )
}


#'Extract taxa >3.0.2
#' @noRd
#' @keywords internal
extract_taxa2 <- function(austraits, family = NULL, genus = NULL, taxon_name = NULL){
  
  ret <- austraits
  
  if(missing(family) & missing(genus) & missing(taxon_name)){
    abort("Either `family`, `genus` or `taxon_name`, must be supplied!")
  }
  
  if( ! is.null(family) ){
    # Retrieving all taxon name that falls under family
    target_in <- stringr::str_which(austraits$taxa$family, paste(family, collapse = "|"))
    target_taxa <- austraits$taxa %>% dplyr::slice(target_in) %>% dplyr::pull(taxon_name) 
  }
  
  if( ! is.null(genus) ){
    # Retrieving all taxon name that falls under genus
    target_in <- stringr::str_which(austraits$taxa$genus, paste(genus, collapse = "|"))
    target_taxa <- austraits$taxa %>% dplyr::slice(target_in) %>% dplyr::pull(taxon_name) 
  }
  
  if( ! is.null(taxon_name)){
    target_taxa <- taxon_name
  }
  
  # Extract data for target_sp
  ret[["traits"]] <- ret[["traits"]] %>% 
    dplyr::filter(taxon_name %in% target_taxa)
  
  dataset_id <- ret[["traits"]][["dataset_id"]] %>% unique() %>% sort()
  
  # Dataset specific tables
  for(v in c("locations", "contexts", "contributors", "methods")){
    ret[[v]] <- austraits[[v]][ austraits[[v]][["dataset_id"]] %in% dataset_id,]
  }
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(taxon_name %in% target_taxa)
  
  ret[["taxonomic_updates"]] <- austraits[["taxonomic_updates"]] %>% dplyr::filter(taxon_name %in% target_taxa)
  # Fix formatting for dataset ids
  ret$taxonomic_updates <-
    tidyr::separate_rows(austraits$taxonomic_updates, dataset_id, sep=" ")
  
  ret[["excluded_data"]] <- austraits[["excluded_data"]] %>% dplyr::filter(taxon_name %in% target_taxa)

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

  
#'Extract taxa <=3.0.2
#' @noRd
#' @keywords internal
extract_taxa1 <- function(austraits, family = NULL, genus = NULL, taxon_name = NULL){
  
  ret <- austraits
  
  if(missing(family) & missing(genus) & missing(taxon_name)){
    abort("Either `family`, `genus` or `taxon_name`, must be supplied!")
  }
  
  if( ! is.null(family) ){
    # Retrieving all taxon name that falls under family
    target_in <- stringr::str_which(austraits$taxa$family, paste(family, collapse = "|"))
    target_taxa <- austraits$taxa %>% dplyr::slice(target_in) %>% dplyr::pull(taxon_name) 
  }
  
  if( ! is.null(genus) ){
    # Retrieving all taxon name that falls under genus
    target_in <- stringr::str_which(austraits$taxa$genus, paste(genus, collapse = "|"))
    target_taxa <- austraits$taxa %>% dplyr::slice(target_in) %>% dplyr::pull(taxon_name) 
  }
  
  if( ! is.null(taxon_name)){
    target_taxa <- taxon_name
  }
  
  # Extract data for target_sp
  ret[["traits"]] <- ret[["traits"]] %>% 
    dplyr::filter(taxon_name %in% target_taxa)
  
  ids <- ret[["traits"]][["dataset_id"]] %>% unique() %>% sort()
  
  ret[["sites"]] <- austraits[["sites"]] %>% dplyr::filter(site_name %in% ret[["traits"]][["site_name"]], dataset_id %in% ids)
  
  ret[["contexts"]] <- austraits[["contexts"]] %>% dplyr::filter(context_name %in% ret[["traits"]][["context_name"]], dataset_id %in% ids)
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]])
  
  ret[["taxonomic_updates"]] <- austraits[["taxonomic_updates"]] %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]])
  # Fix formatting for dataset ids
  ret$taxonomic_updates <-
    tidyr::separate_rows(austraits$taxonomic_updates, dataset_id, sep=" ")
  
  ret[["excluded_data"]] <- austraits[["excluded_data"]] %>% dplyr::filter(taxon_name %in% target_taxa)
  
  ret[["contributors"]] <- austraits[["contributors"]] %>% dplyr::filter(dataset_id %in% ids)
  
  ret[["methods"]] <- austraits[["methods"]] %>% dplyr::filter(dataset_id %in%ids)
  
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

  


  
  
  
  
  
 