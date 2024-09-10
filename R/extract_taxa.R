#' @title Extract all data for specific taxa
#'
#' @description Function to subset of all data associated with a particular taxon from a traits.build relational database.
#' 
#' @param austraits a large list of tibbles built by `traits.build` workflow
#' @param family character string of family or families
#' @param genus character string of genus or genera
#' @param taxon_name character string of taxon name(s)
#' @return List of tibbles containing all traits.build data and metadata for the specified taxa.
#' @details
#' `extract_taxa` has been developed to extract data for specific taxa from databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build] &
#'   [https://github.com/traitecoevo/traits.build-book]
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits] for how to install old versions of the package or download a newer version of the database."
#'
#' @examples 
#' \dontrun{
#'extract_taxa(austraits, family = "Proteaceae")
#'extract_taxa(austraits, genus = "Acacia")
#' }
#' @author Fonti Kar - f.kar@unsw.edu.au
#' @export

extract_taxa <- function(austraits, family = NULL, genus = NULL, taxon_name = NULL){
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  } 
  extract_taxa2(austraits, family, genus, taxon_name)
}


#'Extract taxa for supported versions of databases
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

  


  
  
  
  
  
 