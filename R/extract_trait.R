#' @title Extract all data for specific traits
#'
#' @description Function to subset all data associated with a particular trait from a traits.build relational database.
#' 
#' @usage extract_trait(austraits, trait_names, taxon_names)
#' @param austraits a large list of tibbles built by `traits.build`
#' @param trait_names character string of trait(s) for which data will be extracted
#' @param taxon_names optional argument, specifying taxa for which data will be extracted 
#' @return List of tibbles containing all traits.build data and metadata for the specified trait(s).
#' @details
#' `extract_trait` has been developed to extract data for specific traits from databases built using the traits.build workflow. 
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
#'extract_trait(austraits, "wood_density", taxon_name = "Acacia celsa")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export



extract_trait <- function(austraits, trait_names, taxon_names=NULL) {
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  } 
  extract_trait2(austraits, trait_names, taxon_names)
    
}


#' @title Extract specific trait data from austraits object for versions >3.0.2
#' @noRd
#' @keywords internal
extract_trait2 <- function(austraits, trait_names, taxon_names=NULL) {
  
  ret <- austraits
  
  # Traits table
  ret[["traits"]] <- austraits[["traits"]] %>% 
    dplyr::filter(trait_name %in% trait_names)
  
  # If taxon_name supplied, further filter traits table
  if(!is.null(taxon_names)){
    ret[["traits"]] <- ret[["traits"]] %>% 
    dplyr::filter(taxon_name %in% taxon_names)
  }
  
  dataset_id <- ret[["traits"]][["dataset_id"]] %>% unique() %>% sort()
  
  # Dataset specific tables
  for(v in c("locations", "contexts", "contributors")){
    ret[[v]] <- austraits[[v]][ austraits[[v]][["dataset_id"]] %in% dataset_id,]
  }
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]])
  ret[["taxonomic_updates"]] <- austraits[["taxonomic_updates"]] %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]])
  
  # Fix formating for datasets
  ret$taxonomic_updates <-
    tidyr::separate_rows(austraits$taxonomic_updates, dataset_id, sep=" ")
  
  ret[["excluded_data"]] <- austraits[["excluded_data"]]  %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]], trait_name %in% trait_names)

  ret[["methods"]] <- austraits[["methods"]] %>% dplyr::filter(dataset_id %in% dataset_id, trait_name %in% trait_names)
  
  # Tables that never change
  ret[["definitions"]] <- austraits[["definitions"]]
  ret[["build_info"]] <- austraits[["build_info"]]
  ret[["schema"]] <- austraits[["schema"]]
  
  # if numeric, convert to numeric
  if(!is.na(ret[["traits"]][["unit"]][1])){
    suppressWarnings(ret[["traits"]][["value"]] <- as.numeric(ret[["traits"]][["value"]]))
  }
  
  
  keys <- dplyr::union(ret$methods$source_primary_key, 
                       ret$methods$source_secondary_key %>% strsplit("; ") %>% unlist()) %>% 
    unique() %>% stats::na.omit() %>% as.character()
  
  ret[["sources"]] <- austraits$sources[keys]
  
  ret[names(austraits)]
  
  # Assign class
  attr(ret, "class") <- "austraits"
  
  ret
}

#' @title Extract specific trait data from austraits object for versions <=3.0.2
#' @noRd
#' @keywords internal
extract_trait1 <- function(austraits, trait_names, taxon_names=NULL) {
  
  function_not_supported(austraits)
  
}