#' @title Extract data for specific traits
#'
#' @description Function to subset of all data associated with a particular dataset from austraits
#' @usage extract_trait(austraits, trait_names, taxon_names)
#' @param austraits - A large list of tibbles built from austraits
#' @param trait_names - character string of trait that will be extracted
#' @param taxon_names - optional argument
#' @return A large list of tibbles containing all austraits information for one particular dataset
#'
#' @examples 
#' \dontrun{
#'extract_trait(austraits, "wood_density", taxon_name = "Acacia celsa")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export



extract_trait <- function(austraits, trait_names, taxon_names=NULL) {
  # Switch for different versions
  version <- what_version(austraits)
  
  if(what_version(austraits) %in% c("4-series", "5-series")){
    version <- "new" 
  } else
    version <- "old"
    
  switch (version,
          'new' = extract_trait2(austraits, trait_names, taxon_names),
          'old' = extract_trait1(austraits, trait_names, taxon_names),
  )
}

#' @title Extract specific trait data from austraits object for versions <=3.0.2
#' @noRd
#' @keywords internal
extract_trait1 <- function(austraits, trait_names, taxon_names=NULL) {
  
  ret <- austraits
  
  ret[["traits"]] <- austraits[["traits"]] %>% 
    dplyr::filter(trait_name %in% trait_names)
  
  if(!is.null(taxon_names)){
    ret[["traits"]] <- ret[["traits"]] %>% 
    dplyr::filter(taxon_name %in% taxon_names)
  }
  
  ids <- ret[["traits"]][["dataset_id"]] %>% unique() %>% sort()
  
  ret[["sites"]] <- austraits[["sites"]] %>% dplyr::filter(site_name %in% ret[["traits"]][["site_name"]], dataset_id %in% ids)
  
  ret[["contexts"]] <- austraits[["contexts"]] %>% dplyr::filter(context_name %in% ret[["traits"]][["context_name"]], dataset_id %in% ids)
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]])
  ret[["taxonomic_updates"]] <- austraits[["taxonomic_updates"]] %>% dplyr::filter(taxon_name %in% ret[["traits"]][["taxon_name"]])
  
  ret$taxonomic_updates <-
    tidyr::separate_rows(austraits$taxonomic_updates, dataset_id, sep=" ")
  
  
  ret[["excluded_data"]] <- austraits[["excluded_data"]][austraits[["excluded_data"]][["trait_name"]] %in% trait_names,]
  
  if(!is.null(taxon_names))
    ret[["excluded_data"]] <- ret[["excluded_data"]] %>% dplyr::filter(taxon_name %in% taxon_names)
  
  
  ret[["contributors"]] <- austraits[["contributors"]] %>% dplyr::filter(dataset_id %in% ids)
  
  ret[["methods"]] <- austraits[["methods"]] %>% dplyr::filter(dataset_id %in%ids, trait_name %in% ret[["traits"]][["trait_name"]])
  
  ret[["definitions"]] <- austraits[["definitions"]]
  ret[["build_info"]] <- austraits[["build_info"]]
  
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
