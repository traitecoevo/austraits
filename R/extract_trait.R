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
#' @importFrom rlang .data

extract_trait <- function(austraits, trait_names, taxon_names=NULL) {
  
  ret <- austraits
  
  ret[["traits"]] <- austraits[["traits"]] %>% 
    dplyr::filter(.data$trait_name %in% trait_names)
  
  if(!is.null(taxon_names))
    ret[["traits"]] <- ret[["traits"]] %>% 
    dplyr::filter(.data$taxon_name %in% taxon_names)
  
  ids <- ret[["traits"]][["dataset_id"]] %>% unique() %>% sort()
  
  ret[["sites"]] <- austraits[["sites"]] %>% dplyr::filter(.data$site_name %in% ret[["traits"]][["site_name"]], .data$dataset_id %in% ids)
  
  ret[["contexts"]] <- austraits[["contexts"]] %>% dplyr::filter(.data$context_name %in% ret[["traits"]][["context_name"]], .data$dataset_id %in% ids)
  
  ret[["taxa"]] <- austraits[["taxa"]] %>% dplyr::filter(.data$taxon_name %in% ret[["traits"]][["taxon_name"]])
  ret[["taxonomic_updates"]] <- austraits[["taxonomic_updates"]] %>% dplyr::filter(.data$taxon_name %in% ret[["traits"]][["taxon_name"]])
  
  ret[["excluded_data"]] <- austraits[["excluded_data"]][austraits[["excluded_data"]][["trait_name"]] %in% trait_names,]
  
  if(!is.null(taxon_names))
    ret[["excluded_data"]] <- ret[["excluded_data"]] %>% dplyr::filter(.data$taxon_name %in% taxon_names)
  
  
  ret[["contributors"]] <- austraits[["contributors"]] %>% dplyr::filter(.data$dataset_id %in% ids)
  
  ret[["methods"]] <- austraits[["methods"]] %>% dplyr::filter(.data$dataset_id %in%ids, .data$trait_name %in% ret[["traits"]][["trait_name"]])
  
  ret[["definitions"]] <- austraits[["definitions"]]
  ret[["build_info"]] <- austraits[["build_info"]]
  
  # if numeric, convert to numeric
  if(!is.na(ret[["traits"]][["unit"]][1])){
    ret[["traits"]][["value"]] <- as.numeric(ret[["traits"]][["value"]])
  }
  
  
  keys <- dplyr::union(ret$methods$source_primary_key, 
                       ret$methods$source_secondary_key) %>% 
    unique() %>% stats::na.omit() %>% as.character()

  keys <- keys[!keys == ""] #Omit empty strings
  
  ret[["sources"]] <- austraits$sources[keys]
  
  ret[names(austraits)]
}
