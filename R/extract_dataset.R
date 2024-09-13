#' @title Extract all data for a particular dataset
#'
#' @description Function to subset all data associated with a particular dataset from a traits.build relational database.
#' 
#' @usage extract_dataset(austraits, dataset_id)
#' @param austraits a large list of tibbles built by `traits.build` workflow
#' @param dataset_id character string that matches a `dataset_id` in the database
#' @return List of tibbles containing all traits.build data and metadata for the specified dataset(s).
#' @details
#' `extract_dataset` has been developed to extract data for specific datasets from databases built using the traits.build workflow. 
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
#' extract_dataset(austraits, "Falster_2003")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export


extract_dataset <- function(austraits, dataset_id) {
  
  # Check compatability
  status <- check_compatibility(austraits)

  # If compatible
  if(!status){
    function_not_supported(austraits)
  }
  
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


