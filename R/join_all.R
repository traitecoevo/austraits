#' @title Join metadata fields into the core traits table
#' @description Function to merge metadata stored in relational tables into the core traits table.
#'
#' A traits.build database includes relational tables that contain measurement (and observation) metadata pertaining to locations, contexts, methods, and taxonomy. This function merges all (or some) of the metadata from one (or all) of these tables into the traits table.
#' 
#' @param austraits traits.build generated database
#' @param vars vector specifying which columns from a specific relational table to join to the traits table (works only for `join_` functions joining a single dataframe)
#' @param collapse_context for `join_contexts` only. Logical, whether contexts for a given observation will be concatenated into one cell
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build] &
#'   [https://github.com/traitecoevo/traits.build-book]
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits] for how to install old versions of the package or download a newer version of the database."
#'
#' @rdname join_all

#' @examples
#' \dontrun{
#' austraits$traits
#' 
#' #Append locations data
#' (austraits %>% join_locations)$traits
#'
#' #Append contexts
#' (austraits %>% join_contexts)$traits
#' 
#' # Append methods
#' (austraits %>% join_methods(vars = c("method_id")))$traits
#' 
#' #Append taxonomic details
#' (austraits %>% join_taxonomy)$traits
#' 
#' #Append all information
#' (austraits %>% join_all)$traits
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export


join_all <- function(austraits) {
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  }
  austraits %>% 
    join_locations() %>% 
    join_taxonomy() %>% 
    join_methods()
}

#' @title Joining taxonomic information to traits table
#' @export

#' @rdname join_all

join_taxonomy <- function(austraits, vars =  c("family", "genus", "taxon_rank", "establishment_means")) {
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  } 
  
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by="taxon_name", austraits$taxa %>% dplyr::select("taxon_name", tidyselect::any_of(vars)))
  
  austraits
}

#' @title Joining methodological information to traits table

#' @export
#' @rdname join_all

join_methods <- function(austraits, vars =  c("methods", "year_collected_start", "year_collected_end", "collection_type")) {
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  } 

  austraits$methods %>% 
    dplyr::select(c("dataset_id", "trait_name", "method_id"), tidyselect::any_of(vars)) %>% 
    dplyr::distinct() -> methods
  
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "trait_name", "method_id"),
                     methods, relationship = "many-to-many")
  
  austraits
}

#' @title Joining location information to traits table
#' @export

#' @rdname join_all

join_locations <- function(austraits, vars =  c("longitude (deg)","latitude (deg)")) {
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  } 
  
  sites <- 
    austraits$locations %>% 
    dplyr::filter(location_property %in%  vars) %>% 
    tidyr::pivot_wider(names_from = location_property)
  
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "location_id"), sites)
  
  austraits
}

#' @title  Joining location info for AusTraits versions <= 3.0.2
#' @description `r lifecycle::badge('deprecated')`
#' Joining location info for AusTraits versions <= 3.0.2
#' @param austraits austraits object
#' @param vars  variables from site table to join 
#' @export

join_sites <- function(austraits, vars =  c("longitude (deg)","latitude (deg)")) {
  function_not_supported(austraits)
}

#' @export
#' @rdname join_all

join_contexts <- function(austraits, collapse_context = FALSE){
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  } 

  traits2 <- split(austraits$traits, austraits$traits$dataset_id)
  contexts2 <- split(austraits$contexts, austraits$contexts$dataset_id)
  
  traits_vars <- names(austraits$traits)

  problem_studies <- c("Hall_1981")

  for(id in names(traits2)) {
  
    if(!is.null(contexts2[[id]][1]) & ! (id %in% problem_studies)) {
    
      context_ids <- 
        unique(contexts2[[id]]$link_id)
  
      for(v in context_ids[!is.na(context_ids)]) {
        
        context_sub <- 
          contexts2[[id]] %>%
          dplyr::select(-dplyr::any_of(c("category", "description"))) %>%
          dplyr::filter(link_id == v) %>% 
          tidyr::separate_rows(link_vals) %>% 
          tidyr::pivot_wider(values_from = value, names_from = context_property) %>%
          tidyr::pivot_wider(names_from = link_id, values_from = link_vals)
        
        traits2[[id]] <- 
          dplyr::left_join(by = c("dataset_id", v),
                    traits2[[id]],
                    context_sub
          )
      }
      
      if(collapse_context == TRUE){
        context_text <-
          traits2[[id]] %>% 
          dplyr::select(-dplyr::any_of(traits_vars)) %>% collapse_cols()

        traits2[[id]] <- traits2[[id]] %>% 
          dplyr::mutate(context = context_text) %>% 
          dplyr::select(dplyr::any_of(traits_vars), context)
      }
    }
  }

  austraits$traits <- traits2 %>% dplyr::bind_rows()

  austraits
}


