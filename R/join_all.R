#' @title Join metadata fields into the core traits table
#' @description Function to merge metadata stored in relational tables into the core traits table.
#'
#' A traits.build database includes relational tables that contain measurement (and observation) metadata pertaining to locations, contexts, methods, and taxonomy. This function merges all (or some) of the metadata from one (or all) of these tables into the traits table.
#' 
#' @param austraits traits.build generated database
#' @param vars vector specifying which columns from a specific relational table to join to the traits table (works only for `join_` functions joining a single dataframe)
#' @param collapse_context for `join_context_properties` and `join_location_properties` only. Logical, whether location properties or context properties for a given observation will be concatenated into one cell
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
#' #Append locations properties and output a single table
#' (austraits %>% join_locations)$traits
#' 
#' #Append location properties, but maintain the relational database
#' austraits %>% join_locations()
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
  
  vars <- as.vector(vars)
  
  if (vars[1] == "all" & length(vars == 1)){
    vars <- names(austraits$taxa)
  }
  
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by="taxon_name", austraits$taxa %>% dplyr::select("taxon_name", tidyselect::any_of(vars)))
  
  austraits
}

#' @title Joining methodological information to traits table

#' @export
#' @rdname join_all

join_methods <- function(austraits, vars =  c("methods")) {
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  }

  if (vars == "all"){
    vars <- names(austraits$methods)
  }

  austraits$methods %>% 
    dplyr::select(c("dataset_id", "trait_name", "method_id"), tidyselect::any_of(vars)) %>% 
    dplyr::distinct() -> methods
  
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "trait_name", "method_id"),
                     methods, relationship = "many-to-many")
  
  austraits
}

#' @title Joining location coordinates to traits table
#' @export

#' @rdname join_all
join_location_coordinates <- function(austraits) {
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  }

  location_coordinates <-
    database$locations %>%
    dplyr::filter(location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    tidyr::pivot_wider(names_from = location_property, values_from = value)

  if (any(stringr::str_detect(names(location_coordinates), "latitude "))) {
    austraits$traits <- austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "location_id"), location_coordinates)

  } else {
    austraits$traits <- austraits$traits %>%
      dplyr::mutate(
        location_name = NA_character_,
        `latitude (deg)` = NA_character_,
        `longitude (deg)` = NA_character_,
      )
  }

  austraits
}

#' @title Joining location properties to traits table
#' @export

#' @rdname join_all

join_location_properties <- function(austraits, vars =  "all") {
  # Check compatability
  status <- check_compatibility(austraits)

  # If compatible
  if(!status){
    function_not_supported(austraits)
  }
  
  if (vars == "all") {
    austraits$locations %>%
    distinct(location_properties) %>%
    filter(!location_properties %in% "latitude (deg)", "longitude (deg)")
  }

  sites <- 
    austraits$locations %>% 
    dplyr::filter(location_property %in%  vars) %>% 
    tidyr::pivot_wider(names_from = location_property)
  
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "location_id"), sites)
  
  austraits
}



