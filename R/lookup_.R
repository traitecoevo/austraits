#' Look up a particular trait term
#'
#' @param database traits.build database (list object)
#' @param term character string for trait search term 
#'
#' @return vector containing traits that contains search term
#' @export
#'
#' @examples
#' \dontrun{
#' austraits %>% lookup_trait("leaf") %>% extract_trait(database = austraits, .)
#' }
lookup_trait <- function(database, term){
  
  all_traits <- database$traits$trait_name %>% unique()

   ret <-  stringr::str_subset(all_traits, term)
  
  if(length(ret) == 0){
    stop(paste0("No traits found containing ", term, " !"))
  }
   
   ret
}


#' Look up location properties
#' 
#' @description
#' Look up location properties that contain a specific search term.
#' 
#'
#' @param database traits.build database (list object)
#' @param term character string for location property search term 
#'
#' @return vector containing location properties that contains search term
#' @export
#'
#' @examples
#' \dontrun{
#' austraits %>% lookup_location_property("soil")
#' }
lookup_location_property <- function(database, term){
  
  all_location_properties <- database$locations$location_property %>% unique()
  
  ret <-  stringr::str_subset(all_location_properties, term)
  
  if(length(ret) == 0){
    stop(paste0("No location properties found containing ", term, " !"))
  }
  
  ret
}


#' Look up context properties
#' 
#' @description
#' Look up context properties that contain a specific search term.
#' 
#'
#' @param database traits.build database (list object)
#' @param term character string for context property search term 
#'
#' @return vector containing context properties that contains search term
#' @export
#'
#' @examples
#' \dontrun{
#' austraits %>% lookup_context_property("temperature")
#' }
lookup_context_property <- function(database, term){
  
  all_context_properties <- database$contexts$context_property %>% unique()
  
  ret <-  stringr::str_subset(all_context_properties, term)
  
  if(length(ret) == 0){
    stop(paste0("No context properties found containing ", term, " !"))
  }
  
  ret
}
