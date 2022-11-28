#' Look up a particular trait term
#'
#' @param austraits austraits list
#' @param term character string for trait search term 
#'
#' @return vector containing traits that contains search term
#' @export
#'
#' @examples
#' \dontrun{
#' austraits %>% lookup_trait("leaf") %>% extract_trait(austraits, .)
#' }
lookup_trait <- function(austraits, term){
  
  all_traits <- austraits$traits$trait_name %>% unique()

   ret <-  stringr::str_subset(all_traits, term)
  
  if(length(ret) == 0){
    stop(paste0("No traits found containing ", term, " !"))
  }
   
   ret
}
