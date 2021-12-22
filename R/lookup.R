#' Look up a particular trait term
#'
#' @param austraits austraits list
#' @param term search term 
#'
#' @return vector containing traits that contains search term
#' @export
#'
#' @examples
#' \dontrun{
#' austraits %>% lookup_trait(leaf) %>% extract_trait(austraits, .)
#' }
lookup_trait <- function(austraits, term){
  
  target <- rlang::enquo(term)
  
  all_traits <- austraits$traits$trait_name %>% unique()

  all_traits[stringr::str_detect(all_traits, rlang::as_label(target))]
}
