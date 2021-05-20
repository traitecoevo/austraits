#' Join site and other details into main traits `traits` dataset
#'
#' @param austraits
#'
#' @return austraits list object, but with additonal details appended to `traits`
##' @rdname join_all
#' @examples
#' \dontrun{
#' austraits$traits
#' 
#' #fetch sites
#' (austraits %>% join_sites)$traits
#'
#' #fetch contexts
#' (austraits %>% join_contexts)$traits
#' 
#' # fetch methods
#' (austraits %>% join_methods)$traits
#' 
#' #fetch taxonomic details
#' (austraits %>% join_taxonomy)$traits
#' 
#' # join all
#' (austraits %>% join_all)$traits
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export

join_all <- function(austraits) {
  austraits %>% 
    join_sites() %>% 
    join_taxonomy() %>% 
    join_methods()
}

##' @export
##' @rdname join_all
join_taxonomy <- function(austraits, vars =  c("family", "genus", "taxonRank", "acceptedNameUsageID")) {
  austraits$traits <- austraits$traits %>%
    left_join(by="taxon_name", austraits$taxa %>% select("taxon_name", one_of(vars)))
  
  austraits
}

##' @export
##' @rdname join_all
join_methods <- function(austraits, vars =  names(austraits$methods)){
  austraits$traits <- austraits$traits %>%
    left_join(by=c("dataset_id", "trait_name"), austraits$methods %>% select(c("dataset_id", "trait_name"), one_of(vars)))
  
  austraits
}

##' @export
##' @rdname join_all
join_sites <- function(austraits, vars =  c("longitude (deg)","latitude (deg)")) {
  sites <- 
    austraits$sites %>% 
    filter(site_property %in%  vars) %>% 
    pivot_wider(names_from = site_property, values_from = value)
  
  austraits$traits <- austraits$traits %>%
    left_join(by=c("dataset_id", "site_name"), sites)
  
  austraits
}


##' @export
##' @rdname join_all
join_contexts <- function(austraits, vars =  c()) {
  
  
  if(nrow(austraits$contexts) == 0)
    return (austraits)
  
  contexts <- 
    austraits$contexts %>% 
    filter(context_property %in%  vars) %>% 
    pivot_wider(names_from = context_property, values_from = value)
  
  austraits$traits <- austraits$traits %>%
    left_join(by=c("dataset_id", "context_name"), contexts)
  
  austraits
}
