#' Summarise counts for a particular variable of interest
#'
#' @param austraits A large list of tibbles built from austraits
#' @param var variable you use wish to see summary of
#'
#' @return dataframe of unique levels of variable with counts and percentage
#' @export
#' @examples
#' \dontrun{
#' print_austraits(austraits, "trait_name") %>% head()
#' }

print_austraits <- function(austraits, var){
  switch(var,
         trait_name = print_austraits_traits(austraits, var),
         dataset_id = print_austraits_traits(austraits, var),
         genus =  print_austraits_taxa(austraits, var),
         family = print_austraits_taxa(austraits, var)
  )
}
         
         

#' @rdname print_austraits
         
print_austraits_traits <-function(austraits, var) {
  
  
  ret <- 
    austraits[["traits"]] %>% 
    dplyr::pull({{var}}) %>% 
    sort() %>% 
    janitor::tabyl() 
  
  names(ret)[1] <- var
  
  ret
}

#' @rdname print_austraits

print_austraits_taxa <-function(austraits, var) {
  ret <- 
    austraits[["taxa"]] %>% 
    dplyr::pull({{var}}) %>% 
    sort() %>% 
    janitor::tabyl() 
  
  names(ret)[1] <- var
  
  ret
}




