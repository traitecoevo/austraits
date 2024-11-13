#' @title Generic for outputting a nice summary for austraits objects
#'
#' @name print.austraits
#' @param database traits.build database
#' @param \dots passed to print
#' @keywords internal
#' @return nicely printed table
#' @export

print.austraits <- function(database, ...){
  
  # Setting up
  version <- database$build_info$version %>% as.character()
  nrecords <- nrow(database$traits)
  nspecies <- unique(database$traits$taxon_name) %>% length()
  ntraits <- unique(database$traits$trait_name) %>% length()
  
  cat("This is version",
         version, 
         "of austraits!\n", 
         "\nThis object contains a total of",
         nrecords, "records",
         "for", nspecies, "taxa and",
         ntraits, "traits.\n")
  
  if(package_version(version) <= '3.0.2'){
  
  cat("\nThis object is a 'list' with the following components:\n\n",
  database$definitions$austraits$elements %>%
  purrr::map(~.x[["description"]]) %>% 
  as.vector() %>% 
  sprintf("- `%s`: %s", names(.), .) %>%
  paste(collapse="\n")
  )
  } else{
    cat("\nThis object is a 'list' with the following components:\n\n",
        database$schema$austraits$elements %>%
          purrr::map(~.x[["description"]]) %>% 
          as.vector() %>% 
          sprintf("- `%s`: %s", names(.), .) %>%
          paste(collapse="\n")
    )
  }

  
  cat("\n\nTo access a component, try using the $ e.g. austraits$traits")  
}


