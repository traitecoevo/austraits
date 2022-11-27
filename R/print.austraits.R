#' @title Generic for outputting a nice summary for austraits objects
#'
#' @name print.austraits
#' @param x austraits list object 
#' @param \dots passed to print
#'
#' @return nicely printed table
#' @export

print.austraits <- function(x, ...){
  
  # Setting up
  version <- x$build_info$version %>% as.character()
  nrecords <- nrow(x$traits)
  nspecies <- unique(x$traits$taxon_name) %>% length()
  ntraits <- unique(x$traits$trait_name) %>% length()
  
  cat("This is version",
         version, 
         "of austraits!\n", 
         "\nThis object contains a total of",
         nrecords, "records",
         "for", nspecies, "taxa and",
         ntraits, "traits.\n")
  
  if(package_version(version) <= '3.0.2'){
  
  cat("\nThis object is a 'list' with the following components:\n\n",
  x$definitions$austraits$elements %>%
  purrr::map(~.x[["description"]]) %>% 
  as.vector() %>% 
  sprintf("- `%s`: %s", names(.), .) %>%
  paste(collapse="\n")
  )
  } else{
    cat("\nThis object is a 'list' with the following components:\n\n",
        x$schema$austraits$elements %>%
          purrr::map(~.x[["description"]]) %>% 
          as.vector() %>% 
          sprintf("- `%s`: %s", names(.), .) %>%
          paste(collapse="\n")
    )
  }

  
  cat("\n\nTo access a component, try using the $ e.g. austraits$traits")  
}


