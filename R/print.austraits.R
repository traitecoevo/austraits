#' @title Generic for outputting a nice summary for austraits objects
#'
#' @name print.traits.build
#' @param database traits.build database
#' @param \dots passed to print
#'
#' @return nicely printed table
#' @export

print.traits.build <- function(database, ...){
  # browser()
  
  # Setting up
  version <- database$build_info$version %>% as.character()
  nrecords <- nrow(database$traits)
  nspecies <- unique(database$traits$taxon_name) %>% length()
  ntraits <- unique(database$traits$trait_name) %>% length()
  
  database_name <- database$metadata$title
  
  traits.build_version <- at_six$metadata$related_identifiers |> 
    convert_list_to_df2() |> 
    dplyr::filter(resource_type == "software") |> 
    dplyr::pull(version)
  
  # Formulate message
  cli::cli_h1("This is {database_name}!")
  
  cli::cli_bullets(c(
    "i" = "A database built using traits.build version {traits.build_version}",
    "i" = "This database contains a total of {nrecords} records, for {nspecies} taxa and {ntraits} traits."
  )
  )
  
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


