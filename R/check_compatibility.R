#' @title Check compatibility of traits.build object
#' @description Function to check whether the data object has been compiled by the traits.build workflow and 
#' therefore has a data structure that is appropriate for use with austraits functions.
#' @param database traits.build database (list object)
#' @param single_table_allowed logical for when the input might be a single table instead of a complete database; defaults to FALSE
#'
#' @return logical (T/F) output and messaging for uncompatible versions
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' check_compatibility(database)
#' }
#' @author Elizabeth Wenk - e.wenk@unsw.edu.au

check_compatibility <- function(database, single_table_allowed = FALSE) {
  
  if (!is.null(dim(database)) & single_table_allowed == TRUE) {
    
    compatible <- TRUE
    
  } else {
    
    if (is.null(database$metadata)) {
      
      compatible <- FALSE
      
      # message("You are working with AusTraits version 3.0 or earlier. \nThis database structure is unsupported by the current version of this package. \nPlease see https://github.com/traitecoevo/austraits for details on installing old versions of the package.")
      
    } else {
      
      compiled_by_traits.build <-
        get_compiled_by_traits.build(database)
      
      if(is.null(compiled_by_traits.build) | nrow(compiled_by_traits.build) > 0) {
        compatible <- TRUE
      } else{
        compatible <- FALSE
        
        # message("You are working with AusTraits version 4, which is unsupported by the current version of this package. \nPlease see https://github.com/traitecoevo/austraits for details on installing old versions of the package.")
      }
      
    }
    
  }
  
  invisible(compatible)
  
}




#' Check compatibility of traits table
#'
#' @param trait_data the traits table in a traits.build database
#' @keywords internal
#' @return logical, TRUE indicating version traits table came from traits.build version > 1.0

check_traits_compatibility <- function(trait_data){
  # Check compatibility using column 
  if(any(names(trait_data) %in% c("treatment_context_id", "repeat_measurements_id"))){
    compatible <- TRUE
  } else
    compatible <- FALSE
  
  invisible(compatible)
}


#' Retrieve compiled by information from metadata table
#'
#' @param database traits.build database
#'
#' @return logical, TRUE indicating version traits table came from traits.build version > 1.0

get_compiled_by_traits.build <- function(database){
  database$metadata$related_identifiers %>% 
  convert_list_to_df2() %>%
  dplyr::filter(relation_type == "isCompiledBy") %>% 
  dplyr::filter(stringr::str_detect(identifier, "github.com/traitecoevo/traits.build"))
}