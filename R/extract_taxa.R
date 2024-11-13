#' @title Extract all data for specific taxa
#'
#' @description Function to subset of all data associated with a particular taxon from a traits.build relational database.
#' 
#' @param database traits.build database (list object)
#' @param family character string of family or families
#' @param genus character string of genus or genera
#' @param taxon_name character string of taxon name(s)
#' @return List of tibbles containing all traits.build data and metadata for the specified taxa.
#' @details
#' `extract_taxa` has been developed to extract data for specific taxa from databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build-book) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/database](https://github.com/traitecoevo/database) for how to install old versions of the package or download a newer version of the database.
#'
#' @examples 
#' \dontrun{
#'extract_taxa(database = austraits, family = "Proteaceae")
#'extract_taxa(database = austraits, genus = "Acacia")
#' }
#' @author Fonti Kar - f.kar@unsw.edu.au
#' @export

extract_taxa <- function(database, family = NULL, genus = NULL, taxon_name = NULL){
  # Check compatability
  status <- check_compatibility(database)
  
  # If compatible
  if(!status){
    function_not_supported(database)
  } 
  ret <- database
  
  if(missing(family) & missing(genus) & missing(taxon_name)){
    abort("Either `family`, `genus` or `taxon_name`, must be supplied!")
  }
  
  if( ! is.null(family) ){
    return(extract_data(database, "taxa", "family", col_value = family))
  }
  
  if( ! is.null(genus) ){
    return(extract_data(database, "taxa", "genus", col_value = genus))
  }
  
  if( ! is.null(taxon_name))
    return(extract_data(database, "traits", "taxon_name", col_value = taxon_name))
}

  


  
  
  
  
  
 