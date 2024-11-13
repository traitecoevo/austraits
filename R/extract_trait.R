#' @title Extract all data for specific traits
#'
#' @description Function to subset all data associated with a particular trait from a traits.build relational database.
#' 
#' @usage extract_trait(database, trait_names, taxon_names)
#' @param database traits.build database (list object)
#' @param trait_names character string of trait(s) for which data will be extracted
#' @param taxon_names optional argument, specifying taxa for which data will be extracted 
#' @return List of tibbles containing all traits.build data and metadata for the specified trait(s).
#' @details
#' `extract_trait` has been developed to extract data for specific traits from databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits)  for how to install old versions of the package or download a newer version of the database.
#'
#' @examples 
#' \dontrun{
#'extract_trait(database = austraits, trait_names = "wood_density", taxon_names = "Acacia celsa")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export



extract_trait <- function(database, trait_names, taxon_names=NULL) {
  # Check compatability
  status <- check_compatibility(database, single_table_allowed = TRUE)
  
  # If compatible
  if(!status){
    function_not_supported(database)
  } 

  ret <- extract_data(database, "traits", "trait_name", col_value = trait_names)
  
  if(!is.null(taxon_names))
    ret <- extract_data(ret, "traits", "taxon_name", col_value = taxon_names)
  
  return(ret)
}
