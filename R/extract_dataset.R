#' @title Extract all data for a particular dataset
#'
#' @description Function to subset all data associated with a particular dataset from a traits.build relational database.
#' 
#' @usage extract_dataset(austraits, dataset_id)
#' @param database a large list of tibbles built by `traits.build` workflow
#' @param dataset_id character string that matches a `dataset_id` in the database
#' @return List of tibbles containing all traits.build data and metadata for the specified dataset(s).
#' @details
#' `extract_dataset` has been developed to extract data for specific datasets from databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build] &
#'   [https://github.com/traitecoevo/traits.build-book]
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits] for how to install old versions of the package or download a newer version of the database."
#'
#' @examples
#' \dontrun{
#' extract_dataset(austraits, "Falster_2003")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export


extract_dataset <- function(database, dataset_id) {
  
  # Check compatability
  status <- check_compatibility(database)

  # If compatible
  if(!status){
    function_not_supported(database)
  }
  
  extract_data(database, "traits", "dataset_id", col_value = dataset_id)
}


