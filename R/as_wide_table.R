#' Create a single wide table from the AusTraits data object
#'
#' @param austraits austraits data object
#'
#' @return A single wide table with collapsed contexts and locations text and with 
#' some cols renamed for alignment with other resources
#' @export
#'
#' @examples
#' \dontrun{
#' data <- austraits
#' data %>% as_wide_table()
#' }
#' @importFrom stats family
#' @importFrom utils methods

as_wide_table <- function(austraits){
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  }

  # Function to collapse columns in locations and contexts into single column
  process_table3 <- function(data) {
    data %>% 
      tidyr::pivot_wider(names_from = "property", values_from = "value") %>% 
      tidyr::nest(data=-dplyr::any_of(c("dataset_id", "location_id", "latitude (deg)", "longitude (deg)"))) %>%
      dplyr::mutate(location = purrr::map_chr(data, collapse_cols)) %>%
      dplyr::select(-data) 
  }
  
  ################################################################################
  # Define and adapt each table in the list of austraits to prepare for the wide table format 

    # The contexts table needs the contexts collapsed to one context name per site
  austraits %>% 
    join_context_properties(format = "single_column_pretty", include_description = FALSE) -> austraits

  # Getting rid of the columns that will soon be deleted in the next austraits release and renaming the description column
  austraits$methods <- 
    austraits$methods %>% 
    dplyr::rename(dataset_description = "description")  %>% 
    dplyr::distinct()
  
  # collapse into one column
  austraits$locations <- 
    austraits$locations %>% 
    dplyr::filter(value!="unknown") %>% 
    dplyr::rename("property" = "location_property") %>%
    split(., .$dataset_id) %>%
    purrr::map_dfr(process_table3)

  # rename taxonomic_dataset field to reflect the APC/APNI name matching process better
  austraits$taxa <- 
    austraits$taxa %>% 
    dplyr::distinct()
  
  austraits_wide <- 
    austraits$traits %>% 
    dplyr::left_join(by=c("dataset_id", "location_id"), austraits$locations) %>%
    dplyr::left_join(by=c("dataset_id", "method_id", "trait_name"), austraits$methods) %>%
    dplyr::left_join(by=c("taxon_name"), austraits$taxa)

    # reorder the names to be more intuitive
    austraits_wide %>% dplyr::select(
      
    # The most useful (if you are filtering for just one taxon_name)
      "dataset_id", "observation_id", "trait_name", "taxon_name", "value", "unit", 
      "entity_type", "population_id", "individual_id",
      "value_type", "basis_of_value", 
      "replicates", 
    # tissue, trait_category,  # Add after new zenodo release
    
    # More stuff you can filter on
    "collection_date", "basis_of_record", "life_stage", "sampling_strategy", 
    "treatment_context_id", "temporal_context_id", 
    
    #stuff relating to locations
    "latitude (deg)", "longitude (deg)", "location", "plot_context_id",
    
    #stuff relating to contexts and methods
    "context", "methods", "method_id", "method_context_id", "original_name",
    
    #the citations
    "dataset_description", "source_primary_citation", "source_secondary_citation",
    
    #the taxa details
    "taxonomic_status", "taxon_distribution", 
    "taxon_rank", "genus", "family"
    )
  
  austraits_wide
}

#' Collapse columns into text string
#' @keywords internal
#' @noRd
collapse_cols <- function(data) {
  
  if(ncol(data) ==0) return(NA_character_)
  
  data %>% purrr::imap_dfr(~ sprintf("%s='%s'",.y,.x)) %>%
    tidyr::unite("text", sep="; ") %>% dplyr::pull(text)
}