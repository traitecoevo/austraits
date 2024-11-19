#' Create a single wide table from a traits.build data object
#'
#' @param database traits.build database (list object)
#'
#' @return A single wide table with collapsed contexts and locations text and with 
#' some cols renamed for alignment with other resources
#' @export
#'
#' @examples
#' \dontrun{
#' austraits %>% as_wide_table()
#' }
#' @importFrom stats family
#' @importFrom utils methods

as_wide_table <- function(database){
  # Check compatability
  status <- check_compatibility(database)
  
  # If compatible
  if(!status){
    function_not_supported(database)
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
  # Define and adapt each table in the list of a traits.build database to prepare for the wide table format 

    # The contexts table needs the contexts collapsed to one context name per site
  database %>% 
    join_contexts_old(collapse_context = TRUE) -> database

  # Getting rid of the columns that will soon be deleted in the next austraits release and renaming the description column
  database$methods <- 
    database$methods %>% 
    dplyr::rename(dataset_description = "description")  %>% 
    dplyr::distinct()
  
  # collapse into one column
  database$locations <- 
    database$locations %>% 
    dplyr::filter(value!="unknown") %>% 
    dplyr::rename("property" = "location_property") %>%
    split(., .$dataset_id) %>%
    purrr::map_dfr(process_table3)

  # rename taxonomic_dataset field to reflect the APC/APNI name matching process better
  database$taxa <- 
    database$taxa %>% 
    dplyr::distinct()
  
  database_wide <- 
    database$traits %>% 
    dplyr::left_join(by=c("dataset_id", "location_id"), database$locations) %>%
    dplyr::left_join(by=c("dataset_id", "method_id", "trait_name"), database$methods) %>%
    dplyr::left_join(by=c("taxon_name"), database$taxa)

    # reorder the names to be more intuitive
    database_wide %>% dplyr::select(
      
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
  
  database_wide
}

#' Collapse columns into text string
#' @keywords internal
#' @noRd
collapse_cols <- function(data) {
  
  if(ncol(data) ==0) return(NA_character_)
  
  data %>% purrr::imap_dfr(~ sprintf("%s='%s'",.y,.x)) %>%
    tidyr::unite("text", sep="; ") %>% dplyr::pull(text)
}

#' Old join contexts function that collapses contexts into a single column and doesn't specify categories of context properties.
#' @keywords internal
#' @noRd
join_contexts_old <- function(austraits, collapse_context = FALSE){
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status){
    function_not_supported(austraits)
  } 

  traits2 <- split(austraits$traits, austraits$traits$dataset_id)
  contexts2 <- split(austraits$contexts, austraits$contexts$dataset_id)
  
  traits_vars <- names(austraits$traits)

  problem_studies <- c("Hall_1981")

  for(id in names(traits2)) {
  
    if(!is.null(contexts2[[id]][1]) & ! (id %in% problem_studies)) {
    
      context_ids <- 
        unique(contexts2[[id]]$link_id)
  
      for(v in context_ids[!is.na(context_ids)]) {
        
        context_sub <- 
          contexts2[[id]] %>%
          dplyr::select(-dplyr::any_of(c("category", "description"))) %>%
          dplyr::filter(link_id == v) %>% 
          tidyr::separate_rows(link_vals) %>% 
          tidyr::pivot_wider(values_from = value, names_from = context_property) %>%
          tidyr::pivot_wider(names_from = link_id, values_from = link_vals)
        
        traits2[[id]] <- 
          dplyr::left_join(by = c("dataset_id", v),
                    traits2[[id]],
                    context_sub
          )
      }
      
      if(collapse_context == TRUE){
        context_text <-
          traits2[[id]] %>% 
          dplyr::select(-dplyr::any_of(traits_vars)) %>% collapse_cols()

        traits2[[id]] <- traits2[[id]] %>% 
          dplyr::mutate(context = context_text) %>% 
          dplyr::select(dplyr::any_of(traits_vars), context)
      }
    }
  }

  austraits$traits <- traits2 %>% dplyr::bind_rows()

  austraits
}