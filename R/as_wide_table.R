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
  # Switch for different versions
  version <- what_version(austraits)
  
  switch (version,
          "5-series" = as_wide_table3(austraits),
          "4-series" = as_wide_table2(austraits),
          "3-series-earlier" = as_wide_table1(austraits)
          )
}

#' Turning entire AusTraits object into wide table v5
#' @noRd
#' @keywords internal
as_wide_table3 <- function(austraits){

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
    join_contexts(collapse_context = TRUE) -> austraits

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
    # dplyr::rename("taxonNameValidation" = "taxonomic_dataset") %>% 
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

#' Turning entire AusTraits object into wide table v4
#' @noRd
#' @keywords internal
as_wide_table2 <- function(austraits){
  
  # Function to collapse columns in locations and contexts into single column
  process_table2 <- function(data) {
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
    join_contexts(collapse_context = TRUE) -> austraits
  
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
    purrr::map_dfr(process_table2)
  
  # rename taxonomic_dataset field to reflect the APC/APNI name matching process better
  austraits$taxa <- 
    austraits$taxa %>% 
    dplyr::rename("taxonNameValidation" = "taxonomic_dataset") %>% 
    dplyr::distinct()
  
  austraits_wide <- 
    austraits$traits %>% 
    dplyr::left_join(by=c("dataset_id", "location_id"), austraits$locations) %>%
    dplyr::left_join(by=c("dataset_id", "trait_name"), austraits$methods) %>%
    dplyr::left_join(by=c("taxon_name"), austraits$taxa)
  
  # reorder the names to be more intuitive
  austraits_wide %>% dplyr::select(dplyr::any_of(
    
    # The most useful (if you are filtering for just one taxon_name)
    "dataset_id", "observation_id", "trait_name", "taxon_name", "value", "unit", 
    "entity_type", "population_id", "individual_id",
    "value_type", "basis_of_value", 
    "replicates", 
    # tissue, trait_category,  # Add after new zenodo release
    
    # More stuff you can filter on
    "collection_date", "basis_of_record", "life_stage", "sampling_strategy", 
    "treatment_id", "temporal_id", 
    
    #stuff relating to locations
    "latitude (deg)", "longitude (deg)", "location", "plot_id",
    
    #stuff relating to contexts and methods
    "context", "methods", "method_id", "original_name",
    
    #the citations
    "dataset_description", "source_primary_citation", "source_secondary_citation",
    
    #the taxa details
    "taxonomic_status", "taxon_distribution", 
    "taxon_rank", "genus", "family"

  ))
  
  austraits_wide
}

#' Turning entire AusTraits object into wide table <=3.0.2
#' @noRd
#' @keywords  internal
as_wide_table1 <- function(austraits){
  
  
  ################################################################################
  # TODO: this updated with next zenodo release
  # Load the trait classification doc - classifies the tissue type and type of trait based on the trait_name data field
  # Exclude this for now -- will be added to definitions file in future release
  # trait_class = read.csv("data-raw/Trait_classifications_v3.csv")
  # trait_class[is.na(trait_class)] = ""
  # trait_class <- trait_class %>% as_tibble()
  # 
  # we only need two extra columns from the trait class table - collapsing two category and other_tags cols and renaming them for clarity
  # x2 <- 
  #   trait_class %>% dplyr::mutate(
  #   trait_category = str_c(category, "; ", other_tags) %>% gsub("; $", "", .)
  # ) %>% 
  #   dplyr::select(trait_name, tissue, trait_category)
  # 
  # Function to collapse columns in sites and contexts into single column
  process_table <- function(data) {
   
    data %>% 
      tidyr::pivot_wider(names_from = "property", values_from = "value") %>% 
      tidyr::nest(data=-dplyr::any_of(c("dataset_id", "site_name", "context_name", "latitude (deg)", "longitude (deg)"))) %>%
      dplyr::mutate(site = purrr::map_chr(data, collapse_cols)) %>%
      dplyr::select(-data) 
  }
  
  ################################################################################
  # Define and adapt each table in the list of austraits to prepare for the wide table format 
  
  # the trait table needs little prep. Rename the value columns as value
  austraits$traits <- 
    austraits$traits %>% 
    dplyr::rename(trait_value = "value")
  
  # The contexts table needs the contexts collapsed to one context name per site
  austraits$contexts <- 
    austraits$contexts %>% 
    dplyr::rename(property = "context_property") %>%
    split(austraits$contexts$dataset_id) %>%
    purrr::map_dfr(process_table)  %>% 
    dplyr::rename(context = "site")
  
  # Getting rid of the columns that will soon be deleted in the next austraits release and renaming the description column
  austraits$methods <- 
    austraits$methods %>% 
    #  -----------
  # TODO: this section can be removed for next release
  # Some studies have multiple records per traits. This breaks things when joining
  # For now select first
  dplyr::group_by(dataset_id, trait_name) %>% 
    dplyr::slice(1) %>%
    dplyr:: ungroup() %>%
    #------------
  dplyr::select(-c("year_collected_start", "year_collected_end")) %>% 
    dplyr::rename(dataset_description = "description")  
  
  # collapse into one column
  austraits$sites <- 
    austraits$sites %>% 
    dplyr::filter(value!="unknown") %>% 
    # next line is a fix -- one dataset in 3.0.2 has value "site_name"
    dplyr::mutate(site_property = gsub("site_name", "name", site_property)) %>%
    dplyr::rename("property" = "site_property") %>%
    split(., .$dataset_id) %>%
    purrr::map_dfr(process_table)
  
  # rename source data field to reflect the APC/APNI name matching process better
  austraits$taxa <- 
    austraits$taxa %>% 
    dplyr::rename(taxonNameValidation = "source")
  
  austraits_wide <- 
    austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "context_name"), austraits$contexts) %>%
    dplyr::left_join(by=c("dataset_id", "site_name"), austraits$sites) %>%
    dplyr::left_join(by=c("dataset_id", "trait_name"), austraits$methods) %>%
    dplyr::left_join(by=c("taxon_name"), austraits$taxa) %>%
    
    # reorder the names to be more intuitive
    dplyr::select(
      
      # The most useful (if you are filtering for just one taxon_name)
      "dataset_id", "observation_id", "trait_name", "taxon_name", "trait_value", "unit", 
      "value_type", "replicates", 
      # tissue, trait_category,  # Add after new zenodo release
      
      # More stuff you can filter on
      "date", "collection_type", "sample_age_class", "sampling_strategy", 
      
      #stuff relating to sites
      "latitude (deg)", "longitude (deg)", "site_name", "site",
      
      #stuff relating to contexts and methods
      "context_name", "context", "methods", "original_name",
      
      #the citations
      "dataset_description", "source_primary_citation", "source_secondary_citation",
      
      #the taxa details
      "taxonomicStatus", "taxonDistribution", 
      "taxonRank", "genus", "family", "acceptedNameUsageID", 
      "scientificNameAuthorship", "ccAttributionIRI"
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