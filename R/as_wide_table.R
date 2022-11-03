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
#' @importFrom rlang .data
#' @importFrom stats family
#' @importFrom utils methods
as_wide_table <- function(austraits){


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
  # Function to collapse columns in locations and contexts into single column
  process_table <- function(data) {
    
    # worker function called intop worklfow below
    # for a df, combine all column names and values
    collapse_cols <- function(data) {
      
      if(ncol(data) ==0) return(NA_character_)
      
      data %>% purrr::imap_dfr(~ sprintf("%s='%s'",.y,.x)) %>%
        tidyr::unite("text", sep="; ") %>% dplyr::pull(.data$text)
    }
    
    data %>% 
      tidyr::pivot_wider(names_from = .data$property, values_from = .data$value) %>% 
      tidyr::nest(data=-dplyr::any_of(c("dataset_id", "location_name", "context_name", "latitude (deg)", "longitude (deg)"))) %>%
      dplyr::mutate(site = purrr::map_chr(data, collapse_cols)) %>%
      dplyr::select(-data) 
  }
  
  ################################################################################
  # Define and adapt each table in the list of austraits to prepare for the wide table format 

  # the trait table needs little prep. Rename the value columns as value
  austraits$traits <- 
    austraits$traits %>% 
    dplyr::rename(c("trait_value" = "value")) 
  
  # The contexts table needs the contexts collapsed to one context name per site
  austraits$contexts <- 
    austraits$contexts %>% 
    dplyr::rename(c("property" = "context_property")) %>%
    split(austraits$contexts$dataset_id) %>%
    purrr::map_dfr(process_table)  %>% 
    dplyr::rename(c("context" = "site"))

  # Getting rid of the columns that will soon be deleted in the next austraits release and renaming the description column
  austraits$methods <- 
    austraits$methods %>% 
    #  -----------
    # TODO: this section can be removed for next release
    # Some studies have multiple records per traits. This breaks things when joining
    # For now select first
    dplyr::group_by(.data$dataset_id, .data$trait_name) %>% 
    dplyr::slice(1) %>%
    dplyr:: ungroup() %>%
    #------------
    dplyr::select(-.data$year_collected_start, -.data$year_collected_end) %>% 
    dplyr::rename(c("dataset_description" = "description"))  
  
  # collapse into one column
  austraits$locations <- 
    austraits$locations %>% 
    dplyr::filter(.data$value!="unkown") %>% 
    # next line is a fix -- one dataset in 3.0.2 has value "location_name"
    dplyr::mutate(site_property = gsub("location_name", "name", .data$site_property)) %>%
    dplyr::rename(c("property" = "site_property")) %>%
    split(., .$dataset_id) %>%
    purrr::map_dfr(process_table)

  # rename source data field to reflect the APC/APNI name matching process better
  austraits$taxa <- 
    austraits$taxa %>% 
    dplyr::rename(c("taxonNameValidation" = "source"))
  
  austraits_wide <- 
    austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "context_name"), austraits$contexts) %>%
    dplyr::left_join(by=c("dataset_id", "location_name"), austraits$locations) %>%
    dplyr::left_join(by=c("dataset_id", "trait_name"), austraits$methods) %>%
    dplyr::left_join(by=c("taxon_name"), austraits$taxa) %>%

    # reorder the names to be more intuitive
    dplyr::select(
      
    # The most useful (if you are filtering for just one taxon_name)
      .data$dataset_id, .data$observation_id, .data$trait_name, .data$taxon_name, .data$trait_value, .data$unit, 
      .data$value_type, .data$replicates, 
    # tissue, trait_category,  # Add after new zenodo release
    
    # More stuff you can filter on
    .data$date, .data$collection_type, .data$sample_age_class, .data$sampling_strategy, 
    
    #stuff relating to locations
    .data$`latitude (deg)`, .data$`longitude (deg)`, .data$location_name, .data$site,
    
    #stuff relating to contexts and methods
    .data$context_name, .data$context, .data$methods, .data$original_name,
    
    #the citations
    .data$dataset_description, .data$source_primary_citation, .data$source_secondary_citation,
    
    #the taxa details
    .data$taxonomicStatus, .data$taxonDistribution, 
    .data$taxonRank, .data$genus, .data$family, .data$acceptedNameUsageID, 
    .data$scientificNameAuthorship, .data$ccAttributionIRI
    )
  
  austraits_wide
}