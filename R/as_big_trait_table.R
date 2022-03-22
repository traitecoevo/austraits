#' Create a single wide table from the austraits list for ease of export 
#'
#' @param austraits_all trait table for austraits list
#'
#' @return A single wide table with collapsed contexts and sites text and with some cols renamed for clarity
#' @export
#'
#' @examples
#' \dontrun{
#' data <- austraits
#' data %>% wide_table_format()
#' }
#' @importFrom rlang .data
#' 
wide_table_format <- function(austraits_all){


    
  ################################################################################
  # Load the trait classification doc - classifies the tissue type and type of trait based on the trait_name data field
  trait_class = read.csv("data_raw/Trait_classifications_v3.csv")
  trait_class[is.na(trait_class)] = ""
  ################################################################################
  #Define the collapse-cols function
  collapse_cols <- function(data) {
    data %>% imap_dfr(~ sprintf("%s='%s'",.y,.x)) %>%
      unite("text", sep="; ") %>% pull(text)
  }
  
  ################################################################################
  # Define and adapt each table in the list of austraits to prepare for the wide table format 
  
  # the trait table needs little prep. Rename the value columns as value
  x1 = austraits$traits %>% 
    rename(c("trait_value" = "value")) 
  
  # we only need two extra columns from the trait class table - collapsing two category and other_tags cols and renaming them for clarity
  x2 = trait_class %>% mutate(trait_category = str_c(category, "; ", other_tags)) %>% 
    select(trait_name, tissue, trait_category)
  
  x2$trait_category = gsub("; $", "", x2$trait_category)
  
  # The contexts table needs the contexts collapsed to one context name per site
  x3 <- austraits$contexts %>% 
    pivot_wider(names_from = context_property, values_from = value) %>% 
    nest(data=-c(dataset_id, context_name)) %>%
    mutate(context = purrr::map_chr(data, collapse_cols)) %>%
    select(-data) 
  
  # This removes unwanted NA text left over from the collapse_cols function used previously
  x3$a = strsplit(x3$context,";")
  x3$b = lapply(x3$a, function(x){ subset(x, grepl("'NA'", x) == F)})
  x3$context = unlist(lapply(x3$b, function(x){ str_c(x, collapse = "; ")}))
  x3 = x3 %>% select(-a, -b)
  
  # Getting rid of the columns that will soon be deleted in the next austraits release and renaming the description column
  x4 = austraits$methods %>% 
    select(-year_collected_start, -year_collected_end) %>% 
    rename(c("dataset_description" = "description"))
  
  # Sites needs the most prep. We are creating two extra columns, lat and long as well as collapsing to just one site description column per site
  sites_lat_long = austraits$sites %>% 
    filter(site_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    spread(key = site_property, value = value) %>% 
    filter(!is.na(`latitude (deg)`)) %>%
    filter(`latitude (deg)` != "unknown") 
  
  # collapse into one column
  sites_condensed <- 
    austraits$sites %>% 
    filter(site_property != "latitude (deg)" & site_property != "longitude (deg)") %>% 
    pivot_wider(names_from = site_property, values_from = value, names_repair = "minimal") %>% 
    nest(data=-c(dataset_id, site_name)) %>% 
    mutate(site = purrr::map_chr(data, collapse_cols)) %>% 
    select(-data)
  
  #merge the two dataframes together
  x5 = merge(sites_lat_long, sites_condensed, by = c("dataset_id", "site_name"))
  
  #remove the unwanted extra NA text from the collapse_cols function
  x5$a = strsplit(x5$site,";")
  x5$b = lapply(x5$a, function(x){ subset(x, grepl("'NA'", x) == F)})
  x5$site = unlist(lapply(x5$b, function(x){ str_c(x, collapse = "; ")}))
  x5 = x5 %>% select(-a, -b)
  
  # rename source data field to reflect the APC/APNI name matching process better
  x6 = austraits$taxa %>% 
    rename(c("taxonNameValidation" = "source"))
  
  # merge them together
  austraits_wide = Reduce(function(a,b) merge(x = a, y = b, by = intersect(names(a), names(b))[intersect(names(a), names(b)) %in% c("dataset_id", "context_name", "site_name", "trait_name", "taxon_name")]), 
                          list(x1, x2, x3, x4, x5, x6))
  
  # reorder the names to be more intuitive
  austraits_wide = austraits_wide %>% select(
    
    # The most useful (if you are filtering for just one taxon_name)
    dataset_id, trait_name, trait_value, unit, 
    value_type, replicates, tissue, trait_category,
    
    # More stuff you can filter on
    collection_type, sample_age_class, sampling_strategy, observation_id, date,
    
    #stuff relating to sites
    `latitude (deg)`, `longitude (deg)`, site_name, site,
    
    #stuff relating to contexts and methods
    context_name, context, methods, 
    
    #the citations
    dataset_description, source_primary_citation, source_secondary_key, source_secondary_citation,
    
    #the taxa details
    taxon_name, original_name, taxonomicStatus, taxonDistribution, 
    taxonRank, genus, family, acceptedNameUsageID, 
    scientificNameAuthorship, ccAttributionIRI )
  
  return(austraits_wide)
}