#' @title Joining location coordinates to traits table
#' @description Function to merge geographic coordinates (latitude/longitude) 
#' stored in the locations table of a traits.build database into the core traits table.
#' 
#' @param database traits.build database (list object)
#' @return traits.build list object, but with additional fields (columns) 
#' for latitude and longitude appended to `traits` dataframe
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
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
#' (database %>% join_location_coordinates)$traits
#' }
#' 
#' @export
join_location_coordinates <- function(database) {

  # Check compatibility
  if(!check_compatibility(database)){
    function_not_supported(database)
  }

  location_coordinates <-
    database$locations %>%
    dplyr::filter(location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    tidyr::pivot_wider(names_from = location_property, values_from = value)

  # variables to join_ by depends on if location_name already in traits table
  # from joining coordinates for instances
  join_vars <- intersect(names(database$traits), c("dataset_id", "location_id", "location_name"))

  if (any(stringr::str_detect(names(location_coordinates), "latitude "))) {
    database$traits <- 
      database$traits %>%
      dplyr::left_join(by = join_vars, location_coordinates)

  } else {
    database$traits <- 
      database$traits %>%
      dplyr::mutate(
        location_name = NA_character_,
        `latitude (deg)` = NA_character_,
        `longitude (deg)` = NA_character_,
      )
  }
  
  database
}


#' @title Joining taxonomy to traits table

#' @description Function to merge metadata from the taxa table of a traits.build database into the core traits table.
#' 
#' @param database traits.build database (list object)
#' @param vars Columns from the taxa table to be joined to the traits table, defaulting to c("family", "genus", "taxon_rank", "establishment_means").
#' 
#' @return traits.build list object, but with additional fields (columns) for the specified variables from the taxa table appended to the traits table.
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits)  for how to install old versions of the package or download a newer version of the database.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' #Append taxonomic details
#' (database %>% join_taxa)$traits
#' }
join_taxa <- function(database, 
                      vars =  c("family", "genus", "taxon_rank", "establishment_means")) {

  # Check compatibility
  if(!check_compatibility(database)){
    function_not_supported(database)
  }

  # If all columns to be added, create `vars` vector
  if (vars[1] == "all" & length(vars == 1)){
    vars <- names(database$taxa)
  }

  # Join selected columns to traits table
  database$traits <- 
    database$traits %>%
    dplyr::left_join(by="taxon_name", database$taxa %>% dplyr::select("taxon_name", tidyselect::any_of(vars)))

  database
}


#' @title Joining taxonomic updates information to traits table
#' 
#' @description Function to merge metadata from the taxonomic_updates table of a traits.build database into the core traits table.
#' 
#' @param database traits.build database (list object)
#' @param vars Columns from the taxa table to be joined to the traits table, defaulting to c("aligned_name").
#' 
#' @return traits.build list object, but with additional fields (columns) for the specified variables from the taxonomic_updates table appended to the traits table.
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits)  for how to install old versions of the package or download a newer version of the database.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' #Append taxonomic update details
#' (database %>% join_taxonomic_updates)$traits
#' }
join_taxonomic_updates <- function(database, vars =  c("aligned_name")) {

  # Check compatibility
  if(!check_compatibility(database)){
    function_not_supported(database)
  }

  # If all columns to be added, create `vars` vector
  if (vars[1] == "all" & length(vars == 1)){
    vars <- names(database$taxonomic_updates)
  }

  # Join selected columns to traits table
  database$traits <- 
    database$traits %>%
    dplyr::left_join(by = c("taxon_name", "dataset_id", "original_name"),
        database$taxonomic_updates %>%
          dplyr::select("taxon_name", "dataset_id", "original_name", 
                        tidyselect::any_of(vars)))

  database
}

#' @title Joining methodological information to traits table
#' 
#' @description Function to merge metadata from the methods table of a traits.build database into the core traits table.
#' 
#' @param database traits.build database (list object)
#' @param vars Columns from the taxa table to be joined to the traits table, defaulting to c("methods").
#' 
#' @return traits.build list object, but with additional fields (columns) for the specified variables from the methods table appended to the traits table.
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits)  for how to install old versions of the package or download a newer version of the database.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' (database %>% join_methods)$traits
#' }
join_methods <- function(database, vars =  c("methods")) {

  # Check compatibility
  if(!check_compatibility(database)){
    function_not_supported(database)
  }

  # If all columns to be added, create `vars` vector
  if (vars[1] == "all" & length(vars == 1)){
    vars <- names(database$methods)
  }

  # Join selected columns to traits table
  database$traits <- 
    database$traits %>%
    dplyr::left_join(by=c("dataset_id", "trait_name", "method_id"),
      database$methods %>% 
      dplyr::select(c("dataset_id", "trait_name", "method_id"), tidyselect::any_of(vars)) %>% 
      dplyr::distinct()
    )

  database
}


#' @title Joining data contributor metadata to traits table
#' 
#' @description Function to merge metadata from the data contributors table of a traits.build database into the core traits table.
#' 
#' @param database traits.build database (list object)
#' @param format Specifies whether metadata from the contributors table is output in a human readable format ("single_column_pretty"; default) or using json syntax ("single_column_json").
#' @param vars Columns from the taxa table to be joined to the traits table, defaulting to all columns (vars = "all").
#' 
#' @return traits.build list object, but with additional fields (columns) for the specified variables from the data contributors table appended to the traits table.
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits)  for how to install old versions of the package or download a newer version of the database.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' (database %>% join_contributors(format = "single_column_pretty", 
#' vars = c("last_name", "first_name", "ORCID")))$traits
#' }
join_contributors <- function(database,
                              format = "single_column_pretty",
                              vars =  "all") {

  # Check compatibility
  if(!check_compatibility(database)){
    function_not_supported(database)
  }

  # Work out which vars to retain and create a dataframe for compacting
  if (vars[1] == "all") {
    contributors_tmp <- database$contributors
  } else {
    # Create vector that is combination of selected columns and required columns
    vars_tmp <- c("dataset_id", "last_name", "given_name", vars)
    # Determine which columns aren't wanted
    vars_remove <- setdiff(names(database$contributors), vars_tmp)
    # Remove unwanted columns from contributors dataframe
    contributors_tmp <- database$contributors %>% dplyr::select(-dplyr::any_of(vars_remove))
  }

  # Different options for how data are compacted and joined depending on `format` argument
  if (format == "single_column_pretty") {
    # collapse all metadata for a single contributor into a single cell
    contributor_metadata <-
      contributors_tmp %>%
      tidyr::pivot_longer(cols = 4:ncol(contributors_tmp)) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::group_by(dataset_id, last_name, given_name) %>%
      dplyr::mutate(contributor = paste0(paste0(name, "==", value), collapse = " \\ "))%>%
      dplyr::select(-name, -value) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    # Merge in contributor metadata and paste together with name
    compacted_contributors_column <- 
      contributors_tmp %>%
      dplyr::left_join(contributor_metadata,
                by = c("dataset_id", "last_name", "given_name")) %>%
      dplyr::mutate(
        data_contributors = ifelse(is.na(contributor), 
                                   paste0(last_name, ", ", given_name),
                                   paste0(last_name, ", ", given_name, " <<", contributor, ">>"))) %>%
      dplyr::select(dataset_id, data_contributors) %>%
      # Collapse metadata for all data contributors associated with a dataset into a single cell
      dplyr::group_by(dataset_id) %>%
      dplyr::mutate(data_contributors = paste0(data_contributors, collapse = ";; ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

  } else if (format == "single_column_json") {

    compacted_contributors_column <-
      contributors_tmp %>% 
      tidyr::nest(data = -dplyr::all_of(c("dataset_id"))) %>%
      dplyr::mutate(data_contributors = purrr::map_chr(data, jsonlite::toJSON)) %>%
      dplyr::select(-dplyr::any_of("data")) %>%
      dplyr::ungroup()
  } else {
    stop("format not supported: ", format)
  }

  database$traits <- database$traits %>%
    dplyr::left_join(by = c("dataset_id"), compacted_contributors_column)

  database
}


#' @title Joining location properties to traits table
#' 
#' @description Function to merge metadata from the locations table of a traits.build database into the core traits table.
#' 
#' @param database traits.build database (list object)
#' @param format Specifies whether metadata from the locations is output in a human readable format ("single_column_pretty"; default), with each location property added as a separate column ("many_columns") or using json syntax ("single_column_json").
#' @param vars Location properties for which data is to be appended to the traits table, defaulting to all location properties (vars = "all").
#' 
#' @return traits.build list object, but location properties from the locations table appended to the traits table.
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits)  for how to install old versions of the package or download a newer version of the database.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' (database %>% join_location_properties(format = "single_column_pretty", vars = "all"))$traits
#' }
join_location_properties <- function(database,
                                     format = "single_column_pretty",
                                     vars =  "all") {

  # Check compatibility
  if(!check_compatibility(database)){
    function_not_supported(database)
  }

  # If all location properties to be added, create `vars` vector that is unique list 
  # of location properties in the database
  if (vars[1] == "all") {

    vars_tmp <- database$locations %>%
      dplyr::distinct(location_property) %>%
      dplyr::filter(!location_property %in% c("latitude (deg)", "longitude (deg)"))

    vars <- vars_tmp$location_property
  }
  
  # If latitude, longitude present in vars list, remove them
  vars <- setdiff(vars, c("latitude (deg)", "longitude (deg)"))

  locations <- 
    database$locations %>% 
    dplyr::filter(location_property %in% vars)

  # Variables to join_ by depends on if location_name already in traits table
  # from joining coordinates for instances
  join_vars <- intersect(names(database$traits), c("dataset_id", "location_id", "location_name"))

  # Different options for how data are compacted and joined depending on `format` argument
  if (format == "many_columns") {

    # Pivot wider, so each `location_property` in its own column
    locations <- 
      locations %>%
      dplyr::mutate(location_property = paste0("location_property: ", location_property)) %>%
      tidyr::pivot_wider(names_from = location_property)

    # Join locations, based on appropriate columns
    database$traits <- 
      database$traits %>%
      dplyr::left_join(by = join_vars, locations)

  } else if (format == "single_column_pretty") {

    # Merge each location property and its corresponding value
    compacted_locations_column <- 
      locations %>%
      dplyr::mutate(location_properties = paste0(location_property, "==", value)) %>%
      dplyr::select(dplyr::all_of(c("dataset_id", "location_id", "location_name", "location_properties"))) %>%
      dplyr::group_by(dataset_id, location_id, location_name) %>%
      # collapse all location properties associated with a measurement into a single cell
      dplyr::mutate(location_properties = paste0(location_properties, collapse = ";; ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    database$traits <- 
      database$traits %>%
      dplyr::left_join(by = join_vars, compacted_locations_column)

  } else if (format == "single_column_json") {

    compacted_locations_column <-
      locations %>% 
      tidyr::nest(data = -dplyr::all_of(c("dataset_id", "location_id", "location_name"))) %>%
      dplyr::mutate(location_properties = purrr::map_chr(data, jsonlite::toJSON)) %>%
      dplyr::select(-dplyr::any_of("data")) %>%
      dplyr::ungroup()
    
    database$traits <- database$traits %>%
      dplyr::left_join(by = join_vars, compacted_locations_column)

  } else {
    stop("format not supported: ", format)
  }

  database
}

#' @title Joining context properties to traits table
#' 
#' @description Function to merge metadata from the contexts table of a traits.build database into the core traits table.
#' 
#' @param database traits.build database (list object)
#' @param format Specifies whether metadata from the contexts is output in a human readable format ("single_column_pretty"; default), with each context property added as a separate column ("many_columns") or using json syntax ("single_column_json").
#' @param vars Location properties for which data is to be appended to the traits table, defaulting to all context properties (vars = "all").
#' @param include_description A logical indicating whether to include (TRUE) or omit (FALSE) the context_property descriptions.
#' 
#' @return traits.build list object, but context properties from the contexts table appended to the traits table.
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build) &
#'   [https://github.com/traitecoevo/traits.build-book](https://github.com/traitecoevo/traits.build-book)
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits](https://github.com/traitecoevo/austraits)  for how to install old versions of the package or download a newer version of the database.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' (database %>% join_context_properties(
#' format = "many_columns", vars = "all", include_description = TRUE))$traits
#' }
join_context_properties <- function(database,
                                    format = "single_column_pretty",
                                    vars =  "all",
                                    include_description = TRUE) {
  
  # Check compatibility
  if(!check_compatibility(database)){
    function_not_supported(database)
  }

  # If all context properties to be added, create `vars` vector that is unique list 
  # of context properties in the database
  if (vars[1] == "all") {
    vars <- database$contexts$context_property %>% unique()
  }

  # Create dataframe of contexts to use & add `context_property:` to context properties
  contexts_tmp <- 
    database$contexts %>% 
    dplyr::filter(context_property %in% vars) %>%
    dplyr::mutate(context_property = paste0(category, ": ", context_property))

  # From here format depends on desired output
  if (format == "many_columns") {
    contexts_tmp <-
      contexts_tmp %>%
      dplyr::mutate(
        value = ifelse(
          is.na(description) | include_description == FALSE,
          value,
          paste0(value, " <<", description, ">>"))
      ) %>%
      dplyr::select(-dplyr::all_of(c("description", "category"))) %>%
      tidyr::separate_longer_delim(link_vals, ", ")
    
    pivot <- TRUE
  } else if (format == "single_column_pretty") {
    contexts_tmp <-
      contexts_tmp %>%
      dplyr::mutate(
        value = ifelse(
          !is.na(description) & include_description,
          paste0(context_property, "==", value, " <<", description, ">>"),
          paste0(context_property, "==", value))
      ) %>%
      dplyr::select(-dplyr::all_of(c("description", "context_property", "category"))) %>%
      tidyr::separate_longer_delim(link_vals, ", ") %>%
      dplyr::distinct() %>%
      dplyr::group_by(dataset_id, link_id, link_vals) %>%
      dplyr::mutate(value = paste0(value, collapse = ";; ")) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()
    
    pivot <- FALSE
  } else if (format == "single_column_json") {
    
    contexts_tmp <-
      contexts_tmp %>% 
      tidyr::separate_longer_delim(link_vals, ", ") %>%
      dplyr::distinct() %>% 
      dplyr::mutate(description = ifelse(!is.na(description) & include_description, description, NA)) %>% 
      tidyr::nest(data = -dplyr::all_of(c("dataset_id", "link_id", "link_vals"))) %>% 
      dplyr::mutate(value = purrr::map_chr(data, jsonlite::toJSON)) %>%
      dplyr::select(-dplyr::any_of("data")) %>%
      dplyr::ungroup()
    
    pivot <- FALSE

  } else {
    stop("format not supported: ", format)
  }

  # Merge contexts to database$traits

  # Defines a function to further reformat specific columns of the context table
  reformat_contexts <- function(data, context_id, pivot) {

    data <- 
      data %>%
      dplyr::filter(link_id == context_id) 

    if(pivot) {
      data <- tidyr::pivot_wider(data, names_from = context_property, values_from = value)
    }

    data <- 
      data %>%
      dplyr::select(-link_id) %>%
      dplyr::distinct(dataset_id, link_vals, .keep_all = TRUE) 
    
    names(data)[which(names(data) == "value")] <- gsub("_id", "_properties", context_id, fixed = TRUE)
    names(data)[which(names(data) == "link_vals")] <- context_id
    
    data
  }


  database$traits <- 
    database$traits %>%
    dplyr::left_join(
      by = c("dataset_id", "treatment_context_id"),
      reformat_contexts(contexts_tmp, "treatment_context_id", pivot)
    ) %>%
    dplyr::left_join(
      by = c("dataset_id", "plot_context_id"),
      reformat_contexts(contexts_tmp, "plot_context_id", pivot)
    ) %>%
    dplyr::left_join(
      by = c("dataset_id", "entity_context_id"),
      reformat_contexts(contexts_tmp, "entity_context_id", pivot)
    ) %>%
    dplyr::left_join(
      by = c("dataset_id", "temporal_context_id"),
      reformat_contexts(contexts_tmp, "temporal_context_id", pivot)
    ) %>%
    dplyr::left_join(
      by = c("dataset_id", "method_context_id"),
      reformat_contexts(contexts_tmp, "method_context_id", pivot)
    )

  database

}

