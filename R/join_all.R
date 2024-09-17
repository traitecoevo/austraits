#' @title Joining location coordinates to traits table
#' @description Function to merge metadata stored in relational tables into the core traits table.
#'
#' A traits.build database includes relational tables that contain measurement (and observation) metadata pertaining to locations, contexts, methods, and taxonomy. This function merges all (or some) of the metadata from one (or all) of these tables into the traits table.
#' 
#' @param austraits traits.build generated database
#' @param vars Vector specifying which columns or values from a specific relational table to join to the traits table (currently works only for `join_` functions joining a single dataframe). For the functions `join_methods`, `join_taxa`, `join_taxonomic_updates`, and `join_contributors` the parameter vars specifies which columns to join to the traits table. For the functions `join_location_properties` and `join_context_properties` the parameter `vars` specifies which properties to add to the traits table; the functions `lookup_location_property` and `lookup_context_property` allow easy searches for relevant properties. The function `join_location_coordinates` does not include the parameter `vars`.
#' @param format A parameter for `join_context_properties`, `join_location_properties`, and `join_contributors`. Specifies whether location properties or context properties for a given observation will be concatenated, and if concatenated whether the compacted column(s) will have a human readable ("single_column_pretty") format or be format using json ("single_column_json") syntax. For location and context properties there is also the option to add each location_property or `context_property` to the traits table as its own column ("many_columns").
#' @param include_description For `join_context_properties` only, a logical indicating whether to include (TRUE) or omit (FALSE) the context_property descriptions.
#' @return traits.build list object, but with additional fields (columns) appended to `traits` dataframe
#' @details
#' the `join_` functions have been developed to join relational tables for databases built using the traits.build workflow. 
#' Learn more at:
#'   [https://github.com/traitecoevo/traits.build] &
#'   [https://github.com/traitecoevo/traits.build-book]
#'
#' Note to AusTraits users:
#' -  This function works with AusTraits version >= 5.0.0 (from Nov 2023 release)
#' -  For AusTraits versions <= 4.2.0 (up to Sept 2023 release) see [https://github.com/traitecoevo/austraits] for how to install old versions of the package or download a newer version of the database."
#'
#' @rdname join_
#' @examples
#' \dontrun{
#' austraits$traits
#'
#' #Append locations coordinates and output a single table
#' (austraits %>% join_location_coordinates)$traits
#'
#' #Append location coordinates, but maintain the relational database
#' austraits %>% join_location_coordinates()
#'
#' #Append contexts
#' (austraits %>% join_context_properties)$traits
#'
#' #Append methods
#' (austraits %>% join_methods)$traits
#'
#' #Append taxonomic details
#' (austraits %>% join_taxa)$traits
#'
#' #Append taxonomic update details
#' (austraits %>% join_taxonomic_updates)$traits
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
join_location_coordinates <- function(austraits) {

  # Check compatibility
  if(!check_compatibility(austraits)){
    function_not_supported(austraits)
  }

  location_coordinates <-
    austraits$locations %>%
    dplyr::filter(location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    tidyr::pivot_wider(names_from = location_property, values_from = value)

  # variables to join_ by depends on if location_name already in traits table
  # from joining coordinates for instances
  join_vars <- intersect(names(austraits$traits), c("dataset_id", "location_id", "location_name"))

  if (any(stringr::str_detect(names(location_coordinates), "latitude "))) {
    austraits$traits <- austraits$traits %>%
      dplyr::left_join(by = join_vars, location_coordinates)

  } else {
    austraits$traits <- austraits$traits %>%
      dplyr::mutate(
        location_name = NA_character_,
        `latitude (deg)` = NA_character_,
        `longitude (deg)` = NA_character_,
      )
  }

  austraits
}


#' @title Joining taxonomy to traits table
#' @export
#' @rdname join_location_coordinates
join_taxa <- function(austraits, vars =  c("family", "genus", "taxon_rank", "establishment_means")) {

  # Check compatibility
  if(!check_compatibility(austraits)){
    function_not_supported(austraits)
  }

  # If all columns to be added, create `vars` vector
  if (vars[1] == "all" & length(vars == 1)){
    vars <- names(austraits$taxa)
  }

  # Join selected columns to traits table
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by="taxon_name", austraits$taxa %>% dplyr::select("taxon_name", tidyselect::any_of(vars)))

  austraits
}


#' @title Joining taxonomic updates information to traits table
#' @export
#' @rdname join_location_coordinates
join_taxonomic_updates <- function(austraits, vars =  c("aligned_name")) {

  # Check compatibility
  if(!check_compatibility(austraits)){
    function_not_supported(austraits)
  }

  # If all columns to be added, create `vars` vector
  if (vars[1] == "all" & length(vars == 1)){
    vars <- names(austraits$taxonomic_updates)
  }

  # Join selected columns to traits table
  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by = c("taxon_name", "dataset_id", "original_name"),
                    austraits$taxonomic_updates %>%
                      dplyr::select("taxon_name", "dataset_id", "original_name", tidyselect::any_of(vars)))

  austraits
}

#' @title Joining methodological information to traits table
#' @export
#' @rdname join_location_coordinates
join_methods <- function(austraits, vars =  c("methods")) {

  # Check compatibility
  if(!check_compatibility(austraits)){
    function_not_supported(austraits)
  }

  # If all columns to be added, create `vars` vector
  if (vars[1] == "all" & length(vars == 1)){
    vars <- names(austraits$taxonomic_updates)
  }

  # Join selected columns to traits table
  austraits$methods %>% 
    dplyr::select(c("dataset_id", "trait_name", "method_id"), tidyselect::any_of(vars)) %>% 
    dplyr::distinct() -> methods

  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "trait_name", "method_id"),
                     methods)

  austraits
}


#' @title Joining data contributor metadata to traits table
#' @export
#' @rdname join_location_coordinates
join_contributors <- function(austraits, format = "single_column_pretty", vars =  "all") {

  # Check compatibility
  if(!check_compatibility(austraits)){
    function_not_supported(austraits)
  }

  # Work out which vars to retain and create a dataframe for compacting
  if (vars == "all") {
    contributors_tmp <- austraits$contributors
  } else {
    # Create vector that is combination of selected columns and required columns
    vars_tmp <- c("dataset_id", "last_name", "given_name", vars)
    # Determine which columns aren't wanted
    vars_remove <- setdiff(names(austraits$contributors), vars_tmp)
    # Remove unwanted columns from contributors dataframe
    contributors_tmp <- austraits$contributors %>% dplyr::select(-dplyr::any_of(vars_remove))
  }

  # Different options for how data are compacted and joined depending on `format` argument
  if (format == "single_column_pretty") {
    # collapse all metadata for a single contributor into a single cell
    contributor_metadata <-
      contributors_tmp %>%
      tidyr::pivot_longer(cols = 4:ncol(contributors_tmp)) %>%
      filter(!is.na(value)) %>%
      group_by(dataset_id, last_name, given_name) %>%
      mutate(contributor = paste0(paste0(name, "==", value), collapse = " \\ "))%>%
      select(-name, -value) %>%
      distinct() %>%
      ungroup()

    # Merge in contributor metadata and paste together with name
    compacted_contributors_column <- contributors_tmp %>%
      left_join(contributor_metadata,
                by = c("dataset_id", "last_name", "given_name")) %>%
      mutate(
        data_contributors = ifelse(is.na(contributor), 
                                   paste0(last_name, ", ", given_name),
                                   paste0(last_name, ", ", given_name, " <", contributor, ">"))) %>%
      select(dataset_id, data_contributors) %>%
      # Collapse metadata for all data contributors associated with a dataset into a single cell
      dplyr::group_by(dataset_id) %>%
      dplyr::mutate(data_contributors = paste0(data_contributors, collapse = ";; ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

  } else if (format == "single_column_json") {

    # XX - Daniel insert code to jsonify info
    # XX decide whether to drop empty columns
    # compacted_contributors_column <- contributors_tmp %>%

  }

  austraits$traits <- austraits$traits %>%
    dplyr::left_join(by = c("dataset_id"), compacted_contributors_column)

  austraits

}


#' @title Joining location properties to traits table
#' @export
#' @rdname join_location_coordinates
join_location_properties <- function(austraits, format = "single_column_pretty", vars =  "all") {

  # Check compatibility
  if(!check_compatibility(austraits)){
    function_not_supported(austraits)
  }

  # If all location properties to be added, create `vars` vector that is unique list 
  # of location properties in the database
  if (vars == "all") {

    vars_tmp <- austraits$locations %>%
      distinct(location_property) %>%
      filter(!location_property %in% c("latitude (deg)", "longitude (deg)"))

    vars <- vars_tmp$location_property
  }

  locations <- 
    austraits$locations %>% 
    dplyr::filter(location_property %in% vars)

  # Variables to join_ by depends on if location_name already in traits table
  # from joining coordinates for instances
  join_vars <- intersect(names(austraits$traits), c("dataset_id", "location_id", "location_name"))

  # Different options for how data are compacted and joined depending on `format` argument
  if (format == "many_columns") {

    # Pivot wider, so each `location_property` in its own column
    locations <- locations %>%
      mutate(location_property = paste0("location_property: ", location_property)) %>%
      tidyr::pivot_wider(names_from = location_property)

    # Join locations, based on appropriate columns
    austraits$traits <- austraits$traits %>%
      dplyr::left_join(by = join_vars, locations)

  } else if (format == "single_column_pretty") {

    # Merge each location property and its corresponding value
    compacted_locations_column <- locations %>%
      dplyr::mutate(location_properties = paste0(location_property, "==", value)) %>%
      dplyr::select(dplyr::all_of(c("dataset_id", "location_id", "location_name", "location_properties"))) %>%
      dplyr::group_by(dataset_id, location_id, location_name) %>%
      # collapse all location properties associated with a measurement into a single cell
      dplyr::mutate(location_properties = paste0(location_properties, collapse = ";; ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    austraits$traits <- austraits$traits %>%
      dplyr::left_join(by = join_vars, compacted_locations_column)

  } else if (format == "single_column_json") {

    # XX - Daniel insert code to jsonify info
    # XX make sure to drop empty columns - esp important for location properties, since most columns empty 
    # compacted_locations_column <- XX

    # austraits$traits <- austraits$traits %>%
    #  dplyr::left_join(by = join_vars, compacted_locations_column)

  }

  austraits
}


join_context_properties <- function(austraits, format = "single_column_pretty", vars =  "all", include_description = TRUE) {
  
  # Internal function - join contexts 
  join_contexts <- function(data, contexts_tmp) {
    data %>%
      dplyr::left_join(
        by = c("dataset_id", "treatment_context_id"),
        reformat_contexts(contexts_tmp, "treatment_context_id")
      ) %>%
      dplyr::left_join(
        by = c("dataset_id", "plot_context_id"),
        reformat_contexts(contexts_tmp, "plot_context_id")
      ) %>%
      dplyr::left_join(
        by = c("dataset_id", "entity_context_id"),
        reformat_contexts(contexts_tmp, "entity_context_id")
      ) %>%
      dplyr::left_join(
        by = c("dataset_id", "temporal_context_id"),
        reformat_contexts(contexts_tmp, "temporal_context_id")
      ) %>%
      dplyr::left_join(
        by = c("dataset_id", "method_context_id"),
        reformat_contexts(contexts_tmp, "method_context_id")
      )
  }

  # Check compatibility
  if(!check_compatibility(austraits)){
    function_not_supported(austraits)
  }

  # If all context properties to be added, create `vars` vector that is unique list 
  # of context properties in the database
  if (vars == "all") {

    vars_tmp <- austraits$contexts %>%
      distinct(context_property)

    vars <- vars_tmp$context_property
  }

  # Create dataframe of contexts to use & add `context_property:` to context properties
  contexts_tmp <- 
    austraits$contexts %>% 
    dplyr::filter(context_property %in% vars) %>%
    dplyr::mutate(context_property = paste0(category, ": ", context_property))

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

    # Need specific reformat function for `many_columns` formatting
    reformat_contexts <- function(contexts_table, context_id) {
      context_category <- gsub("_id", "_properties", context_id, fixed = TRUE)
      out <- contexts_table %>%
        dplyr::filter(link_id == context_id) %>%
        tidyr::pivot_wider(names_from = context_property, values_from = value) %>%
        dplyr::select(-link_id) %>%
        dplyr::distinct(dataset_id, link_vals, .keep_all = TRUE)
      names(out)[which(names(out) == "link_vals")] <- context_id
      out
    }

    # Merge contexts to austraits$traits using generic join_contexts function
    austraits$traits <- join_contexts(austraits$traits, contexts_tmp)

  } else if (format == "single_column_pretty") {
    contexts_tmp <-
      contexts_tmp %>%
      dplyr::mutate(
        value = ifelse(
          is.na(description)| include_description == FALSE,
          paste0(context_property, "==", value),
          paste0(context_property, "==", value, " <<", description, ">>"))
      ) %>%
      dplyr::select(-dplyr::all_of(c("description", "context_property", "category"))) %>%
      tidyr::separate_longer_delim(link_vals, ", ") %>%
      dplyr::distinct() %>%
      dplyr::group_by(dataset_id, link_id, link_vals) %>%
      dplyr::mutate(value = paste0(value, collapse = ";; ")) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    # Need specific reformat function for `single_column_pretty` formatting
    reformat_contexts <- function(contexts_table, context_id) {
      context_category <- gsub("_id", "_properties", context_id, fixed = TRUE)
      out <- contexts_table %>%
        dplyr::filter(link_id == context_id) %>%
        dplyr::select(-link_id) %>%
        dplyr::distinct(dataset_id, link_vals, .keep_all = TRUE)
      names(out)[which(names(out) == "value")] <- context_category
      names(out)[which(names(out) == "link_vals")] <- context_id
      out
    }

    # Merge contexts to austraits$traits using generic join_contexts function
    austraits$traits <- join_contexts(austraits$traits, contexts_tmp)

  } else if (format == "single_column_json") {

    ## XX- Daniel, add json option 

  }

  austraits

}