#' Create combined traits.build table
#'
#' Create a single database output that merges together the information
#' in all relational tables within a traits.build database.
#' Trait measurements are still output in long format (1 row per trait value),
#' but all measurement-related metadata (methods, location properties, context properties, contributors)
#' are now included as additional columns in a single table.
#'
#' @param database A traits.build database
#'
#' @return A table combining information in 7 traits.build relational tables: traits, locations, contexts, methods, taxa, taxonomic_updates, and contributors
#' @export
#'
#' @usage database_create_combined_table(database)
#' 
database_create_combined_table <- function(database) {
  
  # separate latitude and longitude, as these will be displayed in dedicated columns
  location_latlon <-
    database$locations %>%
    dplyr::filter(location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    tidyr::pivot_wider(names_from = location_property, values_from = value)
  
  # pack all location property information into a single column
  location_properties <-
    database$locations %>%
    # remove latitude/longitude columns, as these have been retained above
    dplyr::filter(!location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    dplyr::mutate(
      
      # XX - TO DO - NOTE TO DANIEL - replacing syntax isn't a perfect solution because we'll never we able to fully recreate the starting point
      # after implementing the `standardise_syntax` function, I've instead, FOR NOW, used various double syntax as the "hooks" 
      # ==, ;;, <<, >>, and || are used to compact information
      
      #value = standardise_syntax(value),
      #location_property = standardise_syntax(location_property)
      ) %>%
    # merge each location property and its corresponding value
    dplyr::mutate(location_properties = paste0(location_property, "==", value)) %>%
    dplyr::select(dplyr::all_of(c("dataset_id", "location_id", "location_name", "location_properties"))) %>%
    dplyr::group_by(dataset_id, location_id, location_name) %>%
    # collapse all location properties associated with a measurement into a single cell
    dplyr::mutate(location_properties = paste0(location_properties, collapse = ";; ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  # pack all contributor information into a single column
  contributors <-
    database$contributors %>%
    dplyr::mutate(
      #affiliation = standardise_syntax(affiliation),
      #additional_role = standardise_syntax(additional_role),
      
      # TO DO this next line might be difficult to revert in its current form, because people have different numbers of first vs last names
      data_collectors = paste0(last_name, ", ", given_name),
      # merge each data collector with metadata pertaining to them, such as ORCID, affiliation, and any additional roles
      data_collectors = ifelse(
        !is.na(ORCID),
        paste0(data_collectors, " <<ORCID==", ORCID),
        data_collectors),
      data_collectors = ifelse(
        is.na(ORCID),
        paste0(data_collectors, " <<affiliation==", affiliation),
        paste0(data_collectors, " || affiliation==", affiliation)),
      data_collectors = ifelse(
        !is.na(additional_role),
        paste0(data_collectors, " || additional_role==", additional_role, ">>"),
        paste0(data_collectors, ">>"))
    ) %>%
    dplyr::select(-dplyr::all_of(c("last_name", "given_name", "ORCID", "affiliation", "additional_role"))) %>%
    
    # collapse all information for all data collectors associated with a measurement into a single cell
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(data_collectors = paste0(data_collectors, collapse = ";; ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  contexts_tmp <-
    database$contexts %>%
    dplyr::mutate(
      #context_property = standardise_syntax(context_property),
      #value = standardise_syntax(value),
      #description = standardise_syntax(description),
      value = ifelse(
        is.na(description),
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
  
  if (any(stringr::str_detect(names(location_latlon), "latitude "))) {
    combined_table <-
      database$traits %>%
      dplyr::left_join(location_latlon, by = c("dataset_id", "location_id"))
  } else {
    combined_table <-
      database$traits %>%
      dplyr::mutate(
        location_name = NA_character_,
        `latitude (deg)` = NA_character_, 
        `longitude (deg)` = NA_character_,
        )
  }
  
  combined_table <- combined_table %>%
    dplyr::left_join(location_properties, by = c("dataset_id", "location_id", "location_name")) %>%
    join_contexts(contexts_tmp) %>%
    dplyr::left_join(
      database$methods %>% dplyr::select(-dplyr::all_of(c("data_collectors"))),
      by = c("dataset_id", "trait_name", "method_id")
    ) %>%
    dplyr::left_join(contributors, by = c("dataset_id")) %>%
    dplyr::left_join(database$taxa, by = c("taxon_name")) %>%
    dplyr::left_join(database$taxonomic_updates, by = c("taxon_name", "dataset_id", "original_name"))
  
  combined_table
}


## XX - TODO - we might not use this afterall. Retain for now, but currently not used above

#' Standardise syntax
#' @description
#' Replace syntax that are reserved as hooks in the packed columns in the combined table output for traits.build databases.
#' 
#'
#' @param data dataframe that requires syntax standardisation
#' @param column_name column that requires syntax standardisation
#'
#' @return
#' @noRd
#'
#' @examples
standardise_syntax <- function(column_name) {
  
  f <- function(x, find, replace) {
    gsub(find, replace, x, perl = TRUE)
  }
    
  column_name %>%
      f("=", "-") %>%
      f(":", "-") %>%
      f(";", ",") %>%
      f("<", "(") %>%
      f(">", ")") %>%
      f("\\|", ",")
}
