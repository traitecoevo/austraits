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
  
  location_latlon <-
    database$locations %>%
    dplyr::filter(location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    tidyr::pivot_wider(names_from = location_property, values_from = value)
  
  location_properties <-
    database$locations %>%
    dplyr::filter(!location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    dplyr::mutate(
      location_property = stringr::str_replace_all(location_property, "=", "-"),
      value = stringr::str_replace_all(value, "=", "-"),
      location_property = stringr::str_replace_all(location_property, ";", ","),
      value = stringr::str_replace_all(value, ";", ",")
    ) %>%
    dplyr::mutate(location_properties = paste0(location_property, "=", value)) %>%
    dplyr::select(dplyr::all_of(c("dataset_id", "location_id", "location_name", "location_properties"))) %>%
    dplyr::group_by(dataset_id, location_id, location_name) %>%
    dplyr::mutate(location_properties = paste0(location_properties, collapse = ";")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  contributors <-
    database$contributors %>%
    dplyr::mutate(
      affiliation = stringr::str_replace_all(affiliation, ":", "-"),
      affiliation = stringr::str_replace_all(affiliation, ";", ","),
      affiliation = stringr::str_replace_all(affiliation, "<", "("),
      affiliation = stringr::str_replace_all(affiliation, ">", ")"),
      additional_role = stringr::str_replace_all(additional_role, "<", "("),
      additional_role = stringr::str_replace_all(additional_role, ">", ")"),
      data_collectors = paste0(given_name, " ", last_name),
      data_collectors = ifelse(
        !is.na(ORCID),
        paste0(data_collectors, " <ORCID:", ORCID),
        data_collectors),
      data_collectors = ifelse(
        is.na(ORCID),
        paste0(data_collectors, " <affiliation=", affiliation),
        paste0(data_collectors, ";affiliation=", affiliation)),
      data_collectors = ifelse(
        !is.na(additional_role),
        paste0(data_collectors, ";additional_role:", additional_role, ">"),
        paste0(data_collectors, ">"))
    ) %>%
    dplyr::select(-dplyr::all_of(c("last_name", "given_name", "ORCID", "affiliation", "additional_role"))) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(data_collectors = paste0(data_collectors, collapse = "; ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  contexts_tmp <-
    database$contexts %>%
    dplyr::mutate(
      context_property = stringr::str_replace_all(context_property, "=", "-"),
      value = stringr::str_replace_all(value, "=", "-"),
      description = stringr::str_replace_all(description, "=", "-"),
      context_property = stringr::str_replace_all(context_property, ";", ","),
      value = stringr::str_replace_all(value, ";", ","),
      description = stringr::str_replace_all(description, ";", ","),
      value = ifelse(
        is.na(description),
        paste0(context_property, ":", value),
        paste0(context_property, ":", value, " <", description, ">"))
    ) %>%
    dplyr::select(-dplyr::all_of(c("description", "context_property", "category"))) %>%
    tidyr::separate_longer_delim(link_vals, ", ") %>%
    distinct() %>%
    group_by(dataset_id, link_id, link_vals) %>%
    mutate(value = paste0(value, collapse = ";")) %>%
    distinct() %>%
    ungroup()
  
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
  
  combined_table <-
    database$traits %>%
    dplyr::left_join(location_latlon, by = c("dataset_id", "location_id")) %>%
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
