#' Bind multiple traits.build data objects into a single data object
#'
#' `bind_databases` binds all the listed studies into a single traits.build
#' database object as a large list.
#'
#' @param ... Arguments passed to other functions
#' @param databases List of traits.build databases to be bond together
#'
#' @return Compiled database as a single large list
#' @importFrom rlang .data
#' @export
bind_databases <- function(..., databases = list(...)) {
  
  combine <- function(name, databases) {
    
    # Check for null elements and remove them
    databases <- databases[!sapply(databases, is.null)]
    
    # No data in any parts
    if(length(databases) == 0) {
      return(NULL)
    }

    databases %>%
    lapply("[[", name) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()
  }
  
  # Bind sources and remove duplicates
  sources <- databases %>% lapply("[[", "sources")
  keys <- sources %>% lapply(names) %>% unlist() %>% unique() %>% sort()
  sources <- sources %>% purrr::reduce(c)
  sources <- sources[keys]
  
  definitions <- databases %>% lapply("[[", "definitions") %>% purrr::reduce(c)
  definitions <- definitions[!duplicated(names(definitions))]
  definitions <- definitions[sort(names(definitions))]
  
  # Drop null datasets
  databases[sapply(databases, is.null)] <- NULL
  
  # Taxonomy
  
  taxonomic_updates <-
    combine("taxonomic_updates", databases) %>%
    dplyr::group_by(.data$original_name, .data$aligned_name, .data$taxon_name, .data$taxonomic_resolution) %>%
    #dplyr::mutate(dataset_id = paste(.data$dataset_id, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$original_name, .data$aligned_name, .data$taxon_name, .data$taxonomic_resolution)
  
  # Metadata
  contributors <- combine("contributors", databases)
  metadata <- databases[[1]][["metadata"]]
  
  metadata[["contributors"]] <-
    contributors %>%
    dplyr::select(-dplyr::any_of(c("dataset_id", "additional_role"))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$last_name, .data$given_name) %>%
    convert_df_to_list()
  
  # Modify this to allow any dataset to have identifiers, not just the first one
  identifiers <- databases[[1]][["identifiers"]]
  if (!is.null(identifiers)) {
    identifiers <- combine("identifiers", databases) %>% dplyr::distinct()
  }
  
  ret <- list(traits = combine("traits", databases) %>% dplyr::arrange(.data$dataset_id, .data$observation_id, .data$trait_name),
              locations = combine("locations", databases) %>% dplyr::arrange(.data$dataset_id, .data$location_id),
              contexts = combine("contexts", databases) %>% dplyr::arrange(.data$dataset_id, .data$category),
              methods = combine("methods", databases) %>% dplyr::arrange(.data$dataset_id, .data$trait_name),
              excluded_data = combine("excluded_data", databases) %>% dplyr::arrange(.data$dataset_id, .data$observation_id, .data$trait_name),
              taxonomic_updates = taxonomic_updates,
              taxa = combine("taxa", databases) %>% dplyr::distinct() %>% dplyr::arrange(.data$taxon_name),
              identifiers = identifiers,
              contributors = contributors,
              sources = sources,
              definitions = definitions,
              schema = databases[[1]][["schema"]],
              metadata = metadata,
              build_info = list(session_info = utils::sessionInfo())
  )
  
  class(ret) <- c("traits.build")
  
  ret
}
