#' Bind multiple traits.build data objects into a single data object
#'
#' `bind_databases` binds all the listed studies into a single traits.build
#' database object as a large list.
#'
#' @param ... Arguments passed to other functions
#' @param List of traits.build databases to bind together (lists of tibbles)
#'
#' @return Compiled database as a single large list
#' @importFrom rlang .data
#' @export
bind_databases <- function(..., d = list(...)) {
  
  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name)) %>% distinct()
  }
  
  # Bind sources and remove duplicates
  sources <- d %>% lapply("[[", "sources")
  keys <- sources %>% lapply(names) %>% unlist() %>% unique() %>% sort()
  sources <- sources %>% purrr::reduce(c)
  sources <- sources[keys]
  
  definitions <- d %>% lapply("[[", "definitions") %>% purrr::reduce(c)
  definitions <- definitions[!duplicated(names(definitions))]
  definitions <- definitions[sort(names(definitions))]
  
  # Drop null datasets
  d[sapply(d, is.null)] <- NULL
  
  # XX - TODO - I think this line of code needs to change
  names(d) <- sapply(d, "[[", "dataset_id")
  
  # Taxonomy
  
  taxonomic_updates <-
    combine("taxonomic_updates", d) %>%
    dplyr::group_by(.data$original_name, .data$aligned_name, .data$taxon_name, .data$taxonomic_resolution) %>%
    dplyr::mutate(dataset_id = paste(.data$dataset_id, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$original_name, .data$aligned_name, .data$taxon_name, .data$taxonomic_resolution)
  
  # Metadata
  contributors <- combine("contributors", d)
  metadata <- d[[1]][["metadata"]]
  
  metadata[["contributors"]] <-
    contributors %>%
    dplyr::select(-dplyr::any_of(c("dataset_id", "additional_role"))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$last_name, .data$given_name) %>%
    util_df_to_list()
  
  ret <- list(traits = combine("traits", d) %>% dplyr::arrange(.data$dataset_id, .data$observation_id, .data$trait_name),
              locations = combine("locations", d) %>% dplyr::arrange(.data$dataset_id, .data$location_id),
              contexts = combine("contexts", d) %>% dplyr::arrange(.data$dataset_id, .data$category),
              methods = combine("methods", d) %>% dplyr::arrange(.data$dataset_id, .data$trait_name),
              excluded_data = combine("excluded_data", d) %>% dplyr::arrange(.data$dataset_id, .data$observation_id, .data$trait_name),
              taxonomic_updates = taxonomic_updates,
              taxa = combine("taxa", d) %>% dplyr::distinct() %>% dplyr::arrange(.data$taxon_name),
              contributors = contributors,
              sources = sources,
              definitions = definitions,
              schema = d[[1]][["schema"]],
              metadata = metadata,
              build_info = list(session_info = utils::sessionInfo())
  )
  
  class(ret) <- c("list", "traits.build")
  
  ret
}
