#' Bind multiple traits.build data objects into a single data object
#'
#' `bind_databases` binds all the listed studies into a single traits.build
#' database object as a large list.
#'
#' The combined `sources` hold the references of every database, with
#' duplicate keys removed. They are a [RefManageR::BibEntry()] object with the
#' class [`traits_build_sources`][traits_build_sources], so a reference can be
#' looked up by key with `$` as well as `[[` and `[`, e.g.
#' `database$sources$Falster_2005`.
#'
#' @param ... Arguments passed to other functions
#' @param databases List of traits.build databases to be bond together
#'
#' @return Compiled database as a single large list
#' @seealso [traits_build_sources] for looking up `sources` by key
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
  
  # Lets `$` look up by key; see ?traits_build_sources. Applied here, after
  # the c(), because RefManageR's c() returns a plain BibEntry when the first
  # database's sources lack the class
  sources <- as_traits_build_sources(sources)
  
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

  ret <- list(traits = combine("traits", databases) %>% dplyr::arrange(.data$dataset_id, .data$observation_id, .data$trait_name),
              locations = combine("locations", databases) %>% dplyr::arrange(.data$dataset_id, .data$location_id),
              contexts = combine("contexts", databases) %>% dplyr::arrange(.data$dataset_id, .data$category),
              methods = combine("methods", databases) %>% dplyr::arrange(.data$dataset_id, .data$trait_name),
              excluded_data = combine("excluded_data", databases) %>% dplyr::arrange(.data$dataset_id, .data$observation_id, .data$trait_name),
              taxonomic_updates = taxonomic_updates,
              taxa = combine("taxa", databases) %>% dplyr::distinct() %>% dplyr::arrange(.data$taxon_name),
              identifiers = combine("identifiers", databases) %>% dplyr::distinct(),
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
