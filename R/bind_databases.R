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
  check_conflicting_sources(sources, databases)
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

#' Warn if one citation key names different references in different databases
#'
#' `sources` is bound by key, keeping one entry per key, so two different
#' references sharing a key lose one of them unseen -- and with it the citation
#' of every record whose `source_id` names it. Entries are the same reference
#' when their titles and years agree, ignoring case, spacing and punctuation, so
#' one paper formatted differently by two datasets is not reported.
#'
#' A warning rather than an error: such keys exist in published databases, and
#' binding has always kept the first entry, so stopping would break every build
#' until the data are fixed.
#'
#' @param sources List of the `sources` of each database
#' @param databases List of traits.build databases, to name the datasets
#' @return Nothing; warns, listing the keys and datasets, on a conflict
#' @noRd
#' @keywords internal
check_conflicting_sources <- function(sources, databases) {

  keep <- !vapply(sources, is.null, logical(1))
  sources <- sources[keep]
  databases <- databases[keep]
  keys <- lapply(sources, names)

  counts <- table(unlist(lapply(keys, unique)))
  shared <- names(counts)[counts > 1]
  if (length(shared) == 0) return(invisible())

  identity <- function(entry) {
    field <- function(x) if (is.null(x)) "" else paste(unlist(x), collapse = " ")
    tolower(gsub("[^[:alnum:]]", "", paste(field(entry$title), field(entry$year))))
  }

  conflicts <- purrr::map_chr(shared, function(key) {
    holders <- which(vapply(keys, function(k) key %in% k, logical(1)))
    entries <- vapply(holders, function(i) {
      identity(sources[[i]][[match(key, keys[[i]])]])
    }, character(1))
    if (length(unique(entries)) == 1) return(NA_character_)
    datasets <- vapply(holders, function(i) {
      paste(unique(databases[[i]]$methods$dataset_id), collapse = ", ")
    }, character(1))
    sprintf("`%s` (%s)", key, paste(datasets, collapse = "; "))
  })
  conflicts <- conflicts[!is.na(conflicts)]

  if (length(conflicts) > 0) {
    warning(
      "These citation keys name different references in different datasets, ",
      "so `sources` keeps only one of each: ",
      paste(conflicts, collapse = ", "),
      ". Give each reference its own key.",
      call. = FALSE
    )
  }

  invisible()
}
