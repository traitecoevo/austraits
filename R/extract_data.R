#' Extract data from traits.build database
#' 
#' @description Function to extract data from a traits.build database based on 
#' any value(s) from any column in the traits, locations, contexts, methods, 
#' taxa, taxonomic_updates, and contributors tables.
#' The output a traits.build formatted database with all tables subset 
#' based on the specified table, column (variable) and column value.
#'
#' @param database traits.build database
#' @param table Table within a traits.build database
#' @param col Column name within the specified table.
#' @param col_value Value (of column, from with a table) that is used to subset database. This can be a single value or a vector. It includes partial string matches. 
#'
#' @return subset traits.build database
#' @export
#'
#' @examples 
#' \dontrun{
#' extract_data(database = traits.build_database, table = "traits", 
#' col = "trait_name", col_value = "leaf_area")
#' }
extract_data <- function(database, table, col, col_value) {
  
  database$contexts <- database$contexts %>% tidyr::separate_longer_delim(link_vals, delim = ", ")

  database$contexts_tmp <- split(database$contexts, database$contexts$link_id)
  
  empty_tibble <- dplyr::tibble(
    dataset_id = NA_character_,
    context_property = NA_character_,
    category = NA_character_,
    value = NA_character_,
    description = NA_character_,
    link_id = NA_character_,
    link_vals  = NA_character_
  )

  if (is.null(database$contexts_tmp$entity_context_id)) {
    database$contexts_tmp$entity_context_id <- empty_tibble
  }
  
  if (is.null(database$contexts_tmp$method_context_id)) {
    database$contexts_tmp$method_context_id <- empty_tibble
  }
  
  if (is.null(database$contexts_tmp$temporal_context_id)) {
    database$contexts_tmp$temporal_context_id <- empty_tibble
  }
  
  if (is.null(database$contexts_tmp$plot_context_id)) {  
    database$contexts_tmp$plot_context_id <- empty_tibble
  }
  
  if (is.null(database$contexts_tmp$treatment_context_id)) {
    database$contexts_tmp$treatment_context_id <- empty_tibble
  }
  
  database$entity_context_id <- database$contexts_tmp$entity_context_id %>%
    dplyr::rename(entity_context_id = link_vals)
  
  database$method_context_id <- database$contexts_tmp$method_context_id %>%
    dplyr::rename(method_context_id = link_vals)  
  
  database$temporal_context_id <- database$contexts_tmp$temporal_context_id %>%
    dplyr::rename(temporal_context_id = link_vals)
  
  database$plot_context_id <- database$contexts_tmp$plot_context_id %>%
    dplyr::rename(plot_context_id = link_vals)
  
  database$treatment_context_id <- database$contexts_tmp$treatment_context_id %>%
    dplyr::rename(treatment_context_id = link_vals)

  
  tables_to_cut <- c("locations", "entity_context_id", "method_context_id", "temporal_context_id", 
                            "plot_context_id", "treatment_context_id", 
                            "methods", "taxa", "taxonomic_updates", "contributors")
  
  tables_complete_path <- c("database$locations", "database$entity_context_id", 
                            "database$method_context_id", "database$temporal_context_id", 
                            "database$plot_context_id", "database$treatment_context_id", 
                            "database$methods", "database$taxa", "database$taxonomic_updates", "database$contributors")
  
  # Empty list
  ret <- list(
    traits = dplyr::tibble(), 
    locations = dplyr::tibble(),
    contexts = dplyr::tibble(),
    entity_context_id = dplyr::tibble(),
    method_context_id = dplyr::tibble(),
    temporal_context_id = dplyr::tibble(),
    plot_context_id = dplyr::tibble(),
    treatment_context_id = dplyr::tibble(),
    methods = dplyr::tibble(),
    excluded_data = dplyr::tibble(),
    taxonomic_updates = dplyr::tibble(),
    taxa = dplyr::tibble(),
    contributors = dplyr::tibble()
  )
  
  ret_tmp <- list()

  # Cookie cutters
  locations_cc <- c("dataset_id", "location_id")
  entity_contexts_cc <- c("dataset_id", "entity_context_id")
  temporal_contexts_cc <- c("dataset_id", "temporal_context_id")
  method_contexts_cc <- c("dataset_id", "method_context_id")
  plot_contexts_cc <- c("dataset_id", "plot_context_id")
  treatment_contexts_cc <- c("dataset_id", "treatment_context_id")
  methods_cc <- c("dataset_id", "trait_name", "method_id")
  taxonomic_updates_cc <- c("dataset_id", "taxon_name", "original_name")
  taxa_cc <- c("taxon_name")
  contributors_cc <- c("dataset_id")
  
  cookie_cutters <- c("locations_cc", "entity_contexts_cc", "method_contexts_cc", "temporal_contexts_cc", "plot_contexts_cc", "treatment_contexts_cc", 
                      "methods_cc", "taxa_cc", "taxonomic_updates_cc", "contributors_cc")
  
  # Create a table of various look-ups used below
  tables <- dplyr::tibble(
    cookie_cutters = cookie_cutters,
    tables_to_cut = tables_to_cut,
    tables_complete_path = tables_complete_path
  )
  
  # If the context table is queried need to convert the word "contexts" into a vector that indicates which of the 5 context categories have matches
  if (table == "contexts") {

    table_tmp <- database$contexts %>% 
      dplyr::filter(stringr::str_detect(database$contexts[[col]], col_value)) %>%
      dplyr::distinct(link_id)
    
    table <- as.vector(table_tmp[[1]])

    if (length(table) == 0) {
      table <-  as.vector("treatment_context_id")
    }
  
  }
  
  for (i in seq_along(1:length(table))) {
    
      tables_tmp <- tables
      
      # chose columns to select, ensuring "value" isn't among the columns, since it has a different meaning for each table
      columns_to_select <- intersect(setdiff(names(database$traits), "value"), names(database[[table[[i]]]]))
      
      # Trim reference table i.e. locations with pattern matching
      #found_indicies <- database[[table[[i]]]][[col]] |> stringr::str_which(pattern = stringr::regex(col_value, ignore_case = TRUE))
      
      indicies_tmp <- purrr::map(col_value, ~{
        stringr::str_which(database[[table[[i]]]][[col]], 
                           pattern = stringr::regex(.x, ignore_case = TRUE))
      })
      
      found_indicies <- purrr::reduce(indicies_tmp, union)
  
      # Trim traits, based on the columns identified as being common between the traits table and target table
      cc_traits <- database[[table[[i]]]] |>
        dplyr::slice(found_indicies) |>
        dplyr::select(tidyselect::all_of(columns_to_select)) |> 
        dplyr::distinct()
      
      # Filtering join
      ## It will quite literally cookie cutting the traits table if the columns match what is in cc_traits
      ret_tmp[["traits"]] <- database[["traits"]]|> 
        dplyr::semi_join(cc_traits, by = columns_to_select)
      
      # Use same filtering join to trim excluded data
      ret_tmp[["excluded_data"]] <- database[["excluded_data"]]  |> 
        dplyr::semi_join(cc_traits, by = columns_to_select)
      
      
      for (j in seq_along(tables_tmp$tables_to_cut)) {
      
        cut_traits <- ret_tmp[["traits"]] |> 
          dplyr::select(get(tables_tmp$cookie_cutters[[j]])) |> 
          dplyr::distinct() 
        
        cut_traits <- cut_traits |>
          dplyr::filter(dplyr::if_all(tidyselect::everything(), ~ !is.na(.)))
        
        cut_table <- eval(parse(text = tables_tmp$tables_complete_path[[j]])) |>
          dplyr::semi_join(cut_traits, by = get(tables_tmp$cookie_cutters[[j]])) %>%
          dplyr::rename(link_vals = tidyselect::contains("context_id"))
        
        assign(paste0("ret_tmp[[\"", tables_tmp$tables_to_cut[[j]], "\"]]"), cut_table )
      
      }
      
      # Bind together rows extracted from each table
      ## This step is required when the `context` table is queried as it is split into 5 tables and each needs to be checked.
      ret[["traits"]] <- ret[["traits"]] %>% dplyr::bind_rows(ret_tmp[["traits"]]) %>% dplyr::distinct()
      ret[["locations"]] <- ret[["locations"]] %>% dplyr::bind_rows(`ret_tmp[["locations"]]`) %>% dplyr::distinct()
      ret[["entity_context_id"]] <- ret[["entity_context_id"]] %>% dplyr::bind_rows(`ret_tmp[["entity_context_id"]]`) %>% dplyr::distinct()
      ret[["method_context_id"]] <- ret[["method_context_id"]] %>% dplyr::bind_rows(`ret_tmp[["method_context_id"]]`) %>% dplyr::distinct()
      ret[["temporal_context_id"]] <- ret[["temporal_context_id"]] %>% dplyr::bind_rows(`ret_tmp[["temporal_context_id"]]`) %>% dplyr::distinct()
      ret[["plot_context_id"]] <- ret[["plot_context_id"]] %>% dplyr::bind_rows(`ret_tmp[["plot_context_id"]]`) %>% dplyr::distinct()
      ret[["treatment_context_id"]] <- ret[["treatment_context_id"]] %>% dplyr::bind_rows(`ret_tmp[["treatment_context_id"]]`) %>% dplyr::distinct()
      ret[["methods"]] <- ret[["methods"]] %>% dplyr::bind_rows(`ret_tmp[["methods"]]`) %>% dplyr::distinct()
      ret[["excluded_data"]] <- ret[["excluded_data"]] %>% dplyr::bind_rows(ret_tmp[["excluded_data"]]) %>% dplyr::distinct()
      ret[["taxonomic_updates"]] <- ret[["taxonomic_updates"]] %>% dplyr::bind_rows(`ret_tmp[["taxonomic_updates"]]`) %>% dplyr::distinct()
      ret[["taxa"]] <- ret[["taxa"]] %>% dplyr::bind_rows(`ret_tmp[["taxa"]]`) %>% dplyr::distinct()
      ret[["contributors"]] <- ret[["contributors"]] %>% dplyr::bind_rows(`ret_tmp[["contributors"]]`) %>% dplyr::distinct()
      
    }
  
  # Rejoin contexts
  ret[["contexts"]] <- ret[["entity_context_id"]] |>
    dplyr::bind_rows(ret[["method_context_id"]],
              ret[["plot_context_id"]],
              ret[["temporal_context_id"]],
              ret[["treatment_context_id"]]) |>
    dplyr::select(-dplyr::any_of(c("entity_context_id", "method_context_id", "plot_context_id", "temporal_context_id", "treatment_context_id"))) |>
    dplyr::group_by(dataset_id, category, link_id, value, description) |>
      dplyr::mutate(link_vals = paste0(link_vals, collapse = ", ")) |>
    dplyr::ungroup() |>
    dplyr::distinct()
  
  ret <- ret[!names(ret) %in% c("entity_context_id", "method_context_id", "plot_context_id", "temporal_context_id", "treatment_context_id")]
  
    # Trim sources - Are these just dataset_ids...
  from_methods_to_sources_cc <- dplyr::union(ret$methods$source_primary_key,  # Is this part really needed, aren't these just dataset_ids? 
                                             ret$methods$source_secondary_key |> strsplit("; ") |> unlist()) |> 
    unique() |> stats::na.omit() |> as.character()
  
  ret[["sources"]] <- database[["sources"]][from_methods_to_sources_cc]
  
  # Join in other metadata tables
  ret[["definitions"]] <- database[["definitions"]]
  ret[["schema"]] <- database[["schema"]]
  ret[["metadata"]] <- database[["metadata"]]
  ret[["build_info"]] <- database[["build_info"]]
  
  # Reorder list to match database
  ret <- ret[c("traits", "locations", "contexts", "methods", "excluded_data", "taxonomic_updates", 
               "taxa","contributors","sources","definitions","schema", "metadata","build_info")]
  
  # Assign class
  attr(ret, "class") <- "austraits"
  
  ret
  
}



