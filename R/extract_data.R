#' Extract data from traits.build database
#' 
#' @description Function to extract data from a traits.build database based on 
#' any value(s) from any column in the traits, locations, contexts, methods, 
#' taxa, taxonomic_updates, and contributors tables.
#' The output a traits.build formatted database with all tables subset 
#' based on the specified table, column (variable) and column value.
#'
#' @param database traits.build database (list object)
#' @param table Table within a traits.build database
#' @param col Column name within the specified table.
#' @param col_value Value (of column, from with a table) that is used to subset database. This can be a single value or a vector. It includes partial string matches. 
#' @importFrom rlang :=
#' @return subset traits.build database
#' @export
#'
#' @examples 
#' \dontrun{
#' extract_data(database = traits.build_database, table = "traits", 
#' col = "trait_name", col_value = "leaf_area")
#' }
extract_data <- function(database, table = NA, col, col_value) {
  
  # Check missingness
  check_arg_missingness(database, col, col_value)
  
  # Check compatability
  status <- check_compatibility(database, single_table_allowed = TRUE)
  
  # If compatible
  if(!status){
    function_not_supported(database)
  }

  # Check table value is valid 
  check_table_name_exists(database, table)
  
  # If just the traits table is read in
  if (tibble::is_tibble(database)) {
    
    check_col_exists_in_table(database, table, col)
    
    indicies_tmp <- purrr::map(col_value, ~{
      stringr::str_which(database[[col]], 
                         pattern = stringr::regex(.x, ignore_case = TRUE))
    })
    
    found_indicies <- purrr::reduce(indicies_tmp, union)
    
    # Trim traits, based on the columns identified
    ret <- database %>%
      dplyr::slice(found_indicies)
    
    check_col_value_exists(ret, table, col, col_value)

  # If a full traits.build database is read in
  } else {
    
    # Check if col exists in table within database
    check_col_exists_in_table(database, table, col)

    # Proceed to extraction
    database$contexts <- database$contexts %>% tidyr::separate_longer_delim(link_vals, delim = ", ")
  
    database$contexts_tmp <- split(database$contexts, database$contexts$link_id)
    
    empty_tibble <- dplyr::tibble(
      dataset_id = character(),
      context_property = character(),
      category = character(),
      value = character(),
      description = character(),
      link_id = character(),
      link_vals  = character()
    )
    
    # Create an empty database list
    ret <- list( 
      locations = dplyr::tibble(),
      entity_context_id = dplyr::tibble(),
      method_context_id = dplyr::tibble(),
      temporal_context_id = dplyr::tibble(),
      plot_context_id = dplyr::tibble(),
      treatment_context_id = dplyr::tibble(),
      methods = dplyr::tibble(),
      taxa = dplyr::tibble(),
      taxonomic_updates = dplyr::tibble(),
      contributors = dplyr::tibble(),
      traits = dplyr::tibble(),
      excluded_data = dplyr::tibble(),
      contexts = dplyr::tibble()
    )
    
    ret_tmp <- ret[1:10]
  
    # Cookie cutters
    cookie_cutters <- list(
      locations_cc = c("dataset_id", "location_id"),
      entity_contexts_cc = c("dataset_id", "entity_context_id"),
      method_contexts_cc = c("dataset_id", "method_context_id"),
      temporal_contexts_cc = c("dataset_id", "temporal_context_id"),
      plot_contexts_cc = c("dataset_id", "plot_context_id"),
      treatment_contexts_cc = c("dataset_id", "treatment_context_id"),
      methods_cc = c("dataset_id", "trait_name", "method_id"),
      taxa_cc = c("taxon_name"),
      taxonomic_updates_cc = c("dataset_id", "taxon_name", "original_name"),
      contributors_cc = c("dataset_id")
    )
    
    # Create table of various look-up values to be used below
    
    # Create additional vectors for table 
    tables_to_cut <- c("locations", "entity_context_id", "method_context_id", "temporal_context_id", 
                              "plot_context_id", "treatment_context_id", 
                              "methods", "taxa", "taxonomic_updates", "contributors")
    
    tables_complete_path <- c("database$locations", "database$entity_context_id", 
                              "database$method_context_id", "database$temporal_context_id", 
                              "database$plot_context_id", "database$treatment_context_id", 
                              "database$methods", "database$taxa", "database$taxonomic_updates", "database$contributors")
    
    # Create table
    tables <- dplyr::tibble(
      cookie_cutters = names(cookie_cutters),
      tables_to_cut = tables_to_cut,
      tables_complete_path = tables_complete_path
    )
    
    # For any context property categories that do not exist, create empty tibbles.
    for (v in c("entity_context_id", "method_context_id", "temporal_context_id", "plot_context_id", "treatment_context_id")) { 
      if (is.null(database$contexts_tmp[[v]])) {
        database$contexts_tmp[[v]] <- empty_tibble
      } 
    }
    
    # Rename the generic `link_vals` to the specific context category they represent and 
    # move the tables from database_tmp to the main database list.
    
    for (z in c("entity_context_id", "method_context_id", "temporal_context_id", 
                "plot_context_id", "treatment_context_id")) {
      database[[z]] <- database$contexts_tmp[[z]] %>%
        dplyr::rename(!!z := link_vals)
    }
    
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
        
        indicies_tmp <- purrr::map(col_value, ~{
          stringr::str_which(database[[table[[i]]]][[col]], 
                             pattern = stringr::regex(.x, ignore_case = TRUE))
        })
        
        found_indicies <- purrr::reduce(indicies_tmp, union)
    
        # Trim traits, based on the columns identified as being common between the traits table and target table
        cc_traits <- database[[table[[i]]]] %>%
          dplyr::slice(found_indicies) %>%
          dplyr::select(tidyselect::all_of(columns_to_select)) %>% 
          dplyr::distinct()
        
        # Filtering join
        ## It will quite literally cookie cutting the traits table if the columns match what is in cc_traits
        ret_tmp[["traits"]] <- database[["traits"]]%>% 
          dplyr::semi_join(cc_traits, by = columns_to_select)
        
        columns_to_select_excluded <- intersect(setdiff(names(database$excluded_data), "value"), names(database[[table[[i]]]]))
        
        # Use same filtering join to trim excluded data
        ret_tmp[["excluded_data"]] <- database[["excluded_data"]]  %>% 
          dplyr::semi_join(cc_traits, by = columns_to_select_excluded)
        
        
        for (j in seq_along(tables_tmp$tables_to_cut)) {
        
          cut_traits <- ret_tmp[["traits"]] %>% 
            dplyr::select(cookie_cutters[[j]]) %>%
            dplyr::distinct() 
          
          cut_traits <- cut_traits %>%
            dplyr::filter(dplyr::if_all(tidyselect::everything(), ~ !is.na(.)))
          
          cut_table <- eval(parse(text = tables_tmp$tables_complete_path[[j]])) %>%
            dplyr::semi_join(cut_traits, by = cookie_cutters[[j]]) %>%
            dplyr::rename(link_vals = tidyselect::contains("context_id"))
          
          ret_tmp[[j]] <- cut_table
        
        }
        
         for (v in seq_along(c(tables$tables_to_cut, "traits", "excluded_data"))) {
         
            ret[[v]] <- ret[[v]] %>%
              dplyr::bind_rows(ret_tmp[[v]]) %>%
              dplyr::distinct()
         
         }
        
        if (database[["excluded_data"]] == 0) {
            database[["excluded_data"]] <- database[["traits"]][0,]
        }
        
      }
    
    # Rejoin contexts
    ret[["contexts"]] <- ret[["entity_context_id"]] %>%
      dplyr::bind_rows(ret[["method_context_id"]],
                ret[["plot_context_id"]],
                ret[["temporal_context_id"]],
                ret[["treatment_context_id"]]) %>%
      dplyr::select(-dplyr::any_of(c("entity_context_id", "method_context_id", "plot_context_id", "temporal_context_id", "treatment_context_id"))) %>%
      dplyr::group_by(dataset_id, category, link_id, value, description) %>%
        dplyr::mutate(link_vals = paste0(link_vals, collapse = ", ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
    
    ret <- ret[!names(ret) %in% c("entity_context_id", "method_context_id", "plot_context_id", "temporal_context_id", "treatment_context_id")]
    
      # Trim sources - Are these just dataset_ids...
    from_methods_to_sources_cc <- dplyr::union(ret$methods$source_primary_key,  # Is this part really needed, aren't these just dataset_ids? 
                                               ret$methods$source_secondary_key %>% strsplit("; ") %>% unlist()) %>% 
      unique() %>% stats::na.omit() %>% as.character()
    
    ret[["sources"]] <- database[["sources"]][from_methods_to_sources_cc]
    
    # Join in other metadata tables
    ret[["definitions"]] <- database[["definitions"]]
    ret[["schema"]] <- database[["schema"]]
    ret[["metadata"]] <- database[["metadata"]]
    ret[["build_info"]] <- database[["build_info"]]
    
    # Reorder list to match database
    ret <- ret[c("traits", "locations", "contexts", "methods", "excluded_data", "taxonomic_updates", 
                 "taxa","contributors","sources","definitions","schema", "metadata","build_info")]

  }
  
  # Check full database is provided, assign class
  if(!tibble::is_tibble(ret)){
    
    # Check if extraction was successful based on col value
    check_col_value_exists(ret, table, col, col_value)
    
    # Assign class
    attr(ret, "class") <- "traits.build"
  }
    
    ret
 
}



