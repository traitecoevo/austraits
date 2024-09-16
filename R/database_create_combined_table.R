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

  # Since `data_collectors` is also merged into the combined_table via the contributors tibble, we don't want the information twice.
  method_vars <- setdiff(names(austraits$methods), c("data_collectors"))
    
  combined_table <- database %>%
    join_location_coordinates() %>%
    join_location_properties(format = "single_column_pretty", vars =  "all") %>%
    join_context_properties(format = "single_column_pretty", vars =  "all", include_description = TRUE) %>%
    join_methods(vars = method_vars)  %>%
    join_contributors(format = "single_column_pretty", vars = "all")  %>%
    join_taxonomy(vars = "all")  %>%
    join_taxonomic_updates(vars = "all")
  
  combined_table
}
