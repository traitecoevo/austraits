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
#' @usage flatten_database(database)
#' 
flatten_database <- function(austraits,
    format = "single_column_pretty",
    vars = list(
      location = "all",
      context = "all",
      contributors = "all",
      taxonomy = "all",
      taxonomic_updates = "all",
      methods = setdiff(names(austraits$methods), c("data_collectors"))
    ),
    include_description = TRUE
  ) {
  # Since `data_collectors` is also merged into the combined_table via the contributors tibble, we don't want the information twice.

  if (format == "many_columns") {
    format_contributors = "single_column_pretty"
  } else {
    format_contributors = format
  }

  combined_table_relational <- austraits %>%
    join_location_coordinates() %>%
    join_location_properties(format = format, vars =  vars$location) %>%
    join_context_properties(format = format, vars =  vars$context, include_description = TRUE) %>%
    join_methods(vars = vars$methods) %>%
    join_contributors(format = format_contributors, vars = vars$contributors) %>%
    join_taxa(vars = vars$taxonomy) %>%
    join_taxonomic_updates(vars = vars$taxonomic_updates)
  
  combined_table <- combined_table_relational$traits
}
