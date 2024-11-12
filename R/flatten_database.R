#' Create combined traits.build table
#'
#' Create a single database output that merges together the information
#' in all relational tables within a traits.build database.
#' Trait measurements are still output in long format (1 row per trait value),
#' but all measurement-related metadata (methods, location properties, context properties, contributors)
#' are now included as additional columns in a single table.
#'
#' @param database traits.build database (list object)
#' @param format A parameter for the locations, contexts and data contributors tables specifying how data are packed. 
#' All three can be formatted as a single compacted column(s) will have a human readable column ("single_column_pretty")
#' or using json ("single_column_json") syntax. For location properties or context properties  there is also 
#' the option to add each `location_property` or `context_property` to the traits table as its own column ("many_columns");
#' the contributors column defaults to "single_column_pretty" when this option is selected.
#' @param vars List specifying which columns or properties to include from each table. The detail is for all columns/properties to be included.
#' @param include_description A logical indicating whether to include (TRUE) or omit (FALSE) the context_property descriptions; defaults to TRUE.
#'
#' @return A table combining information in 7 traits.build relational tables: traits, locations, contexts, methods, taxa, taxonomic_updates, and contributors
#' @export
#'
#' @usage flatten_database(database, format, vars, include_description)
#' 
flatten_database <- function(database,
    format = "single_column_pretty",
    vars = list(
      location = "all",
      context = "all",
      contributors = "all",
      taxonomy = "all",
      taxonomic_updates = "all",
      methods = setdiff(names(database$methods), c("data_collectors"))
    ),
    include_description = TRUE
  ) {
  # Since `data_collectors` is also merged into the combined_table via the contributors tibble, we don't want the information twice.

  if (format == "many_columns") {
    format_contributors = "single_column_pretty"
  } else {
    format_contributors = format
  }

  combined_table_relational <- database %>%
    join_location_coordinates() %>%
    join_location_properties(format = format, vars =  vars$location) %>%
    join_context_properties(format = format, vars =  vars$context, include_description = TRUE) %>%
    join_methods(vars = vars$methods) %>%
    join_contributors(format = format_contributors, vars = vars$contributors) %>%
    join_taxa(vars = vars$taxonomy) %>%
    join_taxonomic_updates(vars = vars$taxonomic_updates)
  
  combined_table <- combined_table_relational$traits
}
