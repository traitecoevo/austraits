# Everything generic lives in traits.use. austraits re-exports all of
# traits.use's functions (except plot_locations(), which austraits wraps to
# draw a map of Australia), so code written for earlier versions of austraits,
# e.g. `austraits::extract_trait()`, keeps working.

#' @importFrom traits.use %>%
#' @export
traits.use::`%>%`

#' @importFrom traits.use as_wide_table
#' @export
traits.use::as_wide_table

#' @importFrom traits.use bind_databases
#' @export
traits.use::bind_databases

#' @importFrom traits.use bind_trait_values
#' @export
traits.use::bind_trait_values

#' @importFrom traits.use convert_df_to_list
#' @export
traits.use::convert_df_to_list

#' @importFrom traits.use convert_list_to_df1
#' @export
traits.use::convert_list_to_df1

#' @importFrom traits.use convert_list_to_df2
#' @export
traits.use::convert_list_to_df2

#' @importFrom traits.use extract_data
#' @export
traits.use::extract_data

#' @importFrom traits.use extract_dataset
#' @export
traits.use::extract_dataset

#' @importFrom traits.use extract_taxa
#' @export
traits.use::extract_taxa

#' @importFrom traits.use extract_trait
#' @export
traits.use::extract_trait

#' @importFrom traits.use flatten_database
#' @export
traits.use::flatten_database

#' @importFrom traits.use join_context_properties
#' @export
traits.use::join_context_properties

#' @importFrom traits.use join_contributors
#' @export
traits.use::join_contributors

#' @importFrom traits.use join_identifiers
#' @export
traits.use::join_identifiers

#' @importFrom traits.use join_location_coordinates
#' @export
traits.use::join_location_coordinates

#' @importFrom traits.use join_location_properties
#' @export
traits.use::join_location_properties

#' @importFrom traits.use join_methods
#' @export
traits.use::join_methods

#' @importFrom traits.use join_sources
#' @export
traits.use::join_sources

#' @importFrom traits.use join_taxa
#' @export
traits.use::join_taxa

#' @importFrom traits.use join_taxonomic_updates
#' @export
traits.use::join_taxonomic_updates

#' @importFrom traits.use load_database
#' @export
traits.use::load_database

#' @importFrom traits.use lookup_context_property
#' @export
traits.use::lookup_context_property

#' @importFrom traits.use lookup_location_property
#' @export
traits.use::lookup_location_property

#' @importFrom traits.use lookup_trait
#' @export
traits.use::lookup_trait

#' @importFrom traits.use plot_categorical_trait_distribution
#' @export
traits.use::plot_categorical_trait_distribution

#' @importFrom traits.use plot_site_locations
#' @export
traits.use::plot_site_locations

#' @importFrom traits.use plot_trait_distribution_beeswarm
#' @export
traits.use::plot_trait_distribution_beeswarm

#' @importFrom traits.use plot_trait_distribution
#' @export
traits.use::plot_trait_distribution

#' @importFrom traits.use plot_trait_distribution_jitter
#' @export
traits.use::plot_trait_distribution_jitter

#' @importFrom traits.use separate_trait_values
#' @export
traits.use::separate_trait_values

#' @importFrom traits.use summarise_database
#' @export
traits.use::summarise_database

#' @importFrom traits.use trait_pivot_longer
#' @export
traits.use::trait_pivot_longer

#' @importFrom traits.use trait_pivot_wider
#' @export
traits.use::trait_pivot_wider
