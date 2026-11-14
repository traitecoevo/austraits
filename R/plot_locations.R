#' @title Produce location maps of trait values
#' @description Plot location where trait data was collected from, by default
#'   over a map of Australia. This calls [traits.use::plot_locations()] with
#'   the map of Australia as its `basemap`.
#' @param database traits.build database OR traits table from a traits.build database. Note location details must be joined. See join_location_coordinates and examples
#' @param feature grouping/classification categories e.g trait_name, collection_type for <= v3.0.2, basis of record for >3.0.2
#' @param ... arguments passed to ggpointdensity::geom_pointdensity()
#' @param basemap `"australia"` (the default) draws the locations over a map
#' of Australia, and leaves out any location beyond the map, with a message
#' saying how many records that affects; if no location lies on the map, it
#' stops with an error. `NULL` draws every location without a map, for
#' databases from outside Australia. A data frame of map cells (numeric
#' columns `x` and `y`) draws the locations over that map instead; see
#' [traits.use::plot_locations()].
#' @author Dony Indiarto - d.indiarto@student.unsw.edu.au
#' @return ggplot of sites
#' @examples
#' \dontrun{
#' #All traits from a given study
#' data <- austraits %>% extract_dataset(dataset_id = "Falster_2003") %>% join_location_coordinates()
#' data %>% plot_locations("trait_name")
#'
#' #Single trait
#' data <- austraits %>% extract_trait(trait_names = c("plant_height")) %>% join_location_coordinates()
#' data$traits %>% plot_locations("trait_name")
#'
#' #Every location, without the map of Australia
#' data$traits %>% plot_locations("trait_name", basemap = NULL)
#' }
#' @export

plot_locations <- function(database, feature = "trait_name", ..., basemap = "australia"){
  if(is.character(basemap)){
    if(!identical(basemap, "australia")){
      cli::cli_abort("`basemap` must be \"australia\", NULL or a data frame of map cells")
    }
    basemap <- australia_map_raster
  }

  traits.use::plot_locations(database, feature = feature, ..., basemap = basemap)
}
