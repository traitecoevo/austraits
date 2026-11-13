# Produce location maps of trait values

Plot location where trait data was collected from

## Usage

``` r
plot_locations(database, feature = "trait_name", ..., basemap = "australia")
```

## Arguments

- database:

  traits.build database OR traits table from a traits.build database.
  Note location details must be joined. See join_location_coordinates
  and examples

- feature:

  grouping/classification categories e.g trait_name, collection_type for
  \<= v3.0.2, basis of record for \>3.0.2

- ...:

  arguments passed to ggpointdensity::geom_pointdensity()

- basemap:

  `"australia"` (the default) draws the locations over a map of
  Australia, and leaves out any location beyond the map, with a message
  saying how many records that affects; if no location lies on the map,
  it stops with an error. `NULL` draws every location without a map, for
  databases from outside Australia.

## Value

ggplot of sites

## Author

Dony Indiarto - d.indiarto@student.unsw.edu.au

## Examples

``` r
if (FALSE) { # \dontrun{
#All traits from a given study
data <- austraits %>% extract_dataset(dataset_id = "Falster_2003") %>% join_location_coordinates()
data %>% plot_locations("trait_name")

#Single trait
data <- austraits %>% extract_trait(trait_names = c("plant_height")) %>% join_location_coordinates()
data$traits %>% plot_locations("trait_name")

#Every location, without the map of Australia
data$traits %>% plot_locations("trait_name", basemap = NULL)
} # }
```
