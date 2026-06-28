# Produce location maps of trait values

Plot location where trait data was collected from

## Usage

``` r
plot_locations(database, feature = "trait_name", ...)
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

  arguments passed to ggplot()

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
} # }
```
