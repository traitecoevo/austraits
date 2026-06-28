# Produce location maps of trait values

**\[deprecated\]**

Plot location where trait data was collected from

## Usage

``` r
plot_site_locations(trait_data, feature = "trait_name", ...)
```

## Arguments

- trait_data:

  traits table in a traits.build database with site details appended.
  See join_location_coordinates and examples

- feature:

  grouping/classification categories e.g trait_name, collection_type for
  \<= v3.0.2

- ...:

  arguments passed to ggplot()

## Value

ggplot of sites

## Author

Dony Indiarto - d.indiarto@student.unsw.edu.au
