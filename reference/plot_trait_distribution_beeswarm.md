# Beeswarm Trait distribution

Plots distribution of trait values by a grouping variable using
ggbeeswarm package

## Usage

``` r
plot_trait_distribution_beeswarm(
  database,
  trait_name,
  y_axis_category,
  highlight = NA,
  hide_ids = FALSE
)
```

## Arguments

- database:

  traits.build database (list object)

- trait_name:

  Name of trait to plot

- y_axis_category:

  One of `dataset_id`, `family`

- highlight:

  Specify a group to highlight

- hide_ids:

  Logical for whether to add a label on y_axis?

## Author

Daniel Falster - daniel.falster@unsw.edu.au

## Examples

``` r
if (FALSE) { # \dontrun{
austraits %>% plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Westoby_2014")
} # }
```
