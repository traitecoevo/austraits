# Separate bounded trait values

This function reverts the action of bind_trait_values. This function
separates values that were concatenated so that studies that have
multiple observations for a given trait will have separate row for each
observation.

## Usage

``` r
separate_trait_values(trait_data, definitions)
```

## Arguments

- trait_data:

  The traits table in a traits.build database - see example

- definitions:

  The austraits definitions data frame

## Value

trait tibble

## Author

Daniel Falster - daniel.falster@unsw.edu.au

## Examples

``` r
if (FALSE) { # \dontrun{
trait_data <- austraits$traits %>% 
dplyr::filter(dataset_id == "Falster_2005_1")
trait_data
traits_bind <- bind_trait_values(trait_data)
separate_trait_values(traits_bind)
} # }
```
