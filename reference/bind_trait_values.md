# Bind trait values

This function condenses data for studies that have multiple observations
for a given trait into a single row. This function concatenates multiple
values into a single cell

## Usage

``` r
bind_trait_values(trait_data)
```

## Arguments

- trait_data:

  the traits table in a traits.build database – see example

## Value

tibble that is condensed down where multiple observations in value,
value_type and replicates are collapsed down and separated by '–'

## Author

Daniel Falster - daniel.falster@unsw.edu.au

## Examples

``` r
if (FALSE) { # \dontrun{
traits <- austraits$traits %>% 
dplyr::filter(dataset_id == "ABRS_1981")
traits
traits_bind <- bind_trait_values(traits)
} # }
```
