# Print out AusTraits versions

Print out AusTraits versions

## Usage

``` r
get_versions(path = "data/austraits", update = TRUE)
```

## Arguments

- path:

  A file path where AusTraits was previously downloaded

- update:

  Would you like the versions json be updated in case of new releases?

## Value

A tibble containing version numbers and doi which can be used in
load_austraits()

## Examples

``` r
if (FALSE) { # \dontrun{
austraits <- load_austraits(version = "3.0.2", path = "data/austraits")
} # }
```
