# Retrieve the latest version of AusTraits

Retrieve the latest version of AusTraits

## Usage

``` r
get_version_latest(path = "data/austraits", update = TRUE)
```

## Arguments

- path:

  A file path where AusTraits was previously downloaded

- update:

  if `TRUE`, the list is fetched from Zenodo; if that fails (e.g.
  offline), the copy saved in `path` is used. With `FALSE`, the saved
  copy is used, if there is one.

## Value

character string of latest version
