# Print out AusTraits versions

Lists the releases of AusTraits with
[`traits.use::list_versions()`](https://traitecoevo.github.io/traits.use/reference/list_versions.html),
which lists the releases of any traits.build database from its doi, and
saves the list in `path` so it can be used offline.

## Usage

``` r
get_versions(path = "data/austraits", update = TRUE)
```

## Arguments

- path:

  A file path where AusTraits was previously downloaded

- update:

  if `TRUE`, the list is fetched from Zenodo; if that fails (e.g.
  offline), the copy saved in `path` is used. With `FALSE`, the saved
  copy is used, if there is one.

## Value

A tibble of the releases, newest first: `publication_date`, `doi`,
`version` (e.g. "7.0.0") and `id` (the Zenodo record id). Pass a version
to
[`load_austraits()`](https://traitecoevo.github.io/austraits/reference/load_austraits.md).

## See also

[`traits.use::list_versions()`](https://traitecoevo.github.io/traits.use/reference/list_versions.html)

## Examples

``` r
if (FALSE) { # \dontrun{
get_versions(path = "data/austraits")
} # }
```
