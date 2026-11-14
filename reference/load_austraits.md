# Load AusTraits database into R console

Finds the requested release of AusTraits on Zenodo and loads it with
[`traits.use::load_database()`](https://traitecoevo.github.io/traits.use/reference/load_database.html),
which downloads the relational database `.rds` to `path` (if it is not
already there, or if the copy there does not match the release's md5
checksum) and loads it.

## Usage

``` r
load_austraits(
  doi = NULL,
  version = NULL,
  path = "data/austraits",
  update = TRUE
)
```

## Arguments

- doi:

  character string - doi of particular version

- version:

  character string - version number of database, e.g. "7.0.0"

- path:

  file path to where AusTraits will be downloaded

- update:

  if TRUE, the list of AusTraits versions (`austraits.json`) and the
  release's metadata are downloaded afresh

## Value

a large list containing AusTraits data tables, of class `traits.build`.
Its `sources` have the class `traits_build_sources` (see
[`?traits.use::traits_build_sources`](https://traitecoevo.github.io/traits.use/reference/traits_build_sources.html)),
also for releases built before that class existed, so a reference can be
looked up by key with `$` as well as `[[` and `[`, e.g.
`austraits$sources$Falster_2005`.

## See also

[`get_versions()`](https://traitecoevo.github.io/austraits/reference/get_versions.md),
[`get_version_latest()`](https://traitecoevo.github.io/austraits/reference/get_version_latest.md),
[`traits.use::load_database()`](https://traitecoevo.github.io/traits.use/reference/load_database.html),
which loads any traits.build database from its doi

## Examples

``` r
if (FALSE) { # \dontrun{
austraits <- load_austraits(version = "7.0.0", path = "data/austraits")
} # }
```
