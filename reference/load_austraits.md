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
  update = TRUE,
  means = FALSE
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

  if `TRUE`, the list of AusTraits releases (see
  [`get_versions()`](https://traitecoevo.github.io/austraits/reference/get_versions.md))
  and the release's metadata are downloaded afresh; if that fails (e.g.
  offline), the copies saved in `path` are used. With `FALSE`, the saved
  copies are used without contacting Zenodo, so a release loaded before
  loads offline.

- means:

  if `TRUE`, load the release's observed means rather than the database;
  [`traits.use::trait_means_dictionary()`](https://traitecoevo.github.io/traits.use/reference/trait_means_dictionary.html)
  defines their columns. A release from before the means were published
  gives an error saying so.

## Value

a large list containing AusTraits data tables, of class `traits.build`.
Its `sources` have the class `traits_build_sources` (see
[`?traits.use::traits_build_sources`](https://traitecoevo.github.io/traits.use/reference/traits_build_sources.html)),
also for releases built before that class existed, so a reference can be
looked up by key with `$` as well as `[[` and `[`, e.g.
`austraits$sources$Falster_2005`. With `means = TRUE`, a tibble of the
observed means.

## Details

With `means = TRUE` it loads the release's observed means instead: one
row per taxon and trait, computed from the release's records with
[`traits.use::summarise_trait_means()`](https://traitecoevo.github.io/traits.use/reference/summarise_trait_means.html)
and published with it as `austraits-<version>-observed-means.rds` and
`.csv.gz` (releases after 7.0.0; the `.rds`, exact, is read when
present). They are a smaller download than the database. To have both,
load each.

## See also

[`get_versions()`](https://traitecoevo.github.io/austraits/reference/get_versions.md),
[`get_version_latest()`](https://traitecoevo.github.io/austraits/reference/get_version_latest.md),
[`traits.use::load_database()`](https://traitecoevo.github.io/traits.use/reference/load_database.html),
which loads any traits.build database from its doi

## Examples

``` r
if (FALSE) { # \dontrun{
austraits <- load_austraits(version = "7.0.0", path = "data/austraits")

# The observed means of each taxon and trait, published with releases after 7.0.0
means <- load_austraits(version = "8.0.0", path = "data/austraits", means = TRUE)
} # }
```
