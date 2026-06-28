# Load AusTraits database into R console

Load AusTraits database into R console

## Usage

``` r
load_austraits(
  doi = NULL,
  version = NULL,
  path = "data/austraits",
  update = FALSE
)
```

## Arguments

- doi:

  character string - doi of particular version

- version:

  character string - version number of database

- path:

  file path to where AusTraits will be downloaded

- update:

  if TRUE, AusTraits versions .json will be re-downloaded

## Value

a large list containing AusTraits data tables

## See also

get_versions get_version_latest

## Examples

``` r
if (FALSE) { # \dontrun{
austraits <- load_austraits(version = "3.0.2", path = "data/austraits")
} # }
```
