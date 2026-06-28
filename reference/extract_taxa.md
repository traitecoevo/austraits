# Extract all data for specific taxa

Function to subset of all data associated with a particular taxon from a
traits.build relational database.

## Usage

``` r
extract_taxa(
  database,
  family = NULL,
  genus = NULL,
  taxon_name = NULL,
  partial_matches_allowed = TRUE
)
```

## Arguments

- database:

  traits.build database (list object)

- family:

  character string of family or families

- genus:

  character string of genus or genera

- taxon_name:

  character string of taxon name(s)

## Value

List of tibbles containing all traits.build data and metadata for the
specified taxa.

## Details

`extract_taxa` has been developed to extract data for specific taxa from
databases built using the traits.build workflow. Learn more at:
[https://github.com/traitecoevo/traits.build](https://github.com/traitecoevo/traits.build-book)
& <https://github.com/traitecoevo/traits.build-book>

Note to AusTraits users:

- This function works with AusTraits version \>= 5.0.0 (from Nov 2023
  release)

- For AusTraits versions \<= 4.2.0 (up to Sept 2023 release) see
  <https://github.com/traitecoevo/database> for how to install old
  versions of the package or download a newer version of the database.

## Author

Fonti Kar - f.kar@unsw.edu.au

## Examples

``` r
if (FALSE) { # \dontrun{
extract_taxa(database = austraits, family = "Proteaceae")
extract_taxa(database = austraits, genus = "Acacia")
} # }
```
