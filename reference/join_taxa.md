# Joining taxonomy to traits table

Function to merge metadata from the taxa table of a traits.build
database into the core traits table.

## Usage

``` r
join_taxa(
  database,
  vars = c("family", "genus", "taxon_rank", "establishment_means")
)
```

## Arguments

- database:

  traits.build database (list object)

- vars:

  Columns from the taxa table to be joined to the traits table,
  defaulting to c("family", "genus", "taxon_rank",
  "establishment_means").

## Value

traits.build list object, but with additional fields (columns) for the
specified variables from the taxa table appended to the traits table.

## Details

the `join_` functions have been developed to join relational tables for
databases built using the traits.build workflow. Learn more at:
<https://github.com/traitecoevo/traits.build> &
<https://github.com/traitecoevo/traits.build-book>

Note to AusTraits users:

- This function works with AusTraits version \>= 5.0.0 (from Nov 2023
  release)

- For AusTraits versions \<= 4.2.0 (up to Sept 2023 release) see
  <https://github.com/traitecoevo/austraits> for how to install old
  versions of the package or download a newer version of the database.

## Examples

``` r
if (FALSE) { # \dontrun{
#Append taxonomic details
(database %>% join_taxa)$traits
} # }
```
