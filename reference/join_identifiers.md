# Joining identifiers to traits table

Function to merge metadata from the identifiers table of a traits.build
database into the core traits table.

## Usage

``` r
join_identifiers(database, format = "single_column_pretty", vars = "all")
```

## Arguments

- database:

  traits.build database (list object)

- format:

  Specifies whether metadata from the identifiers is output in a human
  readable format ("single_column_pretty"; default), with each location
  property added as a separate column ("many_columns") or using json
  syntax ("single_column_json").

- vars:

  Identifier types for which data is to be appended to the traits table,
  defaulting to all identifier types (vars = "all").

## Value

traits.build list object, but with identifiers from the identifiers
table appended to the traits table.

## Details

the `join_` functions have been developed to join relational tables for
databases built using the traits.build workflow. Learn more at:
<https://github.com/traitecoevo/traits.build> &
<https://github.com/traitecoevo/traits.build-book>

Note to AusTraits users:

- This function works with AusTraits version \>=7.0.0 (2025 releases)

- For AusTraits versions \<= 4.2.0 (up to Sept 2023 release) see
  <https://github.com/traitecoevo/austraits> for how to install old
  versions of the package or download a newer version of the database.

## Examples

``` r
if (FALSE) { # \dontrun{
(database %>% join_identifiers(format = "single_column_pretty", vars = "all"))$traits
} # }
```
