# Joining data contributor metadata to traits table

Function to merge metadata from the data contributors table of a
traits.build database into the core traits table.

## Usage

``` r
join_contributors(database, format = "single_column_pretty", vars = "all")
```

## Arguments

- database:

  traits.build database (list object)

- format:

  Specifies whether metadata from the contributors table is output in a
  human readable format ("single_column_pretty"; default) or using json
  syntax ("single_column_json").

- vars:

  Columns from the taxa table to be joined to the traits table,
  defaulting to all columns (vars = "all").

## Value

traits.build list object, but with additional fields (columns) for the
specified variables from the data contributors table appended to the
traits table.

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
(database %>% join_contributors(format = "single_column_pretty", 
vars = c("last_name", "first_name", "ORCID")))$traits
} # }
```
