# Joining context properties to traits table

Function to merge metadata from the contexts table of a traits.build
database into the core traits table.

## Usage

``` r
join_context_properties(
  database,
  format = "single_column_pretty",
  vars = "all",
  include_description = TRUE
)
```

## Arguments

- database:

  traits.build database (list object)

- format:

  Specifies whether metadata from the contexts is output in a human
  readable format ("single_column_pretty"; default), with each context
  property added as a separate column ("many_columns") or using json
  syntax ("single_column_json").

- vars:

  Location properties for which data is to be appended to the traits
  table, defaulting to all context properties (vars = "all").

- include_description:

  A logical indicating whether to include (TRUE) or omit (FALSE) the
  context_property descriptions.

## Value

traits.build list object, but context properties from the contexts table
appended to the traits table.

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
(database %>% join_context_properties(
format = "many_columns", vars = "all", include_description = TRUE))$traits
} # }
```
