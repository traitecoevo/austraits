# Create combined traits.build table

Create a single database output that merges together the information in
all relational tables within a traits.build database. Trait measurements
are still output in long format (1 row per trait value), but all
measurement-related metadata (methods, location properties, context
properties, contributors) are now included as additional columns in a
single table.

## Usage

``` r
flatten_database(database, format, vars, include_description)
```

## Arguments

- database:

  traits.build database (list object)

- format:

  A parameter for the locations, contexts and data contributors tables
  specifying how data are packed. All three can be formatted as a single
  compacted column(s) will have a human readable column
  ("single_column_pretty") or using json ("single_column_json") syntax.
  For location properties or context properties there is also the option
  to add each `location_property` or `context_property` to the traits
  table as its own column ("many_columns"); the contributors column
  defaults to "single_column_pretty" when this option is selected.

- vars:

  List specifying which columns or properties to include from each
  table. The detail is for all columns/properties to be included.

- include_description:

  A logical indicating whether to include (TRUE) or omit (FALSE) the
  context_property descriptions; defaults to TRUE.

## Value

A table combining information in 8 traits.build relational tables:
traits, locations, contexts, methods, taxa, taxonomic_updates,
contributors, and identifiers
