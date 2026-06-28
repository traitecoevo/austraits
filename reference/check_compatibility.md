# Check compatibility of traits.build object

Function to check whether the data object has been compiled by the
traits.build workflow and therefore has a data structure that is
appropriate for use with austraits functions.

## Usage

``` r
check_compatibility(database, single_table_allowed = FALSE)
```

## Arguments

- database:

  traits.build database (list object)

- single_table_allowed:

  logical for when the input might be a single table instead of a
  complete database; defaults to FALSE

## Value

logical (T/F) output and messaging for uncompatible versions

## Author

Elizabeth Wenk - e.wenk@unsw.edu.au

## Examples

``` r
if (FALSE) { # \dontrun{
check_compatibility(database)
} # }
```
