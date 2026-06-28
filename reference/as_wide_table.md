# Create a single wide table from a traits.build data object

Create a single wide table from a traits.build data object

## Usage

``` r
as_wide_table(database)
```

## Arguments

- database:

  traits.build database (list object)

## Value

A single wide table with collapsed contexts and locations text and with
some cols renamed for alignment with other resources

## Examples

``` r
if (FALSE) { # \dontrun{
austraits %>% as_wide_table()
} # }
```
