# Summarise counts for a particular variable of interest

Summarise counts for a particular variable of interest

## Usage

``` r
summarise_database(database, var)
```

## Arguments

- database:

  traits.build database (list object)

- var:

  variable you use wish to see summary of (trait_name, genus, family)

## Value

dataframe of unique levels of variable with counts and percentage

## Examples

``` r
if (FALSE) { # \dontrun{
summarise_database(database = austraits, "trait_name")
summarise_database(database = austraits, "family")
} # }
```
