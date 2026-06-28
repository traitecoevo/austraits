# Look up a particular trait term

Look up a particular trait term

## Usage

``` r
lookup_trait(database, term)
```

## Arguments

- database:

  traits.build database (list object)

- term:

  character string for trait search term

## Value

vector containing traits that contains search term

## Examples

``` r
if (FALSE) { # \dontrun{
austraits %>% lookup_trait("leaf") %>% extract_trait(database = austraits, .)
} # }
```
