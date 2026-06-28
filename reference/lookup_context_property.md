# Look up context properties

Look up context properties that contain a specific search term.

## Usage

``` r
lookup_context_property(database, term)
```

## Arguments

- database:

  traits.build database (list object)

- term:

  character string for context property search term

## Value

vector containing context properties that contains search term

## Examples

``` r
if (FALSE) { # \dontrun{
austraits %>% lookup_context_property("temperature")
} # }
```
