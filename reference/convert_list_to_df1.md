# Convert list with single entries to dataframe

Convert a list with a single level of entries to a dataframe, useful
when converting a yaml into a dataframe.

## Usage

``` r
convert_list_to_df1(my_list)
```

## Arguments

- my_list:

  A list with single entries

## Value

A tibble with two columns

## Examples

``` r
if (FALSE) { # \dontrun{
convert_list_to_df1(as.list(dplyr::starwars)[2])
} # }
```
