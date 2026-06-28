# Convert list of lists to dataframe

Convert a list of lists to a dataframe, useful when converting a
multi-level yaml into a dataframe. Function required that every list
have same named elements.

## Usage

``` r
convert_list_to_df2(my_list, as_character = TRUE, on_empty = NA)
```

## Arguments

- my_list:

  A list of lists to dataframe

- as_character:

  A logical value, indicating whether the values are read as character

- on_empty:

  Value to return if my_list is NULL, NA or is length == 0, default = NA

## Value

tibble

## Examples

``` r
demo_list1 <- list(word1 = "this", word2 = "is", word3 = "an", word4 = "example", word5 = "list")
demo_list2 <- list(word1 = "and", word2 = "a", word3 = "second", word4 = "list", word5 = "also")
combined_list <- list(demo_list1, demo_list2)
convert_list_to_df2(combined_list)
#> # A tibble: 2 × 5
#>   word1 word2 word3  word4   word5
#>   <chr> <chr> <chr>  <chr>   <chr>
#> 1 this  is    an     example list 
#> 2 and   a     second list    also 
```
