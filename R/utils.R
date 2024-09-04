#' Convert a list of lists to dataframe
#'
#' Convert a list of lists to dataframe; requires that every list have same named elements.
#'
#' @param my_list A list of lists to dataframe
#' @param as_character A logical value, indicating whether the values are read as character
#' @param on_empty Value to return if my_list is NULL, NA or is length == 0, default = NA
#'
#' @keywords internal
#' @noRd
#' @examples demo_list1 <- list(word1 = "this", word2 = "is", word3 = "an", word4 = "example", word5 = "list")
#' demo_list2 <- list(word1 = "and", word2 = "a", word3 = "second", word4 = "list", word5 = "also")
#' combined_list <- list(demo_list1, demo_list2)
#' util_list_to_df2(combined_list)

util_list_to_df2 <- function(my_list, as_character = TRUE, on_empty = NA) {
  
  if (is.null(my_list) || any(is.na(my_list)) || length(my_list) == 0)
    return(on_empty)
  
  if (as_character)
    my_list <- lapply(my_list, lapply, as.character)
  
  dplyr::bind_rows(lapply(my_list, tibble::as_tibble))
}