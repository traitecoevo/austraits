#' Convert a list of lists to dataframe
#'
#' Convert a list of lists to dataframe; requires that every list have same named elements.
#'
#' @param my_list A list of lists to dataframe
#' @param as_character A logical value, indicating whether the values are read as character
#' @param on_empty Value to return if my_list is NULL, NA or is length == 0, default = NA
#'
#' @export
#' @examples util_list_to_df2(util_df_to_list(dplyr::starwars))
util_list_to_df2 <- function(my_list, as_character = TRUE, on_empty = NA) {
  
  if (is.null(my_list) || any(is.na(my_list)) || length(my_list) == 0)
    return(on_empty)
  
  if (as_character)
    my_list <- lapply(my_list, lapply, as.character)
  
  dplyr::bind_rows(lapply(my_list, tibble::as_tibble))
}