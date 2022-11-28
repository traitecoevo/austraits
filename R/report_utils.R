#' html styling for kable tables
#' @param ... arguments passed on to kableExtra::kable, kableExtra::kable_styling
#' @keywords internal
#' @export

my_kable_styling_html <- function(...) {
    kableExtra::kable(...) %>%
    kableExtra::kable_styling(..., 
                  bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = FALSE, 
                  position = "left"
                  ) %>%
    # hack to add margin to plot
    gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', .) #this dot is trigger NOTE in RMD Check
}

#' pdf styling for kable tables
#' @param ...arguments passed on to kableExtra::kable
#' @keywords internal
#' @export
my_kable_styling_pdf <- function(...) {
    kableExtra::kable(...)
}

#' Format a table with kable and default styling
#' @param ... arguments passed on to kableExtra::kable
#' @keywords internal
#' @export
my_kable_styling_markdown <- function(...) {
  kableExtra::kable(...)
}

#' Convert a list with single entries to dataframe
#' @param my_list a list with single entries
#' @return a tibble with two columns
#' @examples list1_to_df(as.list(iris)[2])
#' @keywords internal
#' @export
list1_to_df <- function(my_list) {
  
  for(f in names(my_list)) {
    if(is.null(my_list[[f]])) 
      my_list[[f]] <- NA
  }
  
  tidyr::tibble(key = names(my_list), value = unlist(my_list))
}


