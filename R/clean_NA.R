#' @title NA hygiene
#'
#' @description Helper function to convert character strings of NA into true NA
#' @usage clean_NA(x)
#' @param data The trait data frame generated from austraits - see example
#' @param definitions The austraits definitions data frame
#' @return vector where strings of NA are treated as true NA
#' @examples 
#' \dontrun{
#' clean_NA(c("NA", 1, 2, 3))) %>% is.na()
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @importFrom rlang .data
#' @NoRd

clean_NA <- function(x) {
  ifelse(x == "NA", NA_character_, x)
}
