# Recommendation by jennybc https://github.com/STAT545-UBC/Discussion/issues/451
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "doi", "publication_date", "version"))

# RefManageR stays in Depends, so it is attached as with earlier versions
#' @importFrom RefManageR BibEntry
#' @importFrom rlang .data
#' @keywords internal
"_PACKAGE"
