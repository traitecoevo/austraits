# Recommendation by jennybc https://github.com/STAT545-UBC/Discussion/issues/451
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "doi", "publication_date", "version"))

.onAttach <- function(libname, pkgname) {
    cli::cli_inform("Thanks for showing interest in `austraits`! Please consider citing this package - citation('austraits')", class = "packageStartupMessage")
}

# RefManageR stays in Depends, so it is attached as with earlier versions
#' @importFrom RefManageR BibEntry
#' @keywords internal
"_PACKAGE"
