# Importing the package that austraits depends on
#' @import RefManageR

# Recommendation by jennybc https://github.com/STAT545-UBC/Discussion/issues/451
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "dplyr::n()"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thanks for showing interest in `austraits`! Please consider citing this package - citation('austraits')")
}