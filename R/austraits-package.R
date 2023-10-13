# Importing the package that austraits depends on
#' @import RefManageR

# Recommendation by jennybc https://github.com/STAT545-UBC/Discussion/issues/451
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "dplyr::n()"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thanks for showing interest in `austraits`! Please consider citing this package - citation('austraits')")
}

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

utils::globalVariables(c("..density..",
                       ".data",
                       "Group",
                       "abort",
                       "australia",
                       "colour",
                       "context", 
                       "context_name",
                       "context_property",
                       "dataset_id",
                       "latitude (deg)", 
                       "link_id", 
                       "link_vals",
                       "location_name", 
                       "location_property",
                       "longitude (deg)",
                       "method_context_id",
                       "method_id", 
                       "n", 
                       "n_vals", 
                       "n_value_type", 
                       "observation_id", 
                       "original_name", 
                       "percent",
                       "percent_total",
                       "repeat_measurements_id",
                       "replicates",
                       "shapes",
                       "site_name",
                       "site_property",
                       "source_id",
                       "taxon_name", 
                       "text", 
                       "trait_name",
                       "value",
                       "value_type",
                       "x",
                       "y")
)
