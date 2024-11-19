# Importing the package that austraits depends on
#' @import RefManageR

# Recommendation by jennybc https://github.com/STAT545-UBC/Discussion/issues/451
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "dplyr::n()"))

.onAttach <- function(libname, pkgname) {
    cli::cli_inform("Thanks for showing interest in `austraits`! Please consider citing this package - citation('austraits')", class = "packageStartupMessage")
}

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

utils::globalVariables(c(
  "aus_traits",
  "..density..",
  "description",
  ".data",
  "doi",
  "given_name",
  "genus",
  "Group",
  "abort",
  "australia",
  "basis_of_value",
  "category",
  "colour",
  "context", 
  "context_name",
  "context_property",
  "contributor",
  "data",
  "database",
  "dataset_id",
  "data_contributors",
  "identifier",
  "last_name",
  "latitude (deg)", 
  "link_id", 
  "link_vals",
  "location_id",
  "location_name", 
  "location_property",
  "location_properties",
  "longitude (deg)",
  "method_context_id",
  "method_id", 
  "n", 
  "name",
  "n_vals", 
  "n_value_type", 
  "observation_id", 
  "original_name", 
  "percent",
  "percent_total",
  "publication_date",
  "repeat_measurements_id",
  "resource_type",
  "relation_type",
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
  "y"
)
)
