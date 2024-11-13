#' Identify austraits.build or traits.build version
#' 
#' @description Determine whether database version was built by austraits.build (AusTraits pre 5.0.0) or traits.build (AusTraits post 5.0.0)
#'
#' @param database traits.build database (list object)
#' @return binary version for switch statements
#' @noRd
#' @keywords internal
#' 
what_version <- function(database){
  version <- database$build_info$version %>% as.character()

  if(package_version(version) <= '3.0.2'){
    ret_version <- "3-series-earlier"
  }

  if(package_version(version) > '3.0.2' & package_version(version) < '5.0.0'){
    ret_version <- "4-series"
  }

  if(package_version(version) >='5.0.0'){
    ret_version <- "5-series"
  }
  ret_version
}

#' Print version of AusTraits object
#'
#' @param database traits.build database (list object)
#' @return binary version for switch statements
#' @noRd
#' @keywords internal
#' 
print_version <- function(database){
  database$build_info$version %>% as.character()
}