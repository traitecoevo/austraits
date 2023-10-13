#' Determine whether version was pre or post 3.0.2
#'
#' @param austraits austraits list object
#' @return binary version for switch statements
#' @noRd
#' @keywords internal
#' 
what_version <- function(austraits){
  version <- austraits$build_info$version %>% as.character()

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