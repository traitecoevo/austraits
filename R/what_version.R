#' Determine whether version was pre or post 3.0.2
#'
#' @param austraits austraits list object
#' @return binary version for switch statements
#' @noRd
#' @keywords internal
what_version <- function(austraits){
  version <- austraits$build_info$version %>% as.character()
  
  if(package_version(version) <= '3.0.2'){
    ret_version <- "old"
  } 
  
  if(package_version(version) > '3.0.2'){
    ret_version <- "new"
  } 
  ret_version
}