#' Load AusTraits
#' @description Read in AusTraits .rds file into R, if filepath not specified, it reads in the lite version used for testing
#' @usage load_austraits(path = NA)
#' @param path file path to where downloaded AusTraits .rds file is found
#' @return AusTraits data
#' @export
#' @examples
#' \dontrun{
#' austraits <- load_austraits() #loads Lite version for testing
#' austraits <- load_austraits("data/austraits-2.1.0.rds")
#' }
#' 
load_austraits <- function(path = NA) {
  if(is.na(path)) {
    data <- austraits::austraits #Lite version
  } else
    data <- readRDS(path)

  data
}
