
load_austraits <- function(path=NA) {
  if(is.na(path)) {
    data <- austraits::austraits
  } else
    data <- readRDS(path)

  data
}