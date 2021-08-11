#' Load AusTraits
#' @description Downloads AusTraits version 2.1.0 and reads into R
#' @usage load_austraits(path)
#' @param path file path to where downloaded AusTraits .rds file is found, if empty function will create an 'austraits' directory in your data folder
#' @return AusTraits data
#' @export
#' @examples
#' \dontrun{
#' austraits_lite <- load_austraits_lite() #loads Lite version for testing
#' austraits <- load_austraits("data/austraits/austraits-3.0.2.rds")
#' }


load_austraits <- function(path = "data/austraits/austraits-3.0.2.rds") { 
  if(!file.exists(path)) {
    
    #Get url for version 2.1.0 of austraits
    x <- jsonlite::fromJSON("https://zenodo.org/api/records/5112001")
    url <- x$files$links$download[1]
    filename <- basename(x$files$filename[1])
    
    download_austraits(url, filename)
  }
  data <- readRDS(path) #Load rds

  data
}

download_austraits <- function(url, filename) {
  #Download latest build
  fn <- paste(tempfile(), '.download', sep='') #Temporary folder
  res <- utils::download.file(url=url, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
  
  if (res == 0) { #Warning hygiene
    w <- getOption('warn') #save option
    on.exit(options('warn' = w)) #set it back
    options('warn'=-1)  #Deal with warnings
    
    path <- "data/austraits" #Path for file
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
    
    file.rename(fn, paste0(path,"/",filename)) #Copy tmp file to new folder
    
    if (! file.rename(fn, filename) ) { 
      # rename failed, perhaps because fn and filename refer to different devices
      file.copy(fn, path)
      file.remove(fn)
    }
  } else {
    stop('Could not download the file' )
  }
}


#' Load small subset of austraits 
#' @description Load lite version of austraits for demo and testing purposes
#' @return
#' @rdname load_austraits
#' @export
load_austraits_lite <- function(){
  data <- austraits::austraits #lite version
}
