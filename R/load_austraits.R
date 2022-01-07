#' Load AusTraits
#' @description Downloads AusTraits version 3.0.2 and reads into R
#' @usage load_austraits(path, link, update)
#' @param path file path to where download data. By default set to "data/austraits"
#' @param link link to Zenodo record to download
#' @param update should files be downloaded anew?
#' @return AusTraits data object
#' @export
#' @examples
#' \dontrun{
#' austraits <- load_austraits()
#' }


load_austraits <- function(path = "data/austraits", link = "https://zenodo.org/api/records/5112001", update = FALSE) {
  
  if(!file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }
  
  file_json <- file.path(path, "austraits.json")
  
  if(!file.exists(file_json) | update ) {
    
    #Get url for latest version
    x <- httr::GET(link) %>% httr::content(as = "text")
    
    message("Downloading AusTraits to '", path,"'")
    
    # Save json
    writeLines(x, file_json)
    
    # Retrieve data
    x <- jsonlite::fromJSON(file_json)
    url <- x$files$links$download[1]
    file_path <- file.path(path, basename(x$files$filename[1]))
    
    download_austraits(url, file_path, path = path)
  }
  
  # Retrieve data
  x <- jsonlite::fromJSON(file_json)
  file_path <- file.path(path, basename(x$files$filename[1]))
  
  message("Loading data from '", file_path,"'")
  data <- readRDS(file_path) 
  #data$build_info <- NULL #Get rids of build info
  attr(data, "class") <- "austraits"
  
  data
  
  
}

print.austraits <- function(austraits){
  message(paste("AusTraits version", austraits$build_info$version))
  #"The austraits object has 11 dataframes arranged as a list, see here: https://traitecoevo.github.io/austraits/articles/structure.html" 
}


download_austraits <- function(url, filename, path) {
  #Download latest build
  fn <- paste(tempfile(), '.download', sep='') #Temporary folder
  res <- utils::download.file(url=url, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
  
  if (res == 0) { #Warning hygiene
    w <- getOption('warn') #save option
    on.exit(options('warn' = w)) #set it back
    options('warn'=-1)  #Deal with warnings
    
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
