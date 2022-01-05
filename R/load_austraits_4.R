#' Load AusTraits database into R console
#'
#' @param version character string - version number of database
#' @param path file path to where AusTraits will be downloaded
#' @param update if TRUE, AusTraits version will be redownloaded
#'
#' @return a large list containing AusTraits data tables
#' @export
#'
#' @examples
#' \dontrun{
#' austraits <- load_austraits_4()
#' }


load_austraits_4 <- function(version = "v3.0.2", path = "ignore/data/austraits", update = FALSE){
  # Does the path exist? 
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }
  
  file_json <- file.path(path, "austraits.json")
  
  # Does the .json exist?
  if(! file.exists(file_json) | update == TRUE){
    # Retrieve the .json
    res <- jsonlite::read_json("https://zenodo.org/api/records/?q=conceptrecid:3568417&all_versions=true",
                               simplifyVector = T)
    # Save it
    jsonlite::write_json(res, file_json)
  }

  #Retrieve and load the version
  data <- switch(version,
         v3.0.2 = dload_rds(version, file_json, path),
         v3.0.1 = dload_rds(version, file_json, path),
         v3.0.0 = dload_rds(version, file_json, path),
         v2.1.0 = dload_rds(version, file_json, path),
         v2.0.0 = dload_rds(version, file_json, path),
         v0.9.1 = dload_rds(version, file_json, path))
  

  # Assign class
  attr(data, "class") <- "austraits"
  
  data
}

#' Function for loading .rds AusTraits files
#'
#' @param version character string - version number of database
#' @param json_path file path to austraits.json
#' @param path file path to where AusTraits will be downloaded

dload_rds <- function(version, json_path, path){
  # Load the json
  res <- jsonlite::fromJSON(json_path) 
  
  # Name the files list
  names(res$hits$hits$files) <- res$hits$hits$metadata$version
  
  # Getting specific version
  target <- res$hits$hits$files[[version]]
  
  # Setting up the pars
  url <- target$links$self[1]
  file_path <- file.path(path, target$key[1])
  
  # Downloading 
  download_austraits(url, file_path, path = path)
  
  if(length(list.files(path = path, pattern = "\\.rds$")) > 0){
  # Loading the .rds
  message("Loading data from '", file_path,"'")
  data <- readRDS(file_path) 
  }
}


dload_zip <- function(){
  
}

#' Function for loading .rds AusTraits files
#'
#' @param url url of download via Zenodo API
#' @param filename Name of file that will be downloaded e.g. austraits-3.0.2.rds
#' @param path file path to where AusTraits will be downloaded

download_austraits <- function(url, filename, path) {
  #Download latest build
  fn <- paste(tempfile(), '.download', sep='') #Temporary folder
  
  message("Downloading AusTraits to '", path,"'")
  
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
    stop('Could not download AusTraits!' )
  }
}


print_versions <- function(){
  message("Retrieving all versions of AusTraits...")
  
  res <- jsonlite::read_json("https://zenodo.org/api/records/?q=conceptrecid:3568417&all_versions=true",
                             simplifyVector = T)
  # Create a table
  ret <- dplyr::tibble(date = res$hits$hits$metadata$publication_date,
                       version = stringr::str_extract(res$hits$hits$metadata$version, "[0-9]+\\.[0-9]+\\.[0-9]"),
                       doi = res$hits$hits$metadata$doi)
  
  # Order by numeric version
  ret <- ret[order(dplyr::desc(numeric_version(ret$version))),]
  
  # Add in the v again
  ret %>% dplyr::mutate(version = paste0("v", version))
}

