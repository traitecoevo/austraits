#' Load AusTraits database into R console
#'
#' @param version character string - version number of database
#' @param doi character string - doi of particular version 
#' @param path file path to where AusTraits will be downloaded
#' @param update if TRUE, AusTraits versions json will be redownloaded
#'
#' @return a large list containing AusTraits data tables
#' @export
#'
#' @examples
#' \dontrun{
#' austraits <- load_austraits_4()
#' }


load_austraits_4 <- function(version = NULL, doi = NULL , path = "ignore/data/austraits", update = FALSE){
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

  # Load the json
  res <- jsonlite::fromJSON(file_json) 
  
  # Name the files list
  names(res$hits$hits$files) <- res$hits$hits$metadata$version
  
  # Version table
  ret <- dplyr::tibble(date = res$hits$hits$metadata$publication_date,
                       version = stringr::str_extract(res$hits$hits$metadata$version, "[0-9]+\\.[0-9]+\\.[0-9]"),
                       doi = res$hits$hits$metadata$doi) %>% 
    dplyr::filter(! version < 1) # Exclude any versions prior to 1.0.0
  
  # Order by numeric version
  ret <- ret[order(dplyr::desc(numeric_version(ret$version))),]
  
  # If only doi is provided, match it with its version number
  if(missing(version) & ! missing(doi)){
    version <- ret[which(ret$doi == doi),"version"] %>% as.character()
  }
  
  # If only version is provided, match it with its doi (so it doesn't throw errors below)
  if(! missing(version) & missing(doi)){
    doi <- ret[which(ret$version == version),"doi"] %>% as.character()
  }
                        
  # Check if version/doi is available
  if(! version %in% ret$version | ! doi %in% ret$doi){
    rlang::abort("Requested version/doi is incorrect! Try print_versions()")
  }
  
  # Add in prefix of v
  version_name <- paste0("v", version)
  
  #Check if version/doi is download, if not download
  if(! file.exists(paste0(path,"austraits-",version,".rds")) ){
    # Getting specific version
    target <- res$hits$hits$files[[version_name]]
    
    # Setting up the pars
    url <- target$links$self[1]
    file_path <- file.path(path, target$key[1])
    
    # Downloading file
    download_austraits(url, file_path, path = path)
  }
  
  # Loading the .rds
  message("Loading data from '", file_path,"'")
  data <- readRDS(file_path) 
  
  # Assign class
  attr(data, "class") <- "austraits"
  
  data
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

#' Print out AusTraits versions
#'
#' @param path A file path, if AusTraits was previously downloaded direct to that folder
#' @param update Would you like the versions json be updated in case of new releases?
#'
#' @return A tibble containing version numbers and doi which can be used in load_austraits()
#' @export

print_versions <- function(path, update = TRUE){
  
  file_json <- file.path(path, "austraits.json")
  
  # Does the .json exist in specificied path?
  if(! file.exists(file_json) | update == TRUE){
    # Retrieve the .json
    res <- jsonlite::read_json("https://zenodo.org/api/records/?q=conceptrecid:3568417&all_versions=true",
                               simplifyVector = T)
    
    message("Retrieving all versions of AusTraits...")
    
    # Save it
    jsonlite::write_json(res, file_json)
  }
  
  # Load the json
  res <- jsonlite::fromJSON(file_json) 
  
  # Create a table
  ret <- dplyr::tibble(date = res$hits$hits$metadata$publication_date,
                       version = stringr::str_extract(res$hits$hits$metadata$version, "[0-9]+\\.[0-9]+\\.[0-9]"),
                       doi = res$hits$hits$metadata$doi) %>% 
    dplyr::filter(! version < 1)
  
  # Order by numeric version
  ret <- ret[order(dplyr::desc(numeric_version(ret$version))),]
  
  ret

}

