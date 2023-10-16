#' Load AusTraits database into R console
#'
#' @param version character string - version number of database
#' @param doi character string - doi of particular version 
#' @param path file path to where AusTraits will be downloaded
#' @param update if TRUE, AusTraits versions .json will be re-downloaded
#'
#' @return a large list containing AusTraits data tables
#' @export
#' @seealso get_versions get_version_latest
#'
#' @examples
#' \dontrun{
#' austraits <- load_austraits(version = "3.0.2", path = "data/austraits")
#' }

load_austraits <- function(doi = NULL, version = NULL, path = "data/austraits", update = FALSE){
  # Is either doi or version supplied? 
  if(is.null(doi) & is.null(version)){
    stop("Please supply a doi or version! Don't know which one you are after? Try get_versions()!")
  }
  
  # Is path supplied?
  if(rlang::is_missing(path)){
    stop("File path must be supplied!")
  }

  #remove v from version
  if (!is.null(version)) {
    version <- stringr::str_remove_all(version, "v")
  }

  # Does the path exist? 
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }
  
  # Load the json
  res <- load_json(path = path, update = update) 
  
  # Name the response list
  names(res$files) <- res$metadata$version
  
  # Create metadata table
  ret <- create_metadata(res)
  
  # If only doi is provided, match it with its version number  
  if(! is.null(doi)){
    # Check doi is in list of doi 
    if(! doi %in% ret$doi){
      rlang::abort("Requested version/doi is incorrect!")
    }
    version <- ret[which(ret$doi == doi),"version"] %>% as.character()
  }

  # Check if version/doi is available
  if(! version %in% ret$version){
    rlang::abort("Requested version/doi is incorrect!")
  }
  
  # Add in prefix of v
  version_name <- paste0("v", version)
  
  # Getting specific file
  id <- ret[which(ret$version == version), "id"] |> as.character()
  
  target <- res$files[[version_name]]$filename
  target <- target[grep(".rds", target, fixed = TRUE)]
  
  url <- sprintf("https://zenodo.org/records/%s/files/%s", id, target)
  
  file_path <- file.path(path, target)

  #Check if version/doi is download, if not download
  if(! file.exists(file_path)){
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


#' Load the austraits.json
#'
#' @inheritParams load_austraits

load_json <- function(path, update){
  # Set the directory path to json
  file_json <- file.path(path, "austraits.json")
  
  # Does the .json exist?
  if(! file.exists(file_json) | update == TRUE){
    # Retrieve the .json
    res <- jsonlite::read_json("https://zenodo.org/api/records/3568418/versions",
                               simplifyVector = T)
    # Save it
    jsonlite::write_json(res, file_json)
  }
  
  # Load the json
  jsonlite::fromJSON(file_json) 
}

#' Helper function to create nice metadata table
#'
#' @param res output of austraits.json
#' @return dataframe of metadata (date of release, doi and version)

create_metadata <- function(res){
  # Version table
  ret <- 
    dplyr::tibble(
      date = res$metadata$publication_date,
      version = stringr::str_extract(res$metadata$version, "[0-9]+\\.[0-9]+\\.[0-9]"),
      doi = res$metadata$doi,
      id = stringr::str_remove_all(res$metadata$doi, stringr::fixed("10.5281/zenodo.")
      )) %>%  
    dplyr::filter(! version < 1) %>% # Exclude any versions prior to 1.0.0
  dplyr::as_tibble()
  
  # Order by numeric version
  ret[order(dplyr::desc(numeric_version(ret$version))),]
}

#' Function for loading .rds AusTraits files
#'
#' @param url url of download via Zenodo API
#' @param filename Name of file that will be downloaded e.g. austraits-3.0.2.rds
#' @param path file path to where AusTraits will be downloaded

download_austraits <- function(url, filename, path) {
  # Get user timeout option
  o <- getOption('timeout')
  
  # Set max timeout
  options(timeout = max(300, getOption("timeout"))) 
  on.exit(options(timeout = o)) #Set options back to original
  
  #Download latest build
  fn <- paste(tempfile(), '.download', sep='') #Temporary folder
  
  message("Downloading AusTraits to '", path,"'")
  
  res <- utils::download.file(url=url, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
  
  if (res == 0) { #Warning and timeout hygiene
    w <- getOption('warn') #save option
    on.exit(options(warn = w)) #set options back to original
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
#' @param path A file path where AusTraits was previously downloaded
#' @param update Would you like the versions json be updated in case of new releases?
#'
#' @return A tibble containing version numbers and doi which can be used in load_austraits()
#' @examples
#' \dontrun{
#' austraits <- load_austraits(version = "3.0.2", path = "data/austraits")
#' }
#' @export

get_versions <- function(path = "data/austraits", update = TRUE){
  
  # Is path supplied?
  if(rlang::is_missing(path)){
    stop("File path must be supplied!")
  }
  
  # Does the path exist? 
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }
  
  # Load the json
  res <- load_json(path = path, update = update)  
  
  # Create metadata table
  create_metadata(res) %>% dplyr::as_tibble()
}

#' Retrieve the latest version of AusTraits
#'
#' @inheritParams load_austraits
#' @export
#' @return character string of latest version

get_version_latest <- function(path = "data/austraits", update = TRUE){
  # Is path supplied?
  if(rlang::is_missing(path)){
    stop("File path must be supplied!")
  }
  
  # Does the path exist? 
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }

  # Load the json
  res <- load_json(path = path, update = update)
  
  # Get all versions and remove the 'v'
  version_numbers <- stringr::str_extract(res$metadata$version, "[0-9]+\\.[0-9]+\\.[0-9]")

  # Order by numeric version
  version_numbers <- version_numbers[order(dplyr::desc(numeric_version(version_numbers)))]
  
  # Return the first value as the version we want
  dplyr::first(version_numbers)
}

