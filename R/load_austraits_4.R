

load_austraits_4 <- function(version = "v3.0.2", path = "ignore/data/austraits", update){
  # Does the path exist? 
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }
  
  file_json <- file.path(path, "austraits.json")
  
  # Does the .json exist?
  if(! file.exists(file_json) | update = TRUE){
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
         v3.0.0 = dload_rds(version, file_json, path))
  

  # Assign class
  attr(data, "class") <- "austraits"
  
  data
}

# Function for .rds AusTraits

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
  
  # Loading the .rds
  message("Loading data from '", file_path,"'")
  data <- readRDS(file_path) 
}

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


# ret <- dplyr::tibble(date = res$hits$hits$metadata$publication_date,
#                      version = stringr::str_extract(res$hits$hits$metadata$version, "[0-9]+\\.[0-9]+\\.[0-9]"),
#                      doi = res$hits$hits$metadata$doi) 
