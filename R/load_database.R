#' Load any traits.build database into R console
#'
#' @description Works like [load_austraits()], but accepts the doi of any
#' traits.build formatted database. The doi is resolved to its repository, the
#' relational database `.rds` file is downloaded to `path` (if not already
#' there) and then loaded. Zenodo and Figshare records are supported, as are
#' dois that resolve directly to an `.rds` file.
#'
#' @param doi character string - doi of the database, e.g. "10.5281/zenodo.11188867".
#' A Zenodo concept doi will load the latest version of that record.
#' @param path file path to where the database will be downloaded
#' @param update if TRUE, the record's metadata .json will be re-downloaded
#'
#' @return a large list containing traits.build data tables
#' @export
#' @seealso load_austraits
#'
#' @examples
#' \dontrun{
#' austraits <- load_database(doi = "10.5281/zenodo.11188867", path = "data/austraits")
#' }

load_database <- function(doi = NULL, path = "data/traits.build", update = TRUE){
  # Is doi supplied?
  if(is.null(doi)){
    stop("Please supply a doi!")
  }

  # Is path supplied?
  if(is.null(path)){
    stop("File path must be supplied!")
  }

  # Remove any url prefix from doi
  doi <- stringr::str_remove(doi, "^(https?://)?(dx\\.)?doi\\.org/") %>%
    stringr::str_remove("^doi:")

  # Does the path exist?
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }

  # Load the record metadata
  record <- load_record_json(doi = doi, path = path, update = update)

  # Setting up the pars
  files <- record$files
  is_rds <- grepl("\\.rds$", files$key, ignore.case = TRUE)

  if(! any(is_rds)){
    rlang::abort(paste0("No .rds file found for doi '", doi,
                        "'. Is this a traits.build database?"))
  }

  files <- files[is_rds, ]

  # Ignore flattened database files
  if(nrow(files) > 1) {
    files <- files[!stringr::str_detect(files$key, "flattened"), ]
  }

  if(nrow(files) > 1) {
    rlang::abort(paste0("More than one .rds file found for doi '", doi, "': ",
                        paste(files$key, collapse = ", ")))
  }

  url <- files$url
  file_nm <- file.path(path, basename(files$key))

  #Check if doi is downloaded, if not download
  if(! file.exists(file_nm)){
    # Downloading file
    download_austraits(url, file_nm, path = path)
  }

  # Loading the .rds
  message("Loading data from '", file_nm,"'")
  data <- readRDS(file_nm)

  # Check it looks like a traits.build database
  if(! is.list(data) | ! all(c("traits", "taxa", "methods") %in% names(data))){
    rlang::abort(paste0("'", file_nm, "' is not a traits.build database!"))
  }

  # Assign class
  attr(data, "class") <- "traits.build"

  data
}


#' Load the record metadata .json for a doi
#'
#' @description Resolves the doi to its repository and saves a .json listing
#' the record's files (key and download url) to `path`
#'
#' @noRd
#' @keywords internal

load_record_json <- function(doi, path, update){
  # Set the directory path to json
  file_json <- file.path(path, paste0(stringr::str_replace_all(doi, "[^A-Za-z0-9.]+", "_"), ".json"))

  # Does the .json exist?
  if(! file.exists(file_json) | update == TRUE){
    # Retrieve the record
    res <- resolve_doi_files(doi)
    # Save it
    jsonlite::write_json(res, file_json)
  }

  # Load the json
  jsonlite::fromJSON(file_json)
}

#' Find the files associated with a doi
#'
#' @param doi character string - doi of the record
#' @return list with the doi, source repository and a dataframe of files (key and url)
#' @noRd
#' @keywords internal

resolve_doi_files <- function(doi){
  # Zenodo dois contain the record id, so no need to resolve them
  if(stringr::str_detect(doi, stringr::regex("^10\\.5281/zenodo\\.[0-9]+$", ignore_case = TRUE))){
    landing_url <- paste0("https://zenodo.org/records/", stringr::str_extract(doi, "[0-9]+$"))
  } else {
    landing_url <- resolve_doi(doi)
  }

  # Zenodo
  if(stringr::str_detect(landing_url, "zenodo\\.org/")){
    id <- stringr::str_match(landing_url, "zenodo\\.org/(?:api/)?records?/([0-9]+)")[, 2]

    if(is.na(id)){
      rlang::abort(paste0("Could not find Zenodo record id for doi '", doi, "'"))
    }

    # Concept record ids redirect to the latest version
    res <- jsonlite::read_json(paste0("https://zenodo.org/api/records/", id),
                               simplifyVector = TRUE)

    files <- data.frame(key = res$files$key, url = res$files$links$self)

    return(list(doi = doi, source = "zenodo", version = res$metadata$version, files = files))
  }

  # Figshare
  if(stringr::str_detect(landing_url, "figshare\\.com/")){
    ids <- stringr::str_match(landing_url, "/([0-9]+)(?:/([0-9]+))?/?$")

    if(is.na(ids[, 2])){
      rlang::abort(paste0("Could not find Figshare article id for doi '", doi, "'"))
    }

    api_url <- paste0("https://api.figshare.com/v2/articles/", ids[, 2])
    if(!is.na(ids[, 3])) {
      api_url <- paste0(api_url, "/versions/", ids[, 3])
    }

    res <- jsonlite::read_json(api_url, simplifyVector = TRUE)

    files <- data.frame(key = res$files$name, url = res$files$download_url)

    return(list(doi = doi, source = "figshare", version = as.character(res$version), files = files))
  }

  # doi resolves directly to an .rds file
  if(grepl("\\.rds(\\?.*)?$", landing_url, ignore.case = TRUE)){
    files <- data.frame(key = basename(stringr::str_remove(landing_url, "\\?.*$")), url = landing_url)

    return(list(doi = doi, source = "url", version = NA_character_, files = files))
  }

  rlang::abort(paste0("doi '", doi, "' resolves to '", landing_url,
                      "', which is not a supported repository (Zenodo, Figshare or a direct .rds link)"))
}

#' Resolve a doi to its landing page url using the doi.org handle API
#'
#' @noRd
#' @keywords internal

resolve_doi <- function(doi){
  res <- tryCatch(
    suppressWarnings(jsonlite::read_json(paste0("https://doi.org/api/handles/", doi), simplifyVector = TRUE)),
    error = function(e) NULL
  )

  if(is.null(res) || res$responseCode != 1){
    rlang::abort(paste0("Could not resolve doi '", doi, "'"))
  }

  res$values$data$value[res$values$type == "URL"][[1]]
}
