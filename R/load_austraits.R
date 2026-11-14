#' Load AusTraits database into R console
#'
#' Finds the requested release of AusTraits on Zenodo and loads it with
#' [traits.use::load_database()], which downloads the relational database
#' `.rds` to `path` (if it is not already there, or if the copy there does
#' not match the release's md5 checksum) and loads it.
#'
#' @param version character string - version number of database, e.g. "7.0.0"
#' @param doi character string - doi of particular version
#' @param path file path to where AusTraits will be downloaded
#' @param update if TRUE, the list of AusTraits versions (`austraits.json`)
#'   and the release's metadata are downloaded afresh
#'
#' @return a large list containing AusTraits data tables, of class
#'   `traits.build`. Its `sources` have the class `traits_build_sources` (see
#'   `?traits.use::traits_build_sources`), also for releases built before that
#'   class existed, so a reference can be looked up by key with `$` as well as
#'   `[[` and `[`, e.g. `austraits$sources$Falster_2005`.
#' @export
#' @seealso [get_versions()], [get_version_latest()],
#'   [traits.use::load_database()], which loads any traits.build database from
#'   its doi
#'
#' @examples
#' \dontrun{
#' austraits <- load_austraits(version = "7.0.0", path = "data/austraits")
#' }

load_austraits <- function(doi = NULL, version = NULL, path = "data/austraits", update = TRUE){
  # Is either doi or version supplied?
  if(is.null(doi) & is.null(version)){
    stop("Please supply a doi or version! Don't know which one you are after? Try get_versions()!")
  }

  # Is path supplied?
  if(is.null(path)){
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

  # Load the list of releases
  res <- load_json(path = path, update = update)
  ret <- create_metadata(res)

  # If only doi is provided, match it with its version number
  if(! is.null(doi)){
    if(! doi %in% ret$doi){
      rlang::abort("Requested version/doi is incorrect!")
    }
    version <- ret$version[ret$doi == doi]
  }

  # Check if version/doi is available
  if(! version %in% ret$version){
    rlang::abort("Requested version/doi is incorrect!")
  }

  doi <- ret$doi[ret$version == version]

  # austraits.json lists each release's files, so save the record
  # load_database() would otherwise fetch from Zenodo: that loads a cached
  # release offline with update = FALSE (caches made by austraits 3.x have
  # no record file), and saves a second request with update = TRUE
  record_saved <- save_record_json(res, doi, path)

  # Download (if needed) and load the release
  traits.use::load_database(doi = doi, path = path, update = update && !record_saved)
}

#' Save a release's record, as traits.use::load_database() caches it, from austraits.json
#'
#' The file name and fields follow traits.use's `load_record_json()`; the
#' offline test in test-load_austraits.R fails if they drift apart.
#' @return TRUE if saved, FALSE if austraits.json lists no files for `doi`
#' @noRd
#' @keywords internal
save_record_json <- function(res, doi, path) {
  hits <- res$hits$hits
  i <- which(hits$doi == doi)
  if(length(i) != 1 || is.null(hits$files) || !is.data.frame(hits$files[[i]]) || nrow(hits$files[[i]]) == 0) {
    return(FALSE)
  }
  f <- hits$files[[i]]
  record <- list(
    doi = doi, source = "zenodo", version = hits$metadata$version[i],
    files = data.frame(key = f$key, url = f$links$self,
                       md5 = stringr::str_match(f$checksum, "^md5:([0-9a-f]{32})$")[, 2])
  )
  file_json <- file.path(path, paste0(stringr::str_replace_all(doi, "[^A-Za-z0-9.]+", "_"), ".json"))
  jsonlite::write_json(record, file_json)
  TRUE
}


#' Load the austraits.json
#'
#' @noRd
#' @keywords internal

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
#' @noRd
#' @keywords internal

create_metadata <- function(res){
  # Version table
  ret <- res$hits$hits$metadata %>%
    dplyr::select(tidyselect::all_of(c("publication_date", "doi", "version"))) %>%
    dplyr::mutate(version = gsub("v", "", version) %>% numeric_version(),
                  id = stringr::str_remove_all(doi, stringr::fixed("10.5281/zenodo."))
                  )%>%  # set as numeric version for easier filtering
    dplyr::filter(version >= "3.0.2") %>% # exclude everything pre 3.0.2
    dplyr::mutate(version = as.character(version),
                  publication_date = as.Date(publication_date)) %>%  # change back as character
    dplyr::tibble() %>%
    dplyr::arrange(dplyr::desc(publication_date))

  ret
}

#' Print out AusTraits versions
#'
#' @param path A file path where AusTraits was previously downloaded
#' @param update Would you like the versions json be updated in case of new releases?
#'
#' @return A tibble containing version numbers and doi which can be used in load_austraits()
#' @examples
#' \dontrun{
#' get_versions(path = "data/austraits")
#' }
#' @export

get_versions <- function(path = "data/austraits", update = TRUE){

  # Is path supplied?
  if(is.null(path)){
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
  if(is.null(path)){
    stop("File path must be supplied!")
  }

  # Does the path exist?
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }

  # Load the json
  res <- load_json(path = path, update = update)

  # Create version table, newest first
  metadata <- create_metadata(res)

  # Grab the first version
  dplyr::first(metadata$version) %>% as.character()
}
