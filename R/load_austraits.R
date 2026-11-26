#' Load AusTraits database into R console
#'
#' Finds the requested release of AusTraits on Zenodo and loads it with
#' [traits.use::load_database()], which downloads the relational database
#' `.rds` to `path` (if it is not already there, or if the copy there does
#' not match the release's md5 checksum) and loads it.
#'
#' By default the download is kept in the folder that traits.use shares
#' between all your projects (see [traits.use::database_cache_dir()]), so each
#' release is downloaded once per computer. Use
#' [traits.use::list_cached_databases()] and
#' [traits.use::clear_database_cache()] to see and delete what is there. Give a `path`,
#' such as `"data/austraits"` (the default before austraits 4.1.0), to keep a
#' copy with a project instead.
#'
#' With `means = TRUE` it loads the release's observed means instead: one row
#' per taxon and trait, computed from the release's records with
#' [traits.use::summarise_trait_means()] and published with it as
#' `austraits-<version>-observed-means.rds` and `.csv.gz` (releases after
#' 7.0.0; the `.rds`, exact, is read when present). They
#' are a smaller download than the database. To have both, load each.
#'
#' @param version character string - version number of database, e.g. "7.0.0"
#' @param doi character string - doi of particular version
#' @param path folder AusTraits is downloaded to and cached in. If `NULL` (the
#'   default), the folder shared by all your projects, given by
#'   [traits.use::database_cache_dir()].
#' @param update if `TRUE`, the list of AusTraits releases (see
#'   [get_versions()]) and the release's metadata are downloaded afresh; if
#'   that fails (e.g. offline), the copies saved in `path` are used. With
#'   `FALSE`, the saved copies are used without contacting Zenodo, so a release
#'   loaded before loads offline.
#' @param means if `TRUE`, load the release's observed means rather than the
#'   database; [traits.use::trait_means_dictionary()] defines their columns.
#'   A release from before the means were published gives an error saying so.
#'
#' @return a large list containing AusTraits data tables, of class
#'   `traits.build`. Its `sources` have the class `traits_build_sources` (see
#'   `?traits.use::traits_build_sources`), also for releases built before that
#'   class existed, so a reference can be looked up by key with `$` as well as
#'   `[[` and `[`, e.g. `austraits$sources$Falster_2005`. With `means = TRUE`,
#'   a tibble of the observed means.
#' @export
#' @seealso [get_versions()], [get_version_latest()],
#'   [traits.use::load_database()], which loads any traits.build database from
#'   its doi
#'
#' @examples
#' \dontrun{
#' # Downloaded once to the cache folder shared by all your projects
#' austraits <- load_austraits(version = "7.0.0")
#'
#' # Or kept with a project
#' austraits <- load_austraits(version = "7.0.0", path = "data/austraits")
#'
#' # The observed means of each taxon and trait, published with releases after 7.0.0
#' means <- load_austraits(version = "8.0.0", means = TRUE)
#' }

load_austraits <- function(doi = NULL, version = NULL, path = NULL, update = TRUE, means = FALSE){
  # Is either doi or version supplied?
  if(is.null(doi) & is.null(version)){
    stop("Please supply a doi or version! Don't know which one you are after? Try get_versions()!")
  }

  # The folder the list of releases is saved in; `path` itself goes to
  # load_database(), for which NULL means the cache folder
  folder <- resolve_path(path)
  if(is.null(path)) {
    inform_old_default_folder()
  }

  #remove v from version
  if (!is.null(version)) {
    version <- stringr::str_remove_all(version, "v")
  }

  # The list of releases
  ret <- get_versions(path = folder, update = update)

  # If only doi is provided, match it with its version number
  if(! is.null(doi)){
    if(! doi %in% ret$doi){
      rlang::abort(c("Requested version/doi is incorrect!",
                     i = if(! update) "The list of releases saved in `path` may be out of date: try `update = TRUE`."))
    }
    version <- ret$version[ret$doi == doi]
  }

  # Check if version/doi is available
  if(! version %in% ret$version){
    rlang::abort(c("Requested version/doi is incorrect!",
                   i = if(! update) "The list of releases saved in `path` may be out of date: try `update = TRUE`."))
  }

  doi <- ret$doi[ret$version == version]

  # A release cached by austraits 3.x has no record file for load_database(),
  # but the austraits.json it saved lists the release's files, so offline it
  # still loads
  save_legacy_record_json(doi, folder)

  # Download (if needed) and load the release
  traits.use::load_database(doi = doi, path = path, update = update, means = means)
}

#' The folder for `path`: the traits.use cache folder if NULL
#' @noRd
#' @keywords internal
resolve_path <- function(path) {
  if(is.null(path)) {
    return(traits.use::database_cache_dir())
  }
  if(!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    rlang::abort("`path` must be a single folder path, or NULL for the cache folder.")
  }
  path
}

#' Tell a user with AusTraits in the old default folder, once a session, that
#' it is no longer used by default (unless it is the cache folder, set with
#' the `traits.use.cache_dir` option)
#' @noRd
#' @keywords internal
inform_old_default_folder <- function() {
  old <- file.path("data", "austraits")
  if(!dir.exists(old) ||
     identical(normalizePath(old, winslash = "/", mustWork = FALSE),
               normalizePath(traits.use::database_cache_dir(), winslash = "/", mustWork = FALSE))) {
    return(invisible(FALSE))
  }
  rlang::inform(
    c("AusTraits is now kept in the folder shared by all your projects (see `database_cache_dir()`), not in 'data/austraits'.",
      i = "To keep using the copy in 'data/austraits', give `path = \"data/austraits\"`; otherwise that folder can be deleted."),
    .frequency = "once", .frequency_id = "austraits_old_default_folder"
  )
  invisible(TRUE)
}

# The concept doi of AusTraits on Zenodo: it stands for every release
austraits_concept_doi <- "10.5281/zenodo.3568417"

#' Save a release's record, as traits.use::load_database() caches it, from
#' the austraits.json saved by austraits 3.x
#'
#' Does nothing if the record file is there already, or if there is no
#' austraits.json or it lists no files for `doi`. The file name and fields
#' follow traits.use's `load_record_json()`; the offline test in
#' test-load_austraits_offline.R fails if they drift apart.
#' @return TRUE if saved, otherwise FALSE
#' @noRd
#' @keywords internal
save_legacy_record_json <- function(doi, path) {
  file_record <- file.path(path, paste0(stringr::str_replace_all(doi, "[^A-Za-z0-9.]+", "_"), ".json"))
  file_legacy <- file.path(path, "austraits.json")
  if(file.exists(file_record) || ! file.exists(file_legacy)) {
    return(FALSE)
  }

  hits <- jsonlite::fromJSON(file_legacy)$hits$hits
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
  jsonlite::write_json(record, file_record)
  TRUE
}

#' The releases of AusTraits saved in `path`
#'
#' Saved by get_versions() in austraits_versions.json, or, for a folder last
#' used by austraits 3.x, its austraits.json (Zenodo's own listing)
#' @return the table get_versions() returns, or NULL if neither file is there
#' @noRd
#' @keywords internal
read_saved_versions <- function(path) {
  file_versions <- file.path(path, "austraits_versions.json")
  file_legacy <- file.path(path, "austraits.json")

  if(file.exists(file_versions)) {
    versions <- jsonlite::fromJSON(file_versions)
    return(dplyr::tibble(
      publication_date = as.Date(versions$publication_date),
      doi = versions$doi,
      version = versions$version,
      id = versions$id
    ))
  }

  if(file.exists(file_legacy)) {
    hits <- jsonlite::fromJSON(file_legacy)$hits$hits
    return(format_versions(dplyr::tibble(
      version = hits$metadata$version,
      doi = hits$doi,
      publication_date = as.Date(hits$metadata$publication_date)
    )))
  }

  NULL
}

#' Shape traits.use::list_versions() output as get_versions() always has
#'
#' Versions without their "v", an `id` column, newest first, and only the
#' releases from 3.0.2, the first that austraits can load.
#' @noRd
#' @keywords internal
format_versions <- function(versions) {
  versions |>
    dplyr::mutate(version = stringr::str_remove(.data$version, "^v")) |>
    dplyr::filter(numeric_version(.data$version, strict = FALSE) >= "3.0.2") |>
    dplyr::mutate(id = stringr::str_remove_all(.data$doi, stringr::fixed("10.5281/zenodo."))) |>
    dplyr::select("publication_date", "doi", "version", "id") |>
    dplyr::arrange(dplyr::desc(.data$publication_date))
}

#' Print out AusTraits versions
#'
#' Lists the releases of AusTraits with [traits.use::list_versions()], which
#' lists the releases of any traits.build database from its doi, and saves the
#' list in `path` so it can be used offline.
#'
#' @param path folder the list is saved in, as for [load_austraits()]: if
#'   `NULL` (the default), the folder shared by all your projects, given by
#'   [traits.use::database_cache_dir()].
#' @param update if `TRUE`, the list is fetched from Zenodo; if that fails
#'   (e.g. offline), the copy saved in `path` is used. With `FALSE`, the saved
#'   copy is used, if there is one.
#'
#' @return A tibble of the releases, newest first: `publication_date`, `doi`,
#'   `version` (e.g. "7.0.0") and `id` (the Zenodo record id). Pass a version to
#'   [load_austraits()].
#' @seealso [traits.use::list_versions()]
#' @examples
#' \dontrun{
#' get_versions()
#' }
#' @export

get_versions <- function(path = NULL, update = TRUE){

  if(is.null(path)) {
    inform_old_default_folder()
  }
  path <- resolve_path(path)

  # Does the path exist?
  if(! file.exists(path)) {
    dir.create(path, recursive=TRUE, showWarnings=FALSE) #Create folder
  }

  saved <- if(update) NULL else read_saved_versions(path)
  if(! is.null(saved)) {
    return(saved)
  }

  versions <- tryCatch(
    format_versions(traits.use::list_versions(austraits_concept_doi)),
    error = function(e) {
      saved <- read_saved_versions(path)
      if(is.null(saved)) stop(e)
      message("Could not list the releases of AusTraits (", conditionMessage(e),
              "); using the list saved in '", path, "'")
      NULL
    }
  )

  # Save only a list fetched from Zenodo, so a fallback never writes (and a
  # read-only folder still works offline)
  if(is.null(versions)) {
    return(read_saved_versions(path))
  }

  jsonlite::write_json(dplyr::mutate(versions, publication_date = as.character(.data$publication_date)),
                       file.path(path, "austraits_versions.json"))
  versions
}

#' Retrieve the latest version of AusTraits
#'
#' @inheritParams get_versions
#' @export
#' @return character string of latest version

get_version_latest <- function(path = NULL, update = TRUE){
  get_versions(path = path, update = update)$version[1]
}
