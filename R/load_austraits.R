#' Load AusTraits database into R console
#'
#' Finds the requested release of AusTraits on Zenodo and loads it with
#' [traits.use::load_database()], which downloads the relational database
#' `.rds` (if it has not been downloaded before, or the copy does not match
#' the release's md5 checksum) and loads it.
#'
#' The download is kept in the folder that traits.use shares between all your
#' projects (see [traits.use::database_cache_dir()]), so each release is
#' downloaded once per computer. Use [traits.use::list_cached_databases()] and
#' [traits.use::clear_database_cache()] to see and delete what is there. Give
#' a `path`, such as `"data/austraits"` (the default before austraits 4.1.0),
#' to also keep a copy with a project.
#'
#' The list of releases (see [get_versions()]) is saved with the database and
#' fetched afresh once it is more than 60 days old, or when it does not have
#' the release asked for, so a new release is found without contacting
#' Zenodo on every load. Loading a release other than the latest says so,
#' once a session.
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
#' @param path a folder, such as `"data/austraits"`, to keep a copy of the
#'   release in (with the list of releases, so the folder loads offline on its
#'   own), besides the cache folder. If the folder already holds the release
#'   (e.g. saved there by an earlier version of austraits) and it matches
#'   Zenodo's checksum, it is copied into the cache rather than downloaded
#'   again; if Zenodo cannot be reached, the folder is used on its own. `NULL`
#'   (the default) keeps only the cache copy.
#' @param update whether to fetch the list of releases and the release's
#'   metadata from Zenodo afresh: `NULL` (the default) when the saved copies
#'   are more than 60 days old (`options(traits.use.update_after_days = )`
#'   changes this), `TRUE` always, `FALSE` never if there are saved copies, so
#'   a release loaded before loads offline. If fetching fails (e.g. offline),
#'   the saved copies are used.
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
#' # Also keeping a copy with a project
#' austraits <- load_austraits(version = "7.0.0", path = "data/austraits")
#'
#' # The observed means of each taxon and trait, published with releases after 7.0.0
#' means <- load_austraits(version = "8.0.0", means = TRUE)
#' }

load_austraits <- function(doi = NULL, version = NULL, path = NULL, update = NULL, means = FALSE){
  # Is either doi or version supplied?
  if(is.null(doi) & is.null(version)){
    stop("Please supply a doi or version! Don't know which one you are after? Try get_versions()!")
  }
  check_path(path)
  check_update(update)
  if(is.null(path)) {
    inform_old_default_folder()
  }

  #remove v from version
  if (!is.null(version)) {
    version <- stringr::str_remove_all(version, "v")
  }

  # A doi given as a url or with "doi:", as traits.use::load_database() takes it
  if(!is.null(doi)) {
    doi <- stringr::str_remove(doi, stringr::regex("^(https?://)?(dx\\.)?doi\\.org/", ignore_case = TRUE)) |>
      stringr::str_remove(stringr::regex("^doi:", ignore_case = TRUE)) |>
      stringr::str_remove("/+$")
  }

  # The list of releases, fetched afresh if it lacks the release asked for
  # (a release newer than the saved list)
  ret <- get_versions(path = path, update = update)
  not_listed <- function(ret) if(!is.null(doi)) !doi %in% ret$doi else !version %in% ret$version
  if(not_listed(ret) && is.null(update)) {
    ret <- get_versions(path = path, update = TRUE)
  }
  if(not_listed(ret)) {
    rlang::abort(c("Requested version/doi is incorrect!",
                   i = if(isFALSE(update)) "The saved list of releases may be out of date: try `update = TRUE`."))
  }

  # If only doi is provided, match it with its version number
  if(! is.null(doi)){
    version <- ret$version[ret$doi == doi]
  }
  doi <- ret$doi[ret$version == version]

  if(version != ret$version[1]) {
    rlang::inform(paste0("AusTraits ", ret$version[1], " is the latest release; loading ", version, "."),
                  .frequency = "once", .frequency_id = paste0("austraits_not_latest_", version))
  }

  # A release cached by austraits 3.x has no record file for load_database(),
  # but the austraits.json it saved lists the release's files, so offline it
  # still loads
  if(!is.null(path)) {
    save_legacy_record_json(doi, path)
  }

  # Download (if needed) and load the release
  traits.use::load_database(doi = doi, path = path, update = update, means = means)
}

#' Check `path`: NULL or a single folder path
#' @noRd
#' @keywords internal
check_path <- function(path) {
  if(!is.null(path) && (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path))) {
    rlang::abort("`path` must be a single folder path, or NULL for the cache folder only.")
  }
  invisible(path)
}

#' Check `update`: TRUE, FALSE or NULL
#' @noRd
#' @keywords internal
check_update <- function(update) {
  if(!is.null(update) && (!is.logical(update) || length(update) != 1 || is.na(update))) {
    rlang::abort("`update` must be TRUE, FALSE or NULL (fetch the list of releases when the saved copy is old).")
  }
  invisible(update)
}

#' Should a saved file be fetched afresh, given `update`?
#'
#' As traits.use's needs_update(), with the same option, so the list of
#' releases and the records of the releases age alike.
#' @noRd
#' @keywords internal
needs_update <- function(file, update) {
  if(!is.null(update)) return(isTRUE(update))
  days <- getOption("traits.use.update_after_days", 60)
  if(!is.numeric(days) || length(days) != 1 || is.na(days) || days < 0) {
    rlang::abort("The `traits.use.update_after_days` option must be a number of days.")
  }
  age <- difftime(Sys.time(), file.mtime(file), units = "days")
  is.na(age) || as.numeric(age) > days
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
      i = "To also keep a copy in 'data/austraits' (and use the releases already there), give `path = \"data/austraits\"`; otherwise that folder can be deleted."),
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

#' The file in which a folder holds a saved list of releases
#'
#' austraits_versions.json, saved by get_versions(), or, for a folder last
#' used by austraits 3.x, its austraits.json (Zenodo's own listing)
#' @return the file's path, or NULL if neither is there
#' @noRd
#' @keywords internal
saved_versions_file <- function(path) {
  for(file in file.path(path, c("austraits_versions.json", "austraits.json"))) {
    if(file.exists(file)) return(file)
  }
  NULL
}

#' Read a saved list of releases
#'
#' @param file a file found by saved_versions_file()
#' @return the table get_versions() returns
#' @noRd
#' @keywords internal
read_saved_versions <- function(file) {
  if(basename(file) == "austraits_versions.json") {
    versions <- jsonlite::fromJSON(file)
    return(dplyr::tibble(
      publication_date = as.Date(versions$publication_date),
      doi = versions$doi,
      version = versions$version,
      id = versions$id
    ))
  }

  hits <- jsonlite::fromJSON(file)$hits$hits
  format_versions(dplyr::tibble(
    version = hits$metadata$version,
    doi = hits$doi,
    publication_date = as.Date(hits$metadata$publication_date)
  ))
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
#' list in the cache folder (see [traits.use::database_cache_dir()]) so it can
#' be used offline. The saved list is used until it is more than 60 days old.
#'
#' @param path a folder to also save the list in, as for [load_austraits()].
#'   A list saved there before (e.g. by an earlier version of austraits) is
#'   used when the cache folder has none and Zenodo cannot be reached, or with
#'   `update = FALSE`.
#' @param update whether to fetch the list from Zenodo afresh: `NULL` (the
#'   default) when the saved copy is more than 60 days old
#'   (`options(traits.use.update_after_days = )` changes this), `TRUE` always,
#'   `FALSE` never if there is a saved copy. If fetching fails (e.g. offline),
#'   the saved copy is used.
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

get_versions <- function(path = NULL, update = NULL){
  check_path(path)
  check_update(update)
  if(is.null(path)) {
    inform_old_default_folder()
  }

  cache <- traits.use::database_cache_dir()

  # The saved list: the cache's, which came from Zenodo, else one saved in
  # `path`. As for a release's record in traits.use::load_database(), a list
  # from a project folder is used only when Zenodo cannot be reached or is
  # not to be asked (`update = FALSE`), and never copied into the cache.
  in_cache <- saved_versions_file(cache)
  saved <- in_cache
  if(is.null(saved) && !is.null(path)) {
    saved <- saved_versions_file(path)
  }
  fetch <- if(isFALSE(update)) is.null(saved) else is.null(in_cache) || needs_update(in_cache, update)
  if(!fetch) {
    return(read_saved_versions(saved))
  }

  versions <- tryCatch(
    format_versions(traits.use::list_versions(austraits_concept_doi)),
    error = function(e) {
      if(is.null(saved)) stop(e)
      message("Could not list the releases of AusTraits (", conditionMessage(e),
              "); using the list saved in '", dirname(saved), "'")
      NULL
    }
  )

  # Save only a list fetched from Zenodo, so a fallback never writes (and a
  # read-only folder still works offline)
  if(is.null(versions)) {
    return(read_saved_versions(saved))
  }

  for(folder in c(cache, path)) {
    save_versions(versions, folder)
  }
  versions
}

#' Save the list of releases in a folder, if it can be written to
#' @noRd
#' @keywords internal
save_versions <- function(versions, path) {
  if(!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  tryCatch({
    jsonlite::write_json(dplyr::mutate(versions, publication_date = as.character(.data$publication_date)),
                         file.path(path, "austraits_versions.json"))
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

#' Retrieve the latest version of AusTraits
#'
#' @inheritParams get_versions
#' @export
#' @return character string of latest version

get_version_latest <- function(path = NULL, update = NULL){
  get_versions(path = path, update = update)$version[1]
}
