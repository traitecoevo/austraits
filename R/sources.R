#' Look up references in a database's `sources` by key
#'
#' @description
#' The `sources` component of a traits.build database holds the bibliographic
#' references for every dataset, as a [RefManageR::BibEntry()] object keyed by
#' citation key (for example `"Falster_2005"`). austraits gives it the extra
#' class `traits_build_sources`, which makes `$` look an entry up by its key.
#' Any of these return the reference for `Falster_2005`:
#'
#' ```
#' database$sources$Falster_2005
#' database$sources[["Falster_2005"]]
#' database$sources["Falster_2005"]
#' ```
#'
#' and `database$sources[c("Falster_2003", "Falster_2005")]` returns several.
#' Use `names(database$sources)` to list the keys.
#'
#' @details
#' On a plain `BibEntry`, `$` is field access (`sources$title` returns the
#' title of every entry), so `sources$Falster_2005` returns a list of `NULL`s,
#' one per entry, instead of the reference. With the `traits_build_sources`
#' class, `$` looks up by key when the name is a key, and otherwise falls back
#' to field access, so `sources$title` and `sources$author` work as before.
#'
#' The class only adds behaviour; everything else is inherited from
#' `BibEntry`. Printing, [format()], [RefManageR::WriteBib()] and
#' [RefManageR::toBiblatex()] give the same output with or without it.
#' `[`, `[[` and `c()` keep the class (for `c()`, when the first argument has
#' it), as do the RefManageR methods built on `[`, such as `sort()`,
#' `unique()` and `head()`. RefManageR's replacement functions (`$<-`, `[<-`,
#' `[[<-`) and `merge()` return a plain `BibEntry`.
#'
#' [bind_databases()], [load_austraits()] and [extract_data()] (and so the
#' other `extract_*()` functions) add the class to the `sources` they return,
#' including for databases built before the class existed. An object carrying
#' the class behaves as a plain `BibEntry` in a session where austraits is not
#' loaded.
#'
#' @param x A `traits_build_sources` object: the `sources` component of a
#'   traits.build database.
#' @param name A citation key, or a `BibEntry` field name.
#' @param i,j,...,drop Indices, as for RefManageR's `[` method for `BibEntry`.
#' @param recursive Ignored, as for RefManageR's `c()` method for `BibEntry`.
#'
#' @return For `$`, the entry with key `name` as a `traits_build_sources`
#'   object of length one or, when `name` is not a key, the value of field
#'   `name` for every entry. For `[` and `c()`, a `traits_build_sources`
#'   object.
#'
#' @seealso [bind_databases()], [load_austraits()], [RefManageR::BibEntry()]
#' @name traits_build_sources
#'
#' @examples
#' sources <- c(
#'   RefManageR::BibEntry(
#'     bibtype = "Article", key = "Falster_2003",
#'     title = "Leaf size and angle vary widely across species",
#'     author = "Daniel S. Falster and Mark Westoby",
#'     journal = "New Phytologist", year = 2003
#'   ),
#'   RefManageR::BibEntry(
#'     bibtype = "Article", key = "Falster_2005",
#'     title = "Alternative height strategies among 45 dicot rain forest species",
#'     author = "Daniel S. Falster and Mark Westoby",
#'     journal = "Journal of Ecology", year = 2005
#'   )
#' )
#' database <- list(sources = sources)
#'
#' # Without the class, `$` with a key returns a list of NULLs
#' database$sources$Falster_2005
#'
#' # bind_databases(), load_austraits() and extract_data() add the class
#' class(database$sources) <- c("traits_build_sources", class(database$sources))
#' database$sources$Falster_2005
#' database$sources[["Falster_2005"]]
#'
#' # Names that are not keys are still field access
#' database$sources$year
NULL

#' @rdname traits_build_sources
#' @method $ traits_build_sources
#' @export
`$.traits_build_sources` <- function(x, name) {
  # Read the keys directly: `names()` on a BibEntry is itself `x$key`, so
  # calling it here would recurse
  keys <- unlist(lapply(unclass(x), attr, "key"))
  if (name %in% keys) {
    return(x[[name]])
  }
  NextMethod()
}

#' @rdname traits_build_sources
#' @method [ traits_build_sources
#' @export
`[.traits_build_sources` <- function(x, i, j, ..., drop = FALSE) {
  # RefManageR's `[` returns indices rather than a BibEntry when it calls
  # itself to run a search, and an empty list when nothing matches;
  # as_traits_build_sources() leaves both unchanged
  as_traits_build_sources(NextMethod())
}

#' @rdname traits_build_sources
#' @method c traits_build_sources
#' @export
c.traits_build_sources <- function(..., recursive = FALSE) {
  as_traits_build_sources(NextMethod())
}

#' Give a BibEntry the `traits_build_sources` class
#'
#' Prepends the class to a `BibEntry`, leaving anything else (including an
#' object that already has the class) unchanged.
#'
#' @param x Object to classify
#' @return `x`, with class `traits_build_sources` if it is a `BibEntry`
#' @noRd
#' @keywords internal
as_traits_build_sources <- function(x) {
  if (inherits(x, "BibEntry") && !inherits(x, "traits_build_sources")) {
    class(x) <- c("traits_build_sources", class(x))
  }
  x
}

#' Give a database's `sources` the `traits_build_sources` class
#'
#' @param database traits.build database (list object)
#' @return `database`, with `sources` classified when it is a `BibEntry`
#' @noRd
#' @keywords internal
classify_sources <- function(database) {
  if (inherits(database[["sources"]], "BibEntry")) {
    database[["sources"]] <- as_traits_build_sources(database[["sources"]])
  }
  database
}
