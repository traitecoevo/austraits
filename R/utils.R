#' Convert dataframe to list
#'
#' @description Convert a dataframe to a named list, 
#' useful when converting a dataframe a to yaml.
#'
#' @param df A dataframe
#' @return A (yaml) list
#' @export
#' @examples convert_df_to_list(dplyr::starwars)
convert_df_to_list <- function(df) {
  attr(df, "out.attrs") <- NULL
  unname(lapply(split(df, seq_len(nrow(df))), as.list))
}

#' Convert list with single entries to dataframe
#' 
#' @description Convert a list with a single level of entries to a dataframe, 
#' useful when converting a yaml into a dataframe.
#'
#' @param my_list A list with single entries
#' @return A tibble with two columns
#' @export
#' @examples \dontrun{
#' convert_list_to_df1(as.list(dplyr::starwars)[2])
#' }
convert_list_to_df1 <- function(my_list) {

  for (f in names(my_list)) {
    if (is.null(my_list[[f]]))
      my_list[[f]] <- NA
  }

  tibble::tibble(key = names(my_list), value = unname(unlist(my_list)))
}

#' Convert list of lists to dataframe
#' 
#' @description Convert a list of lists to a dataframe, 
#' useful when converting a multi-level yaml into a dataframe.
#' Function required that every list have same named elements.
#'
#' @param my_list A list of lists to dataframe
#' @param as_character A logical value, indicating whether the values are read as character
#' @param on_empty Value to return if my_list is NULL, NA or is length == 0, default = NA
#' @return tibble
#' @export
#' @examples demo_list1 <- list(word1 = "this", word2 = "is", word3 = "an", word4 = "example", word5 = "list")
#' demo_list2 <- list(word1 = "and", word2 = "a", word3 = "second", word4 = "list", word5 = "also")
#' combined_list <- list(demo_list1, demo_list2)
#' convert_list_to_df2(combined_list)

convert_list_to_df2 <- function(my_list, as_character = TRUE, on_empty = NA) {
  
  if (is.null(my_list) || any(is.na(my_list)) || length(my_list) == 0)
    return(on_empty)
  
  if (as_character)
    my_list <- lapply(my_list, lapply, as.character)
  
  dplyr::bind_rows(lapply(my_list, tibble::as_tibble))
}

#' Notify user the function they are using is no longer support
#'
#' @param database traits.build database (list object)
#'
#' @return cli messaging about the function name, the version of AusTraits they are using and their next options
#' @keywords internal
#' @noRd

function_not_supported <- function(database, ...){
  
  # Extract function name
  function_name <- as.character(sys.calls()[[1]]) %>% dplyr::last()
  
  # Determine if traits table or traits.build object
  if( is.null(dim(database))){
    # Extract AusTraits version
    AusTraits_version <- print_version(database)
  } else
    AusTraits_version <- "< 5.0.0"
  
  # Formulate message
  cli::cli_abort(c(
    "x" = "{function_name} no longer supports this version of AusTraits, {AusTraits_version}",
    "i" = "You can either update to a newer version of the data using `load_austraits()` OR",
    "i" = "Install an older version of the package", 
    "i" = "See https://github.com/traitecoevo/austraits for details."
  ),
  call = rlang::caller_env()
  )
}


#' Retrieve traits table if user passes traits.build object.
#'
#' @param database traits.build database or traits table in a traits.build database


get_traits_table <- function(database){
  if( is.null(dim(database)) ){
    traits <- database$traits
  } else{
    traits <- database
  }
  
  return(traits)
}


#' @title NA hygiene
#'
#' @description Helper function to convert character strings of NA into true NA
#' @usage clean_NA(x)
#' @param trait_data The traits table in a traits.build database
#' @param definitions The definitions tibble from a traits.build database
#' @return vector where strings of NA are treated as true NA
#' @examples 
#' \dontrun{
#' clean_NA(c("NA", 1, 2, 3))) %>% is.na()
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au

#' @noRd

clean_NA <- function(x) {
  ifelse(x == "NA", NA_character_, x)
}

#' Print version of AusTraits object
#'
#' @param database traits.build database (list object)
#' @return binary version for switch statements
#' @noRd
#' @keywords internal
#' 
print_version <- function(database){
  database$build_info$version %>% as.character()
}

#' Give the reference columns of a methods table their current names
#'
#' Databases built with traits.build before 2.3 name the methods table's
#' reference columns `source_primary_*`, `source_secondary_*` and
#' `source_original_dataset_*`. Renames the dataset-level ones to their current
#' names, `dataset_id_*`, `secondary_id` and `secondary_citation`, so the
#' functions here read either; `source_primary_key` and
#' `source_original_dataset_*` have no current equivalent (an observation names its
#' reference in `traits$source_id`) and are left as they are.
#'
#' @param methods The methods table of a traits.build database
#' @return The methods table, with its reference columns renamed
#' @noRd
#' @keywords internal
methods_with_current_source_names <- function(methods) {
  renames <- c(
    dataset_id_type = "source_primary_type",
    dataset_id_citation = "source_primary_citation",
    secondary_id = "source_secondary_key",
    secondary_citation = "source_secondary_citation"
  )
  renames <- renames[renames %in% names(methods) & !names(renames) %in% names(methods)]
  dplyr::rename(methods, dplyr::all_of(renames))
}

#' Reference columns of a methods table, current and old
#' @noRd
#' @keywords internal
methods_source_columns <- c(
  "dataset_id_type", "dataset_id_citation", "secondary_id", "secondary_citation",
  "source_primary_key", "source_primary_type", "source_primary_citation",
  "source_secondary_key", "source_secondary_citation",
  "source_original_dataset_key", "source_original_dataset_citation"
)

#' Format a reference as a citation
#'
#' Formats a reference the way traits.build writes the citations in the
#' methods table, so citations formatted here read the same as those.
#'
#' @param bib A single reference, as a BibEntry object
#' @return Character string of the formatted reference
#' @noRd
#' @keywords internal
format_citation <- function(bib) {
  format_bib <- utils::getFromNamespace("format.BibEntry", "RefManageR")
  oldopts <- RefManageR::BibOptions(first.inits = TRUE, max.names = 1000, style = "markdown")
  on.exit(RefManageR::BibOptions(oldopts))
  # Titles are quoted with `dQuote()`; keep them ASCII whatever the session's option
  oldquotes <- options(useFancyQuotes = FALSE)
  on.exit(options(oldquotes), add = TRUE)

  out <- format_bib(bib, .sort = FALSE)
  out <- gsub("[] ", "", out, fixed = TRUE)
  out <- gsub("\\n", " ", out)
  out <- gsub("  ", " ", out)
  out <- gsub("DOI:", " doi: ", out, fixed = TRUE)
  out <- gsub("URL:", " url: ", out, fixed = TRUE)
  out <- gsub("(pp?\\. \\d+)[\u2013\u2014](\\d+)", "\\1-\\2", out)
  if (tolower(bib$bibtype) == "article") out <- gsub("In:", " ", out)
  out
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL