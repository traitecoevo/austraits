#' @title Generic for outputting a nice summary for austraits objects
#'
#' @name print.traits.build
#' @param x traits.build database
#' @param \dots passed to print
#' @export
#' @return nicely printed table

print.traits.build <- function(x, ...){

  # Setting up printing information
  version <- x$build_info$version %>% as.character()
  nrecords <- nrow(x$traits)
  nspecies <- unique(x$traits$taxon_name) %>% length()
  ntraits <- unique(x$traits$trait_name) %>% length()
  
  if(tibble::is_tibble(x)) return(x)
  
  if(check_compatibility(x)){
    database_name <- x$metadata$title
    
    traits.build_version <- x$metadata$related_identifiers |> 
      convert_list_to_df2() |> 
      dplyr::filter(resource_type == "software") |> 
      dplyr::pull(version)
    
    nice_summary_output <- function() {
      cli::cli_h1("This is {version} of {database_name}!")
      
      cli::cli_bullets(c(
        "i" = "This database is built using traits.build version {traits.build_version}",
        "i" = "This database contains a total of {nrecords} records, for {nspecies} taxa and {ntraits} traits."
      )
      )
      
      cli::cli_h2("This object is a 'list' with the following components:")
      cli::cli_div(theme = list(span.emph = list(color = "forestgreen")))
      cli::cli_ul()
      cli::cli_li("{.emph traits}: A table containing measurements of traits.")
      cli::cli_li("{.emph locations}: A table containing observations of location/site characteristics associated with information in `traits`. Cross referencing between the two dataframes is possible using combinations of the variables `dataset_id`, `location_name`.")
      cli::cli_li("{.emph contexts}: A table containing observations of contextual characteristics associated with information in `traits`. Cross referencing between the two dataframes is possible using combinations of the variables `dataset_id`, `link_id`, and `link_vals`.")
      cli::cli_li("{.emph methods}: A table containing details on methods with which data were collected, including time frame and source. Cross referencing with the `traits` table is possible using combinations of the variables `dataset_id`, `trait_name`.")
      cli::cli_li("{.emph excluded_data}: A table of data that did not pass quality test and so were excluded from the master dataset.")
      cli::cli_li("{.emph taxonomic_updates}: A table of all taxonomic changes implemented in the construction of AusTraits. Changes are determined by comapring against the APC (Australian Plant Census) and APNI (Australian Plant Names Index).")
      cli::cli_li("{.emph taxa}: A table containing details on taxa associated with information in `traits`. This information has been sourced from the APC (Australian Plant Census) and APNI (Australian Plant Names Index) and is released under a CC-BY3 license.")
      cli::cli_li("{.emph contributors}: A table of people contributing to each study.")
      cli::cli_li("{.emph identifiers}: A table of identifiers that cross-references observations between datasets or with other data resources such as museum or herbarium specimens.")
      cli::cli_li("{.emph sources}: Bibtex entries for all primary and secondary sources in the compilation.")
      cli::cli_li("{.emph definitions}: A copy of the definitions for all tables and terms. Information included here was used to process data and generate any documentation for the study.")
      cli::cli_li("{.emph schema}: A copy of the schema for all tables and terms. Information included here was used to process data and generate any documentation for the study.")
      cli::cli_li("{.emph metadata}: Metadata associated with the dataset, including title, creators, license, subject, funding sources.")
      cli::cli_li("{.emph build_info}: A description of the computing environment used to create this version of the dataset, including version number, git commit and R session_info.")
      cli::cli_end()
      
      cli::cli_alert_info("To access a component, try using the $ e.g. austraits$traits")
    }
    
    nice_summary_output()
  } else { # If not compatible (i.e. old version of database)
    # Setting up
    database_name <- x$definitions$austraits$description
    
    old_version_nice_output <- function() {
      cli::cli_h1("This database contains a total of {nrecords} records, for {nspecies} taxa and {ntraits} traits.")
      
      cli::cli_h2("This object is a 'list' with the following components:")
      cli::cli_div(theme = list(span.emph = list(color = "forestgreen")))
      cli::cli_ul()
      cli::cli_li("{.emph traits}: A table containing measurements of traits.")
      cli::cli_li("{.emph locations}: A table containing observations of location/site characteristics associated with information in `traits`. Cross referencing between the two dataframes is possible using combinations of the variables `dataset_id`, `location_name`.")
      cli::cli_li("{.emph contexts}: A table containing observations of contextual characteristics associated with information in `traits`. Cross referencing between the two dataframes is possible using combinations of the variables `dataset_id`, `link_id`, and `link_vals`.")
      cli::cli_li("{.emph methods}: A table containing details on methods with which data were collected, including time frame and source. Cross referencing with the `traits` table is possible using combinations of the variables `dataset_id`, `trait_name`.")
      cli::cli_li("{.emph excluded_data}: A table of data that did not pass quality test and so were excluded from the master dataset.")
      cli::cli_li("{.emph taxonomic_updates}: A table of all taxonomic changes implemented in the construction of AusTraits. Changes are determined by comapring against the APC (Australian Plant Census) and APNI (Australian Plant Names Index).")
      cli::cli_li("{.emph taxa}: A table containing details on taxa associated with information in `traits`. This information has been sourced from the APC (Australian Plant Census) and APNI (Australian Plant Names Index) and is released under a CC-BY3 license.")
      cli::cli_li("{.emph definitions}: A copy of the definitions for all tables and terms. Information included here was used to process data and generate any documentation for the study.")
      cli::cli_li("{.emph contributors}: A table of people contributing to each study.")
      cli::cli_li("{.emph identifiers}: A table of identifiers that cross-references observations between datasets or with other data resources such as museum or herbarium specimens.")
      cli::cli_li("{.emph sources}: Bibtex entries for all primary and secondary sources in the compilation.")
      cli::cli_li("{.emph build_info}:  A description of the computing environment used to create this version of the dataset, including version number, git commit and R session_info.")
      cli::cli_end()
      
      cli::cli_alert_info("To access a component, try using the $ e.g. austraits$traits")
    }
    
    old_version_nice_output()
  }
}


