#' @title Generic for outputting a nice summary for austraits objects
#'
#' @name print.traits.build
#' @param database traits.build database
#' @param \dots passed to print
#'
#' @return nicely printed table
#' @export

print.traits.build <- function(database, ...){
  
  # Setting up
  database_name <- database$metadata$title
  
  traits.build_version <- at_six$metadata$related_identifiers |> 
    convert_list_to_df2() |> 
    dplyr::filter(resource_type == "software") |> 
    dplyr::pull(version)
  
  version <- database$build_info$version %>% as.character()
  nrecords <- nrow(database$traits)
  nspecies <- unique(database$traits$taxon_name) %>% length()
  ntraits <- unique(database$traits$trait_name) %>% length()

  
  if(package_version(version) <= '3.0.2'){
    
    # Setting up
    database_name <- database$definitions$austraits$description
  
    fun <- function() {
      cli::cli_h1("This is {version} of {database_name}!")
      
      cli::cli_bullets(c(
        "i" = "A database built using traits.build version {traits.build_version}",
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
      cli::cli_li("{.emph definitions}: A copy of the definitions for all tables and terms. Information included here was used to process data and generate any documentation for the study.")
      cli::cli_li("{.emph contributors}: A table of people contributing to each study.")
      cli::cli_li("{.emph sources}: Bibtex entries for all primary and secondary sources in the compilation.")
      cli::cli_li("{.emph build_info}:  A description of the computing environment used to create this version of the dataset, including version number, git commit and R session_info.")
      cli::cli_end()
      
      cli::cli_alert_info("To access a component, try using the $ e.g. austraits$traits")
    }
    
    fun()
  } else{
    fun <- function() {
            cli::cli_h1("This is {version} of {database_name}!")
      
      cli::cli_bullets(c(
        "i" = "A database built using traits.build version {traits.build_version}",
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
      cli::cli_li("{.emph sources}: Bibtex entries for all primary and secondary sources in the compilation.")
      cli::cli_li("{.emph definitions}: A copy of the definitions for all tables and terms. Information included here was used to process data and generate any documentation for the study.")
      cli::cli_li("{.emph schema}: A copy of the schema for all tables and terms. Information included here was used to process data and generate any documentation for the study.")
      cli::cli_li("{.emph metadata}: Metadata associated with the dataset, including title, creators, license, subject, funding sources.")
      cli::cli_li("{.emph build_info}: A description of the computing environment used to create this version of the dataset, including version number, git commit and R session_info.")
      cli::cli_end()
      
      cli::cli_alert_info("To access a component, try using the $ e.g. austraits$traits")
    }
    
    fun()
  }
}


