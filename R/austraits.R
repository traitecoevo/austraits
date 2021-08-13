#' A small subset dataset from austraits 
#'
#' @description A functional trait data extracted from four studies used for testing purposes
#' @format A list of 11 data frames, see names(austraits)
#' \describe{
#'   \item{traits}{trait data extracted from each study, tibble}
#'   \item{sites}{site level data for each study e.g. latitude, longitude, tibble}
#'   \item{contexts}{contextial information about each of the scenarios where data were collected, tibble}
#'   \item{methods}{excerpt of methods from original article, tibble}
#'   \item{excluded_data}{excluded data, tibble}
#'   \item{taxonomic_updates}{information on taxonomic name changes needed to align with current taxa, tibble}
#'   \item{taxa}{taxonomic classification for each species, tibble}
#'   \item{definitions}{meta data for all dataframes e.g. traits, value type, build info, tibble}
#'   \item{contributors}{details of researchers that contributed to each study, tibble}
#'   \item{sources}{citation information for each study, tibble}
#'   \item{build_info}{A description of the computing environment used to create this version of the dataset, including version number, git commit and R session_info, tibble}
#' }
#' @import RefManageR
"austraits"