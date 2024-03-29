% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trait_pivot_wider.R
\name{trait_pivot_wider}
\alias{trait_pivot_wider}
\title{Pivot long format austrait data into a wide format}
\usage{
trait_pivot_wider(traits)
}
\arguments{
\item{traits}{The traits table from austraits list object}
}
\value{
list of five tibbles in wide format
}
\description{
\code{trait_pivot_wider} "widens" long format data ("tidy data")

AusTraits data is organised in a long format where observations are on different rows and the type of observation is denoted by various identifying columns (e.g trait_name, dataset_id, observation_id etc.)
This function converts the data into wide format so that each trait in it's own column.
}
\details{
\itemize{
\item For AusTraits <=v3.0.2, some studies have multiple rows of data for each observation_id, so \code{trait_pivot_wider} will return four lists (value, unit, value_type, date and replicates) with the identifying columns and trait data arranged in columns.
\item For AusTraits >3.0.2, \code{trait_pivot_wider} will return a single widen tibble, note that some meta-data columns (unit, replicates, measurement_remarks, basis_of_record, basis_of_value) will be excluded to produce a useful wide tibble.
}
}
\examples{
\dontrun{
data <- austraits$traits \%>\% filter(dataset_id == "Falster_2003")
data #long format 
traits_wide <- trait_pivot_wider(data) 
traits_wide #wide format
}
}
\author{
Daniel Falster - daniel.falster@unsw.edu.au
}
