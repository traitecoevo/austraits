% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_trait_values.R
\name{summarise_trait_means}
\alias{summarise_trait_means}
\title{Compute mean trait values for studies that have multiple observations for a given trait}
\usage{
summarise_trait_means(trait_data)
}
\arguments{
\item{trait_data}{trait table for austraits list}
}
\value{
A reduced trait table, mean values are flagged with the suffix '_summarised' in value_type
}
\description{
Compute mean trait values for studies that have multiple observations for a given trait
}
\examples{
\dontrun{
data <- austraits$traits \%>\% filter(dataset_id == "Falster_2003")
data \%>\% summarise_trait_means()
}
}
