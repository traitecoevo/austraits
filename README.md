
<!-- README.md is generated from README.Rmd. Please edit that file -->

# austraits <img src="inst/figures/hexlogo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/austraits/workflows/R-CMD-check/badge.svg)](https://github.com/traitecoevo/austraits/actions)
[![codecov](https://codecov.io/gh/traitecoevo/austraits/branch/master/graph/badge.svg?token=JT1M0AMZ44)](https://codecov.io/gh/traitecoevo/austraits)
[![](https://img.shields.io/badge/doi-10.1038/s41597--021--01006--6-blue.svg)](https://doi.org/10.1038/s41597-021-01006-6)
<!-- badges: end -->

`austraits` allow users to access, explore and wrangle data from the
AusTraits database in `R`. This package includes several functions such
as filtering and pivoting the dataset that we expect will come in handy.
In our vignette, we have also included some tutorials that uses our
functions as well as `tidyverse` functions to create more data outputs
and summaries - see browseVignettes(“austraits”)

### Installation

The package is not on CRAN yet and is still under active development.
You can install the development version of austraits from
[GitHub](https://github.com/traitecoevo/austraits) with:

``` r
#install.packages("remotes")
remotes::install_github("traitecoevo/austraits", build_vignettes = TRUE)

library(austraits) 
```

### Load most recent version of austraits

``` r
#Load austraits
austraits <- load_austraits()
```

### Extracting by study

``` r
#Extract a single study
zanne_09 <- extract_dataset(austraits, "Zanne_2009") 

#Extract multiple studies
# Filtering multiple studies by same lead author (e.g. Falster) and assigning it to an object
dataset_ids <- austraits$methods$dataset_id %>% unique() #All possible dataset_ids 
falster_ids <- dataset_ids[str_which(dataset_ids, "Falster")]  # Extracting dataset_ids with lead author "Falster" 

falster_studies <- extract_dataset(austraits, falster_ids)
```

### Extracting by trait

``` r
# Extract one particular trait and assigning it to an object
sla_data <- extract_trait(austraits, "specific_leaf_area")

# Extracting all leaf associated data and assigning it to an object
traits <- austraits$traits$trait_name %>% unique()  #All possible traits 
leaf_traits <- traits[str_which(traits, "leaf")] # Extracting data where "leaf" occurs in the trait_name

leaf_data <- extract_trait(austraits, leaf_traits) 
```

### Pivoting between long/wide

``` r
leaf_wide <- leaf_data$traits %>% trait_pivot_wider()
leaf_long <- leaf_wide %>% trait_pivot_longer()
```

### Joining site information and plotting site information

``` r
# Join site based information 
(austraits %>% join_sites)$traits %>% plot_site_locations()
```

<img src="man/figures/README-site-1.png" width="100%" />
