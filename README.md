
<!-- README.md is generated from README.Rmd. Please edit that file -->

# austraits

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/austraits/workflows/R-CMD-check/badge.svg)](https://github.com/traitecoevo/austraits/actions)
[![codecov](https://codecov.io/gh/traitecoevo/austraits/branch/develop/graph/badge.svg?token=JT1M0AMZ44)](https://codecov.io/gh/traitecoevo/austraits)
<!-- badges: end -->

austraits allow users to access, explore and wrangle the austraits
database via R. The package includes several general functions such as
filtering and pivoting the dataset that we expect will come in handy. In
our vignette, we have also included some tutorials that uses `tidyverse`
functions to create more specific output - see vignette(“austraits”)

## Installation

The package is still under development. You can install the development
version of austraits from
[GitHub](https://github.com/traitecoevo/austraits) with:

``` r
install.packages("remotes")
remotes::install_github("traitecoevo/austraits")
```

## Getting started

First load the library and inspect the trait data

``` r
library(austraits)

austraits$traits %>% head()
#>     dataset_id        taxon_name                                    site_name
#> 1 Falster_2003 Acacia myrtifolia Ku-ring-gai Chase National Park low nutrient
#> 2 Falster_2003 Acacia myrtifolia Ku-ring-gai Chase National Park low nutrient
#> 3 Falster_2003 Acacia myrtifolia Ku-ring-gai Chase National Park low nutrient
#> 4 Falster_2003 Acacia suaveolens Ku-ring-gai Chase National Park low nutrient
#> 5 Falster_2003 Acacia suaveolens Ku-ring-gai Chase National Park low nutrient
#> 6 Falster_2003 Acacia suaveolens Ku-ring-gai Chase National Park low nutrient
#>   context_name  observation_id        trait_name  value    unit date
#> 1         <NA> Falster_2003_01        leaf_angle   66.1 degrees <NA>
#> 2         <NA> Falster_2003_01         leaf_area    319     mm2 <NA>
#> 3         <NA> Falster_2003_01 leaf_compoundness simple    <NA> <NA>
#> 4         <NA> Falster_2003_02        leaf_angle   71.7 degrees <NA>
#> 5         <NA> Falster_2003_02         leaf_area    562     mm2 <NA>
#> 6         <NA> Falster_2003_02 leaf_compoundness simple    <NA> <NA>
#>    value_type replicates     original_name
#> 1   site_mean          3 Acacia myrtifolia
#> 2   site_mean          3 Acacia myrtifolia
#> 3 expert_mean       <NA> Acacia myrtifolia
#> 4   site_mean          3 Acacia suaveolens
#> 5   site_mean          3 Acacia suaveolens
#> 6 expert_mean       <NA> Acacia suaveolens
```
