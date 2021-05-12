
<!-- README.md is generated from README.Rmd. Please edit that file -->

# austraits.R

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/austraits.R/workflows/R-CMD-check/badge.svg)](https://github.com/traitecoevo/austraits.R/actions)
[![codecov](https://codecov.io/gh/traitecoevo/austraits.R/branch/develop/graph/badge.svg?token=JT1M0AMZ44)](https://codecov.io/gh/traitecoevo/austraits.R)
<!-- badges: end -->

austraits.R allow users to access, explore and wrangle the austraits
database via R. The package includes several general functions such as
filtering and pivoting the dataset that we expect will come in handy. In
our vignette, we have also included some tutorials that uses `tidyverse`
functions to create more specific output - see vignette(“austraits.R”)

## Installation

The package is still under development. You can install the development
version of austraits.R from [GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("traitecoevo/austraits.R")
```

## Getting started

First load the library and inspect the trait data

``` r
library(austraits.R)
summary(austraits$traits)
#>   dataset_id         taxon_name         site_name         context_name      
#>  Length:953         Length:953         Length:953         Length:953        
#>  Class :character   Class :character   Class :character   Class :character  
#>  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
#>                                                                             
#>                                                                             
#>                                                                             
#>                                                                             
#>  observation_id      trait_name           value               unit          
#>  Length:953         Length:953         Length:953         Length:953        
#>  Class :character   Class :character   Class :character   Class :character  
#>  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
#>                                                                             
#>                                                                             
#>                                                                             
#>                                                                             
#>      date                 value_type   replicates        original_name     
#>  Length:953         site_mean  :597   Length:953         Length:953        
#>  Class :character   unknown    :262   Class :character   Class :character  
#>  Mode  :character   expert_mean: 74   Mode  :character   Mode  :character  
#>                     site_max   : 20                                        
#>                     raw_value  :  0                                        
#>                     site_min   :  0                                        
#>                     (Other)    :  0
```
