
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
version of austraits from [GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("traitecoevo/austraits")
```

## Getting started

First load the library and inspect the trait data

``` r
library(austraits)
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
