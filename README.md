<!-- README.md is generated from README.Rmd. Please edit that file -->

austraits.R
===========

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/austraits.R/workflows/R-CMD-check/badge.svg)](https://github.com/traitecoevo/austraits.R/actions)
[![codecov](https://codecov.io/gh/traitecoevo/austraits.R/branch/develop/graph/badge.svg?token=JT1M0AMZ44)](https://codecov.io/gh/traitecoevo/austraits.R)
<!-- badges: end -->

austraits.R allow users to access, explore and wrangle the austraits
database via R. The package includes several general functions such as
filtering and pivoting the dataset that we expect will come in handy. In
our vignette, we have also included some tutorials that uses `tidyverse`
functions to create more specific output - see vignette(“austraits.R”)

Installation
------------

The package is still under development. You can install the development
version of austraits.R from [GitHub](https://github.com/) with:

    #> 
    #> The downloaded binary packages are in
    #>  /var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//Rtmp0cDV8A/downloaded_packages
    #> purrr      (NA -> 0.3.4) [CRAN]
    #> pkgconfig  (NA -> 2.0.3) [CRAN]
    #> utf8       (NA -> 1.2.1) [CRAN]
    #> fansi      (NA -> 0.4.2) [CRAN]
    #> crayon     (NA -> 1.4.1) [CRAN]
    #> cli        (NA -> 2.5.0) [CRAN]
    #> pillar     (NA -> 1.6.0) [CRAN]
    #> vctrs      (NA -> 0.3.8) [CRAN]
    #> tidyselect (NA -> 1.1.1) [CRAN]
    #> tibble     (NA -> 3.1.1) [CRAN]
    #> R6         (NA -> 2.5.0) [CRAN]
    #> lifecycle  (NA -> 1.0.0) [CRAN]
    #> generics   (NA -> 0.1.0) [CRAN]
    #> ellipsis   (NA -> 0.3.2) [CRAN]
    #> cpp11      (NA -> 0.2.7) [CRAN]
    #> dplyr      (NA -> 1.0.6) [CRAN]
    #> tidyr      (NA -> 1.1.3) [CRAN]
    #> 
    #>   There is a binary version available but the source version is later:
    #>       binary source needs_compilation
    #> dplyr  1.0.5  1.0.6              TRUE
    #> 
    #> 
    #> The downloaded binary packages are in
    #>  /var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//Rtmp0cDV8A/downloaded_packages
    #> * checking for file ‘/private/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T/Rtmp0cDV8A/remotes7b516a963a3/traitecoevo-austraits.R-b9f82de/DESCRIPTION’ ... OK
    #> * preparing ‘austraits.R’:
    #> * checking DESCRIPTION meta-information ... OK
    #> * checking for LF line-endings in source and make files and shell scripts
    #> * checking for empty or unneeded directories
    #> * building ‘austraits.R_0.0.0.9000.tar.gz’

Getting started
---------------

First load the library and inspect the trait data

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
