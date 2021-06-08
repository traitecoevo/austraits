<!-- README.md is generated from README.Rmd. Please edit that file -->

austraits
=========

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/austraits/workflows/R-CMD-check/badge.svg)](https://github.com/traitecoevo/austraits/actions)
[![codecov](https://codecov.io/gh/traitecoevo/austraits/branch/develop/graph/badge.svg?token=JT1M0AMZ44)](https://codecov.io/gh/traitecoevo/austraits)
<!-- badges: end -->

austraits allow users to access, explore and wrangle the austraits
database via R. The package includes several general functions such as
filtering and pivoting the dataset that we expect will come in handy. In
our vignette, we have also included some tutorials that uses `tidyverse`
functions to create more specific output - see vignette(“austraits”)

Installation
------------

The package is still under development. You can install the development
version of austraits from
[GitHub](https://github.com/traitecoevo/austraits) with:

    install.packages("remotes")
    #> 
    #> The downloaded binary packages are in
    #>  /var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//RtmpKSh2jt/downloaded_packages
    remotes::install_github("traitecoevo/austraits")
    #> colorspace   (NA -> 2.0-1) [CRAN]
    #> viridisLite  (NA -> 0.4.0) [CRAN]
    #> RColorBrewer (NA -> 1.1-2) [CRAN]
    #> R6           (NA -> 2.5.0) [CRAN]
    #> munsell      (NA -> 0.5.0) [CRAN]
    #> lifecycle    (NA -> 1.0.0) [CRAN]
    #> labeling     (NA -> 0.4.2) [CRAN]
    #> farver       (NA -> 2.1.0) [CRAN]
    #> utf8         (NA -> 1.2.1) [CRAN]
    #> crayon       (NA -> 1.4.1) [CRAN]
    #> cli          (NA -> 2.5.0) [CRAN]
    #> vctrs        (NA -> 0.3.8) [CRAN]
    #> pkgconfig    (NA -> 2.0.3) [CRAN]
    #> pillar       (NA -> 1.6.1) [CRAN]
    #> fansi        (NA -> 0.5.0) [CRAN]
    #> ellipsis     (NA -> 0.3.2) [CRAN]
    #> withr        (NA -> 2.4.2) [CRAN]
    #> tibble       (NA -> 3.1.2) [CRAN]
    #> scales       (NA -> 1.1.1) [CRAN]
    #> isoband      (NA -> 0.2.4) [CRAN]
    #> gtable       (NA -> 0.3.0) [CRAN]
    #> purrr        (NA -> 0.3.4) [CRAN]
    #> tidyselect   (NA -> 1.1.1) [CRAN]
    #> generics     (NA -> 0.1.0) [CRAN]
    #> vipor        (NA -> 0.4.5) [CRAN]
    #> beeswarm     (NA -> 0.4.0) [CRAN]
    #> ggplot2      (NA -> 3.3.3) [CRAN]
    #> cpp11        (NA -> 0.2.7) [CRAN]
    #> dplyr        (NA -> 1.0.6) [CRAN]
    #> forcats      (NA -> 0.5.1) [CRAN]
    #> gridExtra    (NA -> 2.3  ) [CRAN]
    #> ggbeeswarm   (NA -> 0.6.0) [CRAN]
    #> assertthat   (NA -> 0.2.1) [CRAN]
    #> tidyr        (NA -> 1.1.3) [CRAN]
    #> 
    #> The downloaded binary packages are in
    #>  /var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//RtmpKSh2jt/downloaded_packages
    #> * checking for file ‘/private/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T/RtmpKSh2jt/remotes11064884cc28/traitecoevo-austraits-1ef1d74/DESCRIPTION’ ... OK
    #> * preparing ‘austraits’:
    #> * checking DESCRIPTION meta-information ... OK
    #> * checking for LF line-endings in source and make files and shell scripts
    #> * checking for empty or unneeded directories
    #> * building ‘austraits_0.0.0.9000.tar.gz’

Getting started
---------------

First load the library and inspect the trait data

    library(austraits)

    austraits$traits %>% head()
    #> # A tibble: 6 x 12
    #>   dataset_id  taxon_name site_name  context_name observation_id trait_name value
    #>   <chr>       <chr>      <chr>      <chr>        <chr>          <chr>      <chr>
    #> 1 Falster_20… Acacia my… Ku-ring-g… <NA>         Falster_2003_… leaf_angle 66.1 
    #> 2 Falster_20… Acacia my… Ku-ring-g… <NA>         Falster_2003_… leaf_area  319  
    #> 3 Falster_20… Acacia my… Ku-ring-g… <NA>         Falster_2003_… leaf_comp… simp…
    #> 4 Falster_20… Acacia su… Ku-ring-g… <NA>         Falster_2003_… leaf_angle 71.7 
    #> 5 Falster_20… Acacia su… Ku-ring-g… <NA>         Falster_2003_… leaf_area  562  
    #> 6 Falster_20… Acacia su… Ku-ring-g… <NA>         Falster_2003_… leaf_comp… simp…
    #> # … with 5 more variables: unit <chr>, date <chr>, value_type <fct>,
    #> #   replicates <chr>, original_name <chr>
