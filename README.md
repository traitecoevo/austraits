
<!-- README.md is generated from README.Rmd. Please edit that file -->

# austraits <img src="inst/figures/hexlogo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/austraits/workflows/R-CMD-check/badge.svg)](https://github.com/traitecoevo/austraits/actions)
[![codecov](https://codecov.io/gh/traitecoevo/austraits/branch/master/graph/badge.svg?token=JT1M0AMZ44)](https://codecov.io/gh/traitecoevo/austraits)
[![](https://img.shields.io/badge/doi-10.1038/s41597--021--01006--6-blue.svg)](https://doi.org/10.1038/s41597-021-01006-6)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

`austraits` allow users to access, explore and wrangle data from the
AusTraits database in `R`. This package includes several functions such
as filtering and pivoting the dataset that we expect will come in handy.

### Getting started

The package is not on CRAN yet and is still under active development.
You can install the stable version of `austraits` from
[GitHub](https://github.com/traitecoevo/austraits):

``` r
#install.packages("remotes")
remotes::install_github("traitecoevo/austraits", build_vignettes = TRUE)

library(austraits) 
```

### Take a good look at our vignettes! 👀

In our website, we have also included information about the [structure
of the
database](https://traitecoevo.github.io/austraits/articles/structure.html),
[definitions of the
traits](https://traitecoevo.github.io/austraits/articles/dictionary.html)
and some
[tutorials](https://traitecoevo.github.io/austraits/articles/austraits.html)
that uses our functions as well as some `tidyverse` functions to create
some commonly used data output formats. We highly recommend starting
here before jumping into the database!

``` r
vignette("austraits")
```

### Show us some support 💚

Please consider citing `austraits`, we would super appreciate it!

``` r
citation("austraits")
#> 
#> To cite package 'austraits' in publications use:
#> 
#>   Daniel Falster, Fonti Kar and Dony Indiarto (2021). austraits:
#>   Helpful functions to access, summarise and wrangle austraits data. R
#>   package version 0.0.0.9000. https://traitecoevo.github.io/austraits/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {austraits: Helpful functions to access, summarise and wrangle austraits
#> data},
#>     author = {Daniel Falster and Fonti Kar and Dony Indiarto},
#>     year = {2021},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://traitecoevo.github.io/austraits/},
#>   }
```

### Find a bug? 🐛

Thank you for finding it! Head over to the `Issues` tab and let us know
about it! We will try to get to it as soon as we can!
