---
editor_options: 
  markdown: 
    wrap: 72
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# austraits <img src="man/figures/austraits_hex.png" align="right" width="120"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/austraits/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/traitecoevo/austraits/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/gh/traitecoevo/austraits/branch/master/graph/badge.svg?token=JT1M0AMZ44)](https://codecov.io/gh/traitecoevo/austraits)
[![](https://img.shields.io/badge/doi-10.1038/s41597--021--01006--6-blue.svg)](https://doi.org/10.1038/s41597-021-01006-6)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

`austraits` allow users to access, explore and wrangle data from the
AusTraits database in `R`. This package includes several functions such
as filtering and pivoting the dataset that we expect will come in handy.

**For R users**, access and manipulation of the data is assisted with
the `austraits` package

**For Python or other users**, the [Zenodo
download](https://zenodo.org/record/5112001#collapseTwo) includes a .zip
file containing all the data in plain text (.csv files) and associated
meta-data

### Installation

The package is not on CRAN yet and is still under active development.
For the current stable release of `austraits`, which has full
capabilities of the functions used in vignettes (e.g. plotting
functions), use:

``` r
#install.packages("remotes")
remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits) 
```

Otherwise, for a lightweight installation where dependencies for
plotting and the vignettes will not be installed, use:

``` r
remotes::install_github("traitecoevo/austraits", upgrade = "ask")
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

If you prefer to view the vignettes in R, we recommend installing the
package with `build_vignettes = TRUE`.

``` r
remotes::install_github("traitecoevo/austraits", 
                        dependencies = TRUE, upgrade = "ask", 
                        build_vignettes = TRUE)

vignette("austraits")
```

### Show us some support 💚

Please consider citing `austraits`, we would super appreciate it!

``` r
citation("austraits")
#> 
#> To cite austraits in publications use:
#> 
#>   Falster, D., Gallagher, R., Wenk, E.H. et al. AusTraits, a curated
#>   plant trait database for the Australian flora. Sci Data 8, 254
#>   (2021). https://doi.org/10.1038/s41597-021-01006-6
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {AusTraits, a curated plant trait database for the Australian flora},
#>     author = {Daniel Falster and Rachael Gallagher and Elizabeth Wenk et al.},
#>     journal = {Scientific Data},
#>     year = {2021},
#>     volume = {8},
#>     number = {1},
#>     pages = {254 - 274},
#>     url = {https://doi.org/10.1038/s41597-021-01006-6},
#>   }
```

### Behind the scenes 🔧

Check out
[austraits.build](http://traitecoevo.github.io/austraits.build/), if you
are interested in how AusTraits the database is managed and created

### Find a bug? 🐛

Thank you for finding it! Head over to the GitHub Issues tab and let us
know about it! We will try to get to it as soon as we can!
