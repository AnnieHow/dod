
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dod

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dod)](https://CRAN.R-project.org/package=dod)
<!-- badges: end -->

The goal of dod is to make it easier to download various types of oceanographic data from common sources such as BATS, NAOO, MEDS, BBMP.

## Installation

You can install the development version of dod from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AnnieHow/dod")
```

## Example

This is a basic example which shows you how to download ctd index from
the BBMP program in the year 2022 and download, read and plot the first
item in the index using the `oce` package.

``` r
library(dod)
#> Loading required package: rmarkdown
library(oce)
#> Loading required package: gsw
index <- dod.ctd("BBMP", 2022, index=TRUE) 
item <- index[1,"file"]
file <- dod.ctd("BBMP", 2022, item)
plot(read.ctd(file))
```

<img src="man/figures/README-example-1.png" width="100%" />
