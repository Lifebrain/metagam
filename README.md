
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metagam

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/lifebrain/metagam.svg?branch=master)](https://travis-ci.org/lifebrain/metagam)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Lifebrain/metagam?branch=master&svg=true)](https://ci.appveyor.com/project/Lifebrain/metagam)
[![Codecov test
coverage](https://codecov.io/gh/Lifebrain/metagam/branch/master/graph/badge.svg)](https://codecov.io/gh/Lifebrain/metagam?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/metagam)](https://CRAN.R-project.org/package=metagam)
<!-- badges: end -->

## Overview

metagam is an R-package for flexible meta-analysis of generalized
additive models (GAMs). Its main application is cases in which raw data
are located in multiple locations, and cannot be shared due to ethical
or regulatory restrictions. metagam provides functions for removing all
individual participant data from from GAMs fitted separately at each
location, such that the resulting object can be shared to a central
location. Next, metagam provides functions for meta-analysing these
fitted GAMs using pointwise meta-analysis, as well as plotting and
summary methods for analyzing the meta-analytic fits.

This package is under development, so changes to the interface can be
expected. Suggestions for improvements and bug reports are warmly
welcome, either by filing an
[Issue](https://github.com/lifebrain/metagam/issues) or opening a [Pull
Request](https://github.com/lifebrain/metagam/pulls).

## Installation

Install the current development version of `metagam` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("lifebrain/metagam")
```

## Application Example

``` r
library(metagam)
```

Simulate three datasets and fit a GAM to each of them. Then use
`strip_rawdata()` from metagam to remove individual participant data.

``` r
## Simulate using mgcv::gamSim
datasets <- lapply(1:3, function(x) gamSim(verbose = FALSE))
## Fit a model to each dataset
models <- lapply(datasets, function(dat){
  ## Full gam with mgcv
  full_model <- gam(y ~ s(x2, bs = "cr"), data = dat)
  ## Strip rawdata
  strip_rawdata(full_model)
})
```

`models` now is a list containing three GAMs without individual
participant data. We can then meta-analyze them using `metagam()`.

``` r
meta_analysis <- metagam(models)
summary(meta_analysis)
#> Meta-analysis of GAMs from 3 cohorts, using method FE.
#> 
#> Smooth terms analyzed: s(x2) 
#> 
#> Meta-analytic p-values of smooth terms:
#> 
#> Test                    s(x2)     
#> ----------------------  ----------
#> Stouffer's sum of z     2.547e-46 
#> Edgington's sum of p    6.158e-48 
#> Wilkinson's maximum p   1.000e-48 
#> Wilkinson's minimum p   3.000e-16 
#> logit p method          1.628e-19 
#> Fisher's sum of logs    6.219e-45
```

For further documentation and vignettes, please visit the [package
website](https://lifebrain.github.io/metagam/).
