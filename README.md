
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metagam <img src="man/figures/logo.png" height=200 align="right"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/metagam)](https://CRAN.R-project.org/package=metagam)
[![R-CMD-check](https://github.com/Lifebrain/metagam/workflows/R-CMD-check/badge.svg)](https://github.com/Lifebrain/metagam/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/Lifebrain/metagam/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Lifebrain/metagam?branch=master)
[![R-CMD-check](https://github.com/Lifebrain/metagam/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Lifebrain/metagam/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

metagam is an R-package for meta-analysis of generalized additive models
(GAMs). Its main application is cases in which raw data are located in
multiple locations, and cannot be shared due to ethical or regulatory
restrictions. metagam provides functions for removing all individual
participant data from from GAMs fitted separately at each location, such
that the resulting object can be shared to a central location. Next,
metagam provides functions for meta-analysing these fitted GAMs using
pointwise meta-analysis, as well as plotting and summary methods for
analyzing the meta-analytic fits. The methods implemented are described
in Sorensen et al. (2021), extending upon previous works by Schwartz and
Zanobetti (2000) and Crippa, Thomas, and Orsini (2018).

Currently, GAMs objects created with the following functions are
supported:

- From package [mgcv](https://cran.r-project.org/package=mgcv): `bam()`,
  `gam()` and `gamm()`.
- From package [gamm4](https://cran.r-project.org/package=gamm4):
  `gamm4()`.

This package is under development, so changes to the interface can be
expected. Suggestions for improvements and bug reports are warmly
welcome, either by filing an
[Issue](https://github.com/lifebrain/metagam/issues) or opening a [Pull
Request](https://github.com/lifebrain/metagam/pulls).

## Installation

Install the current release of metagam from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("metagam")
```

Install the current development version of `metagam` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("lifebrain/metagam")
```

## Application Example

``` r
library("metagam")
library("mgcv")
#> Loading required package: nlme
#> This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.
```

Simulate three datasets and fit a GAM to each of them. Then use
`strip_rawdata()` from metagam to remove individual participant data.

``` r
## Set seed for reproducible random numbers
set.seed(8562957)
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
#> Meta-analysis of GAMs from  cohorts, using method FE.
#> 
#> Smooth terms analyzed: s(x2).
```

For further documentation and vignettes, please visit the [package
website](https://lifebrain.github.io/metagam/).

## Code of Conduct

Please note that the metagam project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct.html).
By contributing to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Crippa2018" class="csl-entry">

Crippa, Alessio, Ilias Thomas, and Nicola Orsini. 2018. “A Pointwise
Approach to Dose-Response Meta-Analysis of Aggregated Data.”
*International Journal of Statistics in Medical Research* 7 (May):
25–32. <https://doi.org/10.6000/1929-6029.2018.07.02.1>.

</div>

<div id="ref-Schwarz2000" class="csl-entry">

Schwartz, Joel, and Antonella Zanobetti. 2000. “Using Meta-Smoothing to
Estimate Dose-Response Trends Across Multiple Studies, with Application
to Air Pollution and Daily Death.” *Epidemiology* 11 (6): 666–72.

</div>

<div id="ref-Sorensen2021" class="csl-entry">

Sorensen, Oystein, Andreas M. Brandmaier, Didac Macia, Klaus Ebmeier,
Paolo Ghisletta, Rogier A. Kievit, Athanasia M. Mowinckel, Kristine B.
Walhovd, Rene Westerhausen, and Anders Fjell. 2021. “Meta-Analysis of
Generalized Additive Models in Neuroimaging Studies.” *NeuroImage* 224
(January): 117416. <https://doi.org/10.1016/j.neuroimage.2020.117416>.

</div>

</div>
