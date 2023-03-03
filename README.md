
<!-- README.md is generated from README.Rmd. Please edit that file -->

# goal

- [Full documentation](https://dimitrisk.github.io/goal/index.html)

## Description

<img src="man/figures/logo.png" align="right" />

This is an R library for the **analysis** of geographical data. It
includes 4 group of functions:

1)  functions for the analysis of networks (`net`).

2)  functions for spatial-geometry conversions (`geo`).

3)  functions for geographical visualization and interpretation of
    statistics (`vis`).

4)  functions for analysis of UAV missions and flightpaths (`uav`).

## Install

    library(devtools)
    install_github("dimitrisk/goal")

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![GitHub all
releases](https://img.shields.io/github/downloads/dimitrisk/goal/total)
![GitHub issues](https://img.shields.io/github/issues/dimitrisk/goal)
![GitHub pull
requests](https://img.shields.io/github/issues-pr/dimitrisk/goal)
![GitHub Repo
stars](https://img.shields.io/github/stars/dimitrisk/goal?style=social)
![GitHub R package
version](https://img.shields.io/github/r-package/v/dimitrisk/goal)
![GitHub release (latest by
date)](https://img.shields.io/github/v/release/dimitrisk/goal) ![GitHub
last commit](https://img.shields.io/github/last-commit/dimitrisk/goal)

# goal

<!-- badges: start -->
<!-- badges: end -->

The goal of goal is to …

## Installation

You can install goal from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dimitrisk/goal")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(goal)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
