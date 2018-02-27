
<!-- README.md is generated from README.Rmd. Please edit that file -->
gateway <img src="man/figures/gatewayLogo.png" align="right" />
===============================================================

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis-CI Build Status](https://travis-ci.org/slu-openGIS/gateway.svg?branch=master)](https://travis-ci.org/slu-openGIS/gateway) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/slu-openGIS/gateway?branch=master&svg=true)](https://ci.appveyor.com/project/chris-prener/gateway) [![codecov](https://codecov.io/gh/slu-openGIS/gateway/branch/master/graph/badge.svg)](https://codecov.io/gh/slu-openGIS/gateway) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/gateway)](https://cran.r-project.org/package=gateway)

The `gateway` package contains tools for manipulating spatial data for the City of St. Louis

Installation
------------

You can install gateway from github with:

``` r
devtools::install_github("slu-openGIS/gateway")
```

Useage
------

The package current has two functions for working with St. Louis neighborhood vectors - `nhood_num()` and `nhood_str()`. It also has a function `stdName()` for standardizing City of St. Louis street names (`FIRST` becomes `1st`, for example). You can view the database that `stdName()` by spening the `stdStreets` data included with the package.

Contributor Code of Conduct
---------------------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
