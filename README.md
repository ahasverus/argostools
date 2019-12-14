
<!-- README.md is generated from README.Rmd. Please edit that file -->

# argostools <img src="inst/figures/argostools.png" height="120" align="right"/>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/ahasverus/argostools.svg?branch=master)](https://travis-ci.org/ahasverus/argostools)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ahasverus/argostools?branch=master&svg=true)](https://ci.appveyor.com/project/ahasverus/argostools)
[![Project Status:
Stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/37935776.svg)](https://zenodo.org/badge/latestdoi/37935776)
<!-- badges: end -->

This package provides functions to import (xls(x), txt, csv and diag
extensions) and format Argos data (coordinates and date fields). Some
filters are also available (spatial, temporal and speed-based filters)
to clean up locations. After compilation, data can be passed under an
SQL structure using several functions (database creation, relation
update, extraction queries, etc.)

## Installation

You can install the dev version of argostools from
[GitHub](https://github.com/ahasverus/argostools) with:

``` r
devtools::install_github("ahasverus/argostools", build_vignettes = TRUE)
```

## Getting started

``` r
library(argostools)
ls("package:argostools")
```

Main functions to be used in order:

  - `pg_extract_locs()`
  - `temporal_buffer()`
  - `spatial_buffer()`
  - `speed_filter()`

Enjoy\!
