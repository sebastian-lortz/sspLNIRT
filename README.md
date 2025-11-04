
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{sspLNIRT}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/sebastian-lortz/sspLNIRT/graph/badge.svg)](https://app.codecov.io/gh/sebastian-lortz/sspLNIRT)
<!-- badges: end -->

## Installation

You can install the development version of `{sspLNIRT}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
sspLNIRT::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-11-04 13:10:14 CET"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> Writing 'NAMESPACE'
#> ℹ Loading sspLNIRT
#> ── R CMD check results ──────────────────────────────── sspLNIRT 0.0.0.9000 ────
#> Duration: 8.8s
#> 
#> ❯ checking package subdirectories ... NOTE
#>   Problems with news in ‘NEWS.md’:
#>   No news entries found.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> sspLNIRT Coverage: 94.24%
#> R/run_app.R: 0.00%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
#> R/mod_ssp_lnirt.R: 100.00%
```
