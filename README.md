
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sspLNIRT

<!-- badges:start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges:end -->

I introduce sspLNIRT: sample-size planning for item calibration with the
joint hierarchical model of response accuracy and response time. The
package can be used to estimate minimum sample sizes to each a target
item parameter precision under a specified data simulation model.

## Usage

The tool is available as R package and comprehensive ShinyApp.

### Web App

You can use the app at <https://sebastian-lortz.shinyapps.io/sspLNIRT/>.

### System Requirements

The `sspLNIRT` package was build under R Version 4.5.2 using Apple clang
version 16.0.0 (clang-1600.0.26.6) and GNU Fortran (GCC) 14.2.0. To
compile R from source, install the appropriate toolchain  
- macOS: see <https://mac.r-project.org/tools/>  
- windows: see <https://cran.r-project.org/bin/windows/Rtools/>

### Installation

You can install the latest version of the R package `sspLNIRT` like so:

``` r
# install devtools if needed
if (!requireNamespace("devtools")) {install.packages("devtools")}

# install from GitHub
devtools::install_github("sebastian-lortz/sspLNIRT")
```

### Run

You can launch the ShinyApp locally by running:

``` r
discourse::run_app()
```

## Citation

Please cite `sspLNIRT` if you use it. To cite the software, use:

Lortz S (2026). *sspLNIRT: Sample Size Planning for Item Calibration
using the Joint Hierarchical Model*. R package version 0.0.0.9000,
<https://github.com/sebastian-lortz/sspLNIRT>.

Or copy the reference information to your BibTeX file:

``` bibtex
@Manual{,
    title = {sspLNIRT: Sample Size Planning for Item Calibration using the Joint Hierarchical Model},
    author = {Sebastian A. J. Lortz},
    year = {2026},
    note = {R package version 0.0.0.9000},
    url = {https://github.com/sebastian-lortz/sspLNIRT},
  }
```

## Code of Conduct

I am open to feedback and new ideas. Please mind the Contributor Code of
Conduct.

## About

You are reading the doc about version: 0.0.0.9000

This README has been compiled on 2026-02-10 23:50:31.
