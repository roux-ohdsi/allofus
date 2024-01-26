
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `allofus` R Package

The goal of the `allofus` R package is to streamline the use of R within
the [All of Us Researcher
Workbench](https://www.researchallofus.org/data-tools/workbench/).

*The `allofus` R package is a not affiliated with or endorsed by the All
of Us Research Program.*

The `allofus` package helps researchers query the database and build
transparent and reproducible analysed pipeline. The package allows
researchers to easily use other database packages, such as `dbplyr` and
`DBI`, with the All of Us database. The package also includes functions
to help researchers manage storage in their workspace and bucket, as
well as take advantage of tools developed for [OMOP
CDM](https://www.researchallofus.org/faq/what-is-omop/) data.

<!-- badges: start -->

[![R-CMD-check](https://github.com/roux-ohdsi/allofus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/roux-ohdsi/allofus/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/allofus)](https://CRAN.R-project.org/package=allofus)
[![](http://cranlogs.r-pkg.org/badges/grand-total/allofus)](https://cran.r-project.org/package=allofus)
[![DOI](https://zenodo.org/badge/659848534.svg)](https://zenodo.org/doi/10.5281/zenodo.10420610)
<!-- badges: end -->

### Installation

Install the released version of `allofus` from CRAN:

``` r
install.packages("allofus")
```

Install the development version from Github:

``` r
install.packages("pak")
pak::pak("roux-ohdsi/allofus")
```

### Use

Read through the [getting
started](https://roux-ohdsi.github.io/allofus/articles/allofus.html)
vignette to learn how to use the package.

### Citation

Please cite the `allofus` package as:
<p>
Smith L, Cavanaugh R (2023). <em>allofus: Interface for ‘All of Us’
Researcher Workbench</em>. R package version 1.0.0,
<a href="https://CRAN.R-project.org/package=allofus">https://CRAN.R-project.org/package=allofus</a>.
</p>

or with

``` r
citation("allofus")
```

### Bugs

Please leave us comments, requests, and report bugs using the “Issues”
tab on github located here:
<https://github.com/roux-ohdsi/allofus/issues>.
