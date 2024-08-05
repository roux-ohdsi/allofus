
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `allofus` R Package

<!-- badges: start -->

[![R-CMD-check](https://github.com/roux-ohdsi/allofus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/roux-ohdsi/allofus/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/allofus)](https://CRAN.R-project.org/package=allofus)
[![](http://cranlogs.r-pkg.org/badges/grand-total/allofus)](https://cran.r-project.org/package=allofus)
[![DOI](https://zenodo.org/badge/659848534.svg)](https://zenodo.org/doi/10.5281/zenodo.10420610)

<!-- badges: end -->

The goal of the `allofus` R package is to streamline the use of R within
the [All of Us Researcher
Workbench](https://www.researchallofus.org/data-tools/workbench/). It
has 4 primary goals:

1.  Facilitate the use of popular `tidyverse` ecosystem of R packages on
    the Researcher Workbench
2.  Help researchers more efficiently and accurately extract and
    synthesize survey data and EHR data
3.  Increase the interoperability between tools created by the
    Observational Health Data Sciences and Informatics community (OHDSI)
    for the [OMOP
    CDM](https://www.researchallofus.org/faq/what-is-omop/)) and the
    Researcher Workbench
4.  Make connecting to the database and managing files simple

*The `allofus` R package was developed by Louisa Smith and Rob Cavanaugh
at [Northeastern University](https://ohdsi.northeastern.edu) and is not
affiliated with or endorsed by the All of Us Research Program.*

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

On the new RStudio interface on the workbench, you will need to manually
specify the CRAN mirror to be able to download *any* recently updated
packages.

``` r
# specify the mirror directly
install.packages("allofus", repos = "https://cloud.r-project.org")

# OR set the mirror as an option at the top of your script
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Github development versions may requires using the remotes package
install.packages("remotes")
remotes::install_github("roux-ohdsi/allofus", repos = "https://cloud.r-project.org")
```

### Use

Read through the [getting
started](https://roux-ohdsi.github.io/allofus/vignettes/allofus.html)
vignette to learn how to use the package.

### Citation

Please cite the `allofus` package as:

<p>
Smith L, Cavanaugh R (2023). <em>allofus: Interface for ‘All of Us’
Researcher Workbench</em>.
<a href="https://doi.org/10.5281/zenodo.10420610">doi:10.5281/zenodo.10420610</a>,
<a href="https://roux-ohdsi.github.io/allofus/">https://roux-ohdsi.github.io/allofus/</a>.
</p>

or with

``` r
citation("allofus")
```

### Bugs

Please leave us comments, requests, and report bugs using the “Issues”
tab on github.
