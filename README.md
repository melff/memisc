# memisc 
### Tools for Managing Survey Data and Creating Tables of Estimates and Data Summaries

[![Travis build status](https://api.travis-ci.com/melff/memisc.svg?branch=master)](https://app.travis-ci.com/github/melff/memisc) 
[![Current release on GitHub](http://img.shields.io/github/release/melff/memisc.svg)](http://github.com/melff/memisc/releases/)
<!-- [![CRAN](http://www.r-pkg.org/badges/version-last-release/memisc)](http://cran.r-project.org/package=memisc)
[![Total downloads from RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/grand-total/memisc)](http://cran.r-project.org/web/packages/memisc/index.html)
[![Total downloads from RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/memisc)](http://cran.r-project.org/web/packages/memisc/index.html) -->

<!--[![Build status](https://ci.appveyor.com/api/projects/status/iav1id61lmlh7nkb?svg=true)](https://ci.appveyor.com/project/melff/memisc) one CI is enough .. -->

One of the aims of this package is to make life easier for useRs who deal with
survey data sets. It provides an infrastructure for the management of survey
data including [value labels](https://melff.github.io/memisc/reference/labels.html), [definable missing values](https://melff.github.io/memisc/reference/value-filters.html), [recoding](https://melff.github.io/memisc/reference/recode.html) of variables,
production of [codebooks](https://melff.github.io/memisc/reference/codebook.html), and [import of (subsets of) SPSS and Stata
files](https://melff.github.io/memisc/reference/importers.html). Further, it provides functionality to produce [tables and data frames of
arbitrary descriptive statistics](https://melff.github.io/memisc/reference/genTable.html) and (almost) publication-ready [tables of
regression model estimates](https://melff.github.io/memisc/reference/mtable.html), which can be exported to [LaTeX](reference/mtable-format-latex.html) and [HTML](https://melff.github.io/memisc/reference/mtable-format-html.html).

Releases will be published on [CRAN](http://cran.r-project.org/web/packages/memisc/). (Pre-)Releases are also available from [GitHub](https://github.com/melff/memisc) in
source and binary form. To install from the sources on GitHub you can use `remotes::install_github("melff/memisc",subdir="pkg")`. The most convenient way to install the current development version of the package however is from [melff.r-universe.dev](https://melff.r-universe.dev/memisc) were you can find binaries for Windows, macOS, Linux, and webasm. Probably the easiest way of installation from R is `install.packages("memisc", repos = c("https://melff.r-universe.dev", "https://cloud.r-project.org"))`

The package has a DOI now: [10.32614/CRAN.package.memisc](https://doi.org/10.32614/CRAN.package.memisc)
