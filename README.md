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
data including [value labels](reference/labels.html), [definable missing values](reference/value-filters.html), [recoding](reference/recode.html) of variables,
production of [codebooks](reference/codebook.html), and [import of (subsets of) SPSS and Stata
files](reference/importers.html). Further, it provides functionality to produce [tables and data frames of
arbitrary descriptive statistics](reference/genTable.html) and (almost) publication-ready [tables of
regression model estimates](reference/mtable.html), which can be exported to [LaTeX](reference/mtable-format-latex.html) and [HTML](reference/mtable-format-html.html).

Releases will be published on [CRAN](http://cran.r-project.org/web/packages/memisc/). Pre-releases will be available on [GitHub](https://github.com/melff/memisc) in
source and binary form. To install from the sources on GitHub you can use
`remotes::install_github("melff/memisc",subdir="pkg")`.
