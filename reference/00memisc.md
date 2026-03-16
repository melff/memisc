# Introduction to the 'memisc' Package

This package collects an assortment of tools that are intended to make
work with `R` easier for the author of this package and are submitted to
the public in the hope that they will be also be useful to others.

The tools in this package can be grouped into four major categories:

- Data preparation and management

- Data analysis

- Presentation of analysis results

- Programming

## Data preparation and management

### Survey Items

`memisc` provides facilities to work with what users from other packages
like SPSS, SAS, or Stata know as \`variable labels', \`value labels' and
\`user-defined missing values'. In the context of this package these
aspects of the data are represented by the `"description"`, `"labels"`,
and `"missing.values"` attributes of a data vector. These facilities are
useful, for example, if you work with survey data that contain coded
items like vote intention that may have the following structure:

Question: “If there was a parliamentary election next tuesday, which
party would you vote for?”

|     |                                             |
|-----|---------------------------------------------|
| 1   | Conservative Party                          |
| 2   | Labour Party                                |
| 3   | Liberal Democrat Party                      |
| 4   | Scottish Nation Party                       |
| 5   | Plaid Cymru                                 |
| 6   | Green Party                                 |
| 7   | British National Party                      |
| 8   | Other party                                 |
| 96  | Not allowed to vote                         |
| 97  | Would not vote                              |
| 98  | Would vote, do not know yet for which party |
| 99  | No answer                                   |

A statistical package like SPSS allows to attach labels like
\`Conservative Party', \`Labour Party', etc. to the codes 1,2,3, etc.
and to mark mark the codes 96, 97, 98, 99 as \`missing' and thus to
exclude these variables from statistical analyses. `memisc` provides
similar facilities. Labels can be attached to codes by calls like
[`labels`](https://melff.github.io/memisc/reference/labels.md)`(x) <- something`
and expendanded by calls like
[`labels`](https://melff.github.io/memisc/reference/labels.md)`(x) <- `[`labels`](https://melff.github.io/memisc/reference/labels.md)`(x) + something`,
codes can be marked as \`missing' by calls like
[`missing.values`](https://melff.github.io/memisc/reference/value-filters.md)`(x) <- something`
and
[`missing.values`](https://melff.github.io/memisc/reference/value-filters.md)`(x) <- `[`missing.values`](https://melff.github.io/memisc/reference/value-filters.md)`(x) + something`.

`memisc` defines a class called "data.set", which is similar to the
class "data.frame". The main difference is that it is especially geared
toward containing survey item data. Transformations of and within
"data.set" objects retain the information about value labels, missing
values etc. Using `as.data.frame` sets the data up for *R*'s statistical
functions, but doing this explicitely is seldom necessary. See
[`data.set`](https://melff.github.io/memisc/reference/dataSets.md).

### More Convenient Import of External Data

Survey data sets are often relative large and contain up to a few
thousand variables. For specific analyses one needs however only a
relatively small subset of these variables. Although modern computers
have enough RAM to load such data sets completely into an R session,
this is not very efficient having to drop most of the variables after
loading. Also, loading such a large data set completely can be
time-consuming, because R has to allocate space for each of the many
variables. Loading just the subset of variables really needed for an
analysis is more efficient and convenient - it tends to be much quicker.
Thus this package provides facilities to load such subsets of variables,
without the need to load a complete data set. Further, the loading of
data from SPSS files is organized in such a way that all informations
about variable labels, value labels, and user-defined missing values are
retained. This is made possible by the definition of
[`importer`](https://melff.github.io/memisc/reference/importers.md)
objects, for which a [`subset`](https://rdrr.io/r/base/subset.html)
method exists.
[`importer`](https://melff.github.io/memisc/reference/importers.md)
objects contain only the information about the variables in the external
data set but not the data. The data itself is loaded into memory when
the functions `subset` or
[`as.data.set`](https://melff.github.io/memisc/reference/dataSets.md)
are used.

### Recoding

`memisc` also contains facilities for recoding survey items. Simple
recodings, for example collapsing answer categories, can be done using
the function
[`recode`](https://melff.github.io/memisc/reference/recode.md). More
complex recodings, for example the construction of indices from multiple
items, and complex case distinctions, can be done using the function
[`cases`](https://melff.github.io/memisc/reference/cases.md). This
function may also be useful for programming, in so far as it is a
generalization of [`ifelse`](https://rdrr.io/r/base/ifelse.html).

### Code Books

There is a function
[`codebook`](https://melff.github.io/memisc/reference/codebook.md) which
produces a code book of an external data set or an internal "data.set"
object. A codebook contains in a conveniently formatted way concise
information about every variable in a data set, such as which value
labels and missing values are defined and some univariate statistics.

An extended example of all these facilities is contained in the vignette
"anes48", and in `demo(anes48)`

## Data Analysis

### Tables and Data Frames of Descriptive Statistics

[`genTable`](https://melff.github.io/memisc/reference/genTable.md) is a
generalization of [`xtabs`](https://rdrr.io/r/stats/xtabs.html): Instead
of counts, also descriptive statistics like means or variances can be
reported conditional on levels of factors. Also conditional percentages
of a factor can be obtained using this function.

In addition an `Aggregate` function is provided, which has the same
syntax as `genTable`, but gives a data frame of descriptive statistics
instead of a `table` object.

### Per-Subset Analysis

[`By`](https://melff.github.io/memisc/reference/By.md) is a variant of
the standard function [`by`](https://rdrr.io/r/base/by.html):
Conditioning factors are specified by a formula and are obtained from
the data frame the subsets of which are to be analysed. Therefore there
is no need to [`attach`](https://rdrr.io/r/base/attach.html) the data
frame or to use the dollar operator.

## Presentation of Results of Statistical Analysis

### Publication-Ready Tables of Coefficients

Journals of the Political and Social Sciences usually require that
estimates of regression models are presented in the following form:

        ==================================================
                        Model 1     Model 2     Model 3
        --------------------------------------------------
        Coefficients
        (Intercept)     30.628***    6.360***   28.566***
                        (7.409)     (1.252)     (7.355)
        pop15           -0.471**                -0.461**
                        (0.147)                 (0.145)
        pop75           -1.934                  -1.691
                        (1.041)                 (1.084)
        dpi                          0.001      -0.000
                                    (0.001)     (0.001)
        ddpi                         0.529*      0.410*
                                    (0.210)     (0.196)
        --------------------------------------------------
        Summaries
        R-squared         0.262       0.162       0.338
        adj. R-squared    0.230       0.126       0.280
        N                50          50          50
        ==================================================

Such tables of coefficient estimates can be produced by
[`mtable`](https://melff.github.io/memisc/reference/mtable.md). To see
some of the possibilities of this function, use `example(mtable)`.

### LaTeX Representation of R Objects

Output produced by
[`mtable`](https://melff.github.io/memisc/reference/mtable.md) can be
transformed into LaTeX tables by an appropriate method of the generic
function [`toLatex`](https://rdrr.io/r/utils/toLatex.html) which is
defined in the package `utils`. In addition, `memisc` defines `toLatex`
methods for matrices and [`ftable`](https://rdrr.io/r/stats/ftable.html)
objects. Note that results produced by
[`genTable`](https://melff.github.io/memisc/reference/genTable.md) can
be coerced into [`ftable`](https://rdrr.io/r/stats/ftable.html) objects.
Also, a default method for the `toLatex` function is defined which
coerces its argument to a matrix and applies the matrix method of
`toLatex`.

## Programming

### Looping over Variables

Sometimes users want to contruct loops that run over variables rather
than values. For example, if one wants to set the missing values of a
battery of items. For this purpose, the package contains the function
[`foreach`](https://melff.github.io/memisc/reference/Foreach.md). To set
8 and 9 as missing values for the items `knowledge1`, `knowledge2`,
`knowledge3`, one can use

        foreach(x=c(knowledge1,knowledge2,knowledge3),
            missing.values(x) <- 8:9)
      

### Changing Names of Objects and Labels of Factors

`R` already makes it possible to change the names of an object.
Substituting the [`names`](https://rdrr.io/r/base/names.html) or
[`dimnames`](https://rdrr.io/r/base/dimnames.html) can be done with some
programming tricks. This package defines the function
[`rename`](https://melff.github.io/memisc/reference/rename.md),
[`dimrename`](https://melff.github.io/memisc/reference/dimrename.md),
[`colrename`](https://melff.github.io/memisc/reference/dimrename.md),
and [`rowrename`](https://melff.github.io/memisc/reference/dimrename.md)
that implement these tricks in a convenient way, so that programmers
(like the author of this package) need not reinvent the weel in every
instance of changing names of an object.

### Dimension-Preserving Versions of `lapply` and `sapply`

If a function that is involved in a call to
[`sapply`](https://rdrr.io/r/base/lapply.html) returns a result an array
or a matrix, the dimensional information gets lost. Also, if a list
object to which [`lapply`](https://rdrr.io/r/base/lapply.html) or
[`sapply`](https://rdrr.io/r/base/lapply.html) are applied have a
dimension attribute, the result looses this information. The functions
[`Lapply`](https://melff.github.io/memisc/reference/Sapply.md) and
[`Sapply`](https://melff.github.io/memisc/reference/Sapply.md) defined
in this package preserve such dimensional information.

### Combining Vectors and Arrays by Names

The generic function
[`collect`](https://melff.github.io/memisc/reference/collect.md)
collects several objects of the same mode into one object, using their
names, `rownames`, `colnames` and/or `dimnames`. There are methods for
atomic vectors, arrays (including matrices), and data frames. For
example

      a <- c(a=1,b=2)
      b <- c(a=10,c=30)
      collect(a,b)
      

leads to

         x  y
      a  1 10
      b  2 NA
      c NA 30
      

### Reordering of Matrices and Arrays

The `memisc` package includes a
[`reorder`](https://melff.github.io/memisc/reference/reorder.array.md)
method for arrays and matrices. For example, the matrix method by
default reorders the rows of a matrix according the results of a
function.
