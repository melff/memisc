# Deprecated Functions in Package memisc

These functions are provided for compatibility with older versions of
memisc only, and may be defunct as soon as the next release.

## Usage

``` r
fapply(formula,data,...) # calls UseMethod("fapply",data)
# Default S3 method
fapply(formula, data, subset=NULL,
      names=NULL, addFreq=TRUE,...)
```

## Arguments

- formula:

  a formula. The right hand side includes one or more grouping variables
  separated by '+'. These may be factors, numeric, or character vectors.
  The left hand side may be empty, a numerical variable, a factor, or an
  expression. See details below.

- data:

  an environment or data frame or an object coercable into a data frame.

- subset:

  an optional vector specifying a subset of observations to be used.

- names:

  an optional character vector giving names to the result(s) yielded by
  the expression on the left hand side of `formula`. This argument may
  be redundant if the left hand side results in is a named vector. (See
  the example below.)

- addFreq:

  a logical value. If TRUE and `data` is a table or a data frame with a
  variable named "Freq", a call to `table`,
  [`Table`](https://melff.github.io/memisc/reference/Table.md),
  [`percent`](https://melff.github.io/memisc/reference/percent.md), or
  [`nvalid`](https://melff.github.io/memisc/reference/value-filters.md)
  is supplied by an additional argument `Freq` and a call to `table` is
  translated into a call to `Table`.

- ...:

  further arguments, passed to methods or ignored.
