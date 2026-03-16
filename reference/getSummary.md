# Get Model Summaries for Use with "mtable"

A generic function and methods to collect coefficients and summary
statistics from a model object. It is used in
[`mtable`](https://melff.github.io/memisc/reference/mtable.md)

## Usage

``` r
# S3 method for class 'lm'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'glm'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'merMod'
getSummary(obj, alpha=.05, ...)

# These are contributed by Christopher N. Lawrence
  # S3 method for class 'clm'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'polr'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'simex'
getSummary(obj, alpha=.05,...)

# These are contributed by Jason W. Morgan
  # S3 method for class 'aftreg'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'coxph'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'phreg'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'survreg'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'weibreg'
getSummary(obj, alpha=.05,...)

# These are contributed by Achim Zeileis
  # S3 method for class 'ivreg'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'tobit'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'hurdle'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'zeroinfl'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'betareg'
getSummary(obj, alpha=.05,...)
  # S3 method for class 'multinom'
getSummary(obj, alpha=.05,...)
  
# A variant that reports exponentiated coefficients.
# The default method calls 'getSummary()' internally and should
# be applicable to all classes for which 'getSummary()' methods exist.
getSummary_expcoef(obj, alpha=.05,...)
  # Default S3 method
getSummary_expcoef(obj, alpha=.05,...)
```

## Arguments

- obj:

  a model object, e.g. of class `lm` or `glm`

- alpha:

  level of the confidence intervals; their coverage should be 1-alpha/2

- ...:

  further arguments; ignored.

## Details

The generic function `getSummary` is called by
[`mtable`](https://melff.github.io/memisc/reference/mtable.md) in order
to obtain the coefficients and summaries of model objects. In order to
adapt [`mtable`](https://melff.github.io/memisc/reference/mtable.md) to
models of classes other than [`lm`](https://rdrr.io/r/stats/lm.html) or
[`glm`](https://rdrr.io/r/stats/glm.html) one needs to define
`getSummary` methods for these classes and to set a summary template via
[`setSummaryTemplate`](https://melff.github.io/memisc/reference/styles.md)

## Value

Any method of `getSummary` must return a list with the following
components:

- coef:

  an array with coefficient estimates; the lowest dimension *must* have
  the following names and meanings:

  |        |     |                                        |
  |--------|-----|----------------------------------------|
  | `est`  |     | the coefficient estimates,             |
  | `se`   |     | the estimated standard errors,         |
  | `stat` |     | t- or Wald-z statistics,               |
  | `p`    |     | significance levels of the statistics, |
  | `lwr`  |     | lower confidence limits,               |
  | `upr`  |     | upper confidence limits.               |

  The higher dimensions of the array correspond to the individual
  coefficients and, in multi-equation models, to the model equations.

- sumstat:

  a vector containing the model summary statistics; the components may
  have arbitrary names.
