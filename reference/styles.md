# Formatting Styles for Coefficients, Factor Contrasts, and Summary Statistics

Methods for setting and getting templates for formatting model
coefficients and summaries for use in
[`mtable`](https://melff.github.io/memisc/reference/mtable.md).

## Usage

``` r
setCoefTemplate(...)
getCoefTemplate(style)
getSummaryTemplate(x)
setSummaryTemplate(...)
summaryTemplate(x)
```

## Arguments

- ...:

  sevaral tagged arguments; in case of `setCoefTemplate` the tags
  specify the `coef.style`s, in case of `setSummaryTemplate` they
  specify model classes. The associated values are
  [template](https://melff.github.io/memisc/reference/applyTemplate.md)s.

- style:

  a character string with the name of a coefficient style, if left
  empty, all coefficient templates are returned.

- x:

  a model or a name of a model class, for example `"lm"` or `"glm"`; if
  left empty, all summary templates are returned.

## Details

The style in which model coefficients are formatted by
[`mtable`](https://melff.github.io/memisc/reference/mtable.md) is by
default selected from the `coef.style` setting of
[`options`](https://rdrr.io/r/base/options.html), "factory-fresh"
setting being `options(coef.style="default")`.

The appearance of factor levels in an
[`mtable`](https://melff.github.io/memisc/reference/mtable.md) can be
influenced by the `factor.style` setting of
[`options`](https://rdrr.io/r/base/options.html). The "factory-fresh"
setting is `options(factor.style="($f): ($l)")`, where `($f)` stands for
the factor name and `($l)` stands for the factor level. In case of
treatment contrasts, the baseline level will also appear in an
[`mtable`](https://melff.github.io/memisc/reference/mtable.md) separated
from the current factor level by the `baselevel.sep` setting of
[`options`](https://rdrr.io/r/base/options.html). The "factory-fresh"
setting is `options(baselevel.sep="-")`,

Users may specify additional coefficient styles by a call to
`setCoefTemplate`.

In order to adapt the display of summary statistics of other model
classes, users need to set a template for model summaries via a call to
`setSummaryTemplate` or to define a method of the generic function
`summaryTemplate`.
