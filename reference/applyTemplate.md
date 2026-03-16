# Apply a Formatting Template to a Numeric or Character Vector

`applyTemplate` is called internally by
[`mtable`](https://melff.github.io/memisc/reference/mtable.md) to format
coefficients and summary statistics.

## Usage

``` r
applyTemplate(x,template,float.style=getOption("float.style"),
                      digits=min(3,getOption("digits")),
                      signif.symbols=getOption("signif.symbols"))
```

## Arguments

- x:

  a numeric or character vector to be formatted, or a list of such
  vectors.

- template:

  a character vector that defines the template, see details.

- float.style:

  A character string that is passed to
  [`formatC`](https://rdrr.io/r/base/formatc.html) by `applyTemplate`;
  valid values are `"e"`, `"f"`, `"g"`, `"fg"`, `"E"`, and `"G"`. By
  default, the `float.style` setting of
  [`options`](https://rdrr.io/r/base/options.html) is used. The
  \`factory fresh' setting is `options(float.style="f")`

- digits:

  number of significant digits to use if not specified in the template.

- signif.symbols:

  a named vector that specifies how significance levels are symbolically
  indicated, values of the vector specify significance levels and names
  specify the symbols. By default, the `signif.symbols` setting of
  [`options`](https://rdrr.io/r/base/options.html) is used. The
  "factory-fresh" setting is
  `options(signif.symbols=c("***"=.001,"**"=.01,"*"=.05))`.

## Details

Character vectors that are used as templates may be arbitrary. However,
certain character sequences may form *template expressions*. A template
expression is of the form `($<POS>:<Format spec>)`, where "`($`"
indicates the start of a template expression, "`<POS>`" stands for
either an index or name that selects an element from `x` and
"`<Format spec>`" stands for a *format specifier*. It may contain an
letter indicating the style in which the vector element selected by
`<POS>` will be formatted by
[`formatC`](https://rdrr.io/r/base/formatc.html), it may contain a
number as the number of significant digits, a "`#`" indicating that the
number of signifcant digits will be at most that given by
`getOption("digits")`, or `*` that means that the value will be
formatted as a significance symbol.

## Value

`applyTemplate` returns a character vector in which template expressions
in `template` are substituted by formatted values from `x`. If
`template` is an array then the return value is also an array of the
same shape.

## Examples

``` r
applyTemplate(c(a=.0000000000000304,b=3),template=c("($1:g7#)($a:*)"," (($1:f2)) "))
#>      [,1]         
#> [1,] "3.04e-14***"
#> [2,] " (0.00) "   
applyTemplate(c(a=.0000000000000304,b=3),template=c("($a:g7#)($a:*)"," (($b:f2)) "))
#>      [,1]         
#> [1,] "3.04e-14***"
#> [2,] " (3.00) "   
```
