# Additional Methods for LaTeX Representations for R objects

Methods for the generic function
[`toLatex`](https://rdrr.io/r/utils/toLatex.html) of package
“[utils](https://rdrr.io/r/utils/utils-package.html)” are provided for
generating LaTeX representations of matrices and flat contingency tables
(see [`ftable`](https://rdrr.io/r/stats/ftable.html)). Also a default
method is defined that coerces its first argument into a matrix and
applies the matrix method.

## Usage

``` r
# Default S3 method
toLatex(object,...)

# S3 method for class 'matrix'
toLatex(object,
    show.titles=TRUE,
    show.vars=FALSE,
    show.xvar=show.vars,
    show.yvar=show.vars,
    digits=if(is.table(object)) 0 else getOption("digits"),
    format="f",
    useDcolumn=getOption("useDcolumn",TRUE),
    colspec=if(useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
    midrule=if(useBooktabs) "\\midrule" else "\\hline",
    cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
    bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    ...)


# S3 method for class 'data.frame'
toLatex(object,
    digits=getOption("digits"),
    format="f",
    useDcolumn=getOption("useDcolumn",TRUE),
    numeric.colspec=if(useDcolumn)
                       paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
                    else "r",
    factor.colspec="l",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
    midrule=if(useBooktabs) "\\midrule" else "\\hline",
    cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
    bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
    row.names=is.character(attr(object,"row.names")),
    NAas="",
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    ...)

# S3 method for class 'ftable'
toLatex(object,
    show.titles=TRUE,
    digits=if(is.integer(object)) 0 else getOption("digits"),
    format=if(is.integer(object)) "d" else "f",
    useDcolumn=getOption("useDcolumn",TRUE),
    colspec=if(useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
    midrule=if(useBooktabs) "\\midrule" else "\\hline\n",
    cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
    bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
    extrarowsep = NULL,
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    fold.leaders=FALSE,
    ...)

# S3 method for class 'ftable_matrix'
toLatex(object,
    show.titles=TRUE,
    digits=getOption("digits"),
    format="f",
    useDcolumn=getOption("useDcolumn",TRUE),
    colspec=if(useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
    midrule=if(useBooktabs) "\\midrule" else "\\hline",
    cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
    bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
    compact=FALSE,
    varontop,varinfront,
    groupsep="3pt",
    grouprule=midrule,
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    multi_digits=NULL,
    ...)
```

## Arguments

- object:

  an [`ftable`](https://rdrr.io/r/stats/ftable.html), a matrix or an
  object coercable into a matrix.

- show.titles:

  logical, should variable names (in case of the `ftable` and `table`
  methods) or row and column names (in case of the `matrix` method) be
  appear in the `LaTeX` code?

- show.vars,show.xvar,show.yvar:

  logical, should the names of the dimnames of `object` be shown in the
  margins of the LaTeX tabular? Such names usually represent the row
  and/or column variables of a two-dimensional
  [`table`](https://rdrr.io/r/base/table.html).

- digits:

  number of significant digits.

- format:

  character containing a format specifier, see
  [`format`](https://rdrr.io/r/base/format.html).

- useDcolumn:

  logical, should the facilities of the `dcolumn` LaTeX package be used?
  Note that, if TRUE, you will need to include `\usepackage{dcolumn}` in
  the preamble of your LaTeX document.

- colspec:

  character, LaTeX table column format specifyer(s).

- numeric.colspec:

  character, LaTeX table column format specifyer(s) for numeric vectors
  in the data frame.

- factor.colspec:

  character, LaTeX table column format specifyer(s) for factors in the
  data frame.

- LaTeXdec:

  character, the decimal point in the final LaTeX output.

- ddigits:

  integer, digits after the decimal point.

- useBooktabs:

  logical, should the facilities of the `booktabs` LaTeX package be
  used? Note that, if TRUE, you will need to include
  `\usepackage{booktabs}` in the preamble of your LaTeX document.

- toprule:

  character string, TeX code that determines the appearance of the top
  border of the LaTeX `tabular` environment.

- midrule:

  character string, TeX code that determines how coefficients and
  summary statistics are separated in the LaTeX `tabular` environment.

- cmidrule:

  character string, TeX code that determines the appearance of rules
  under section headings.

- bottomrule:

  character string, TeX code that determines the appearance of the
  bottom border of the LaTeX `tabular` environment.

- extrarowsep:

  character string, extra code to be inserted between the column titles
  and the table body produced by `toLatex`.

- compact:

  logical, if `TRUE`, extra column space between sub-tables is
  suppressed. Defaults to `FALSE`

- varontop:

  logical, whether names of column variables should appear on top of
  factor levels

- varinfront:

  logical, whether names of row variables should appear in front of
  factor levels

- groupsep:

  character string, containing a TeX length; extra vertical space
  inserted between sub-tables, unless `compact` is `TRUE`.

- grouprule:

  character string, TeX code that determines how sub-table headings are
  embellished.

- row.names:

  logical, whether row names should be included in exported LaTeX code.

- NAas:

  character string, how missing values should be represented.

- toLatex.escape.tex:

  logical, should symbols "\$", "\_", and "^" be escaped with
  backslashes?

- fold.leaders:

  logical, if `TRUE`, factor levels of row variables are not distributed
  into different columns, but 'folded' into a single column.

- multi_digits:

  NULL, a numeric vector, or a list. If it is a list it should have as
  many elements as the "ftable_matrix" contains columns, where each
  vector has as many columns as the respective "ftable". If it is a
  vector, it is put into a list with replicated elements according to
  the "ftable" components. The elements of these vectors can be used to
  specify a separate number of digits for each column of the respective
  "ftable".

- ...:

  further argument, currently ignored.

## Examples

``` r
toLatex(diag(5))
#> \begin{tabular}{D{.}{.}{7}D{.}{.}{7}D{.}{.}{7}D{.}{.}{7}D{.}{.}{7}}
#> \toprule
#> 1.0000000 & 0.0000000 & 0.0000000 & 0.0000000 & 0.0000000 \\
#> 0.0000000 & 1.0000000 & 0.0000000 & 0.0000000 & 0.0000000 \\
#> 0.0000000 & 0.0000000 & 1.0000000 & 0.0000000 & 0.0000000 \\
#> 0.0000000 & 0.0000000 & 0.0000000 & 1.0000000 & 0.0000000 \\
#> 0.0000000 & 0.0000000 & 0.0000000 & 0.0000000 & 1.0000000 \\
#> \bottomrule
#> \end{tabular}

toLatex(ftable(UCBAdmissions))
#> \begin{tabular}{lllD{.}{.}{7}D{.}{.}{7}D{.}{.}{7}D{.}{.}{7}D{.}{.}{7}D{.}{.}{7}}
#> \toprule
#> & && \multicolumn{6}{c}{Dept}\\
#> \cmidrule{4-4}\cmidrule{5-5}\cmidrule{6-6}\cmidrule{7-7}\cmidrule{8-8}\cmidrule{9-9}
#> Admit&Gender && \multicolumn{1}{c}{A}&\multicolumn{1}{c}{B}&\multicolumn{1}{c}{C}&\multicolumn{1}{c}{D}&\multicolumn{1}{c}{E}&\multicolumn{1}{c}{F}\\
#> \midrule
#> Admitted&Male     && 512.0000000 & 353.0000000 & 120.0000000 & 138.0000000 & 53.0000000  & 22.0000000 \\
#>         &Female   && 89.0000000  & 17.0000000  & 202.0000000 & 131.0000000 & 94.0000000  & 24.0000000 \\
#> Rejected&Male     && 313.0000000 & 207.0000000 & 205.0000000 & 279.0000000 & 138.0000000 & 351.0000000\\
#>         &Female   && 19.0000000  & 8.0000000   & 391.0000000 & 244.0000000 & 299.0000000 & 317.0000000\\
#> \bottomrule
#> \end{tabular}

toLatex(rbind(
  ftable(margin.table(UCBAdmissions,c(2,1))),
  ftable(margin.table(UCBAdmissions,c(3,1)))
))
#> \begin{tabular}{llD{.}{.}{7}D{.}{.}{7}}
#> \toprule
#> &&\multicolumn{2}{c}{Admit}\\
#> \cmidrule{3-4}
#> &&\multicolumn{1}{c}{Admitted}&\multicolumn{1}{c}{Rejected}\\
#> \midrule
#> Gender & Male & 1198.0000000 & 1493.0000000\\
#>  & Female & 557.0000000 & 1278.0000000\\
#> \midrule Dept & A & 601.0000000 & 332.0000000\\
#>  & B & 370.0000000 & 215.0000000\\
#>  & C & 322.0000000 & 596.0000000\\
#>  & D & 269.0000000 & 523.0000000\\
#>  & E & 147.0000000 & 437.0000000\\
#>  & F & 46.0000000 & 668.0000000\\
#> \bottomrule
#> \end{tabular}
```
