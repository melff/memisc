# Format 'mtable' Results for LaTeX

This function formats objects created by
[`mtable`](https://melff.github.io/memisc/reference/mtable.md) for
inclusion into LaTeX files.

## Usage

``` r
mtable_format_latex(x,
            useDcolumn=getOption("useDcolumn",TRUE),
            colspec=if(useDcolumn) 
                      paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") 
                    else "l",
            LaTeXdec=".",
            ddigits=min(3,getOption("digits")),
            useBooktabs=getOption("useBooktabs",TRUE),
            toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
            midrule=if(useBooktabs) "\\midrule" else "\\hline",
            cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
            bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
            interaction.sep = " $\\times$ ",
            sdigits=min(1,ddigits),
            compact=FALSE,
            sumry.multicol=FALSE,
            escape.tex=getOption("toLatex.escape.tex",FALSE),
            signif.notes.type=getOption("toLatex.signif.notes.type","include"),
            signif.notes.spec=getOption("toLatex.signif.notes.spec","p{.5\\linewidth}"),
            ...
  )
```

## Arguments

- x:

  an object of class `mtable`

- useDcolumn:

  should the `dcolumn` LaTeX package be used? If true, you will have to
  include `\usepackage{dcolumn}` into the preamble of your LaTeX
  document.

- colspec:

  LaTeX table column format specifyer(s).

- LaTeXdec:

  the decimal point in the final LaTeX output.

- ddigits:

  alignment specification or digits after the decimal point.

- useBooktabs:

  should the `booktabs` LaTeX package be used? If true, you will have to
  include `\usepackage{booktabs}` into the preamble of your LaTeX
  document.

- toprule:

  appearance of the top border of the LaTeX `tabular` environment.

- midrule:

  how are coefficients and summary statistics separated in the LaTeX
  `tabular` environment.

- cmidrule:

  appearance of rules under section headings.

- bottomrule:

  appearance of the bottom border of the LaTeX `tabular` environment.

- interaction.sep:

  a character string that separates factors that are involved in an
  interaction effect

- sdigits:

  integer; number of digits after decimal dot for summary statistics.

- compact:

  logical; should the table be compact, without extra columns between
  multi-equation models?

- sumry.multicol:

  logical, should summaries enclosed into `\multicol` commands?

- escape.tex:

  logical, should symbols `$`, `_`, and `^` be escaped with backslashes?

- signif.notes.type:

  character string; should be either `"include"`,`"append"`, `"drop"`,
  or `"tnotes"`. If `"append"`, (very simple) LaTeX code is appended
  that contains notes that relate significance symbols to p-values. If
  `"include"`, the LaTeX table will include a (multi-column) cell with
  these notes. If `"drop"`, notes will not be added. If `"tnotes"`, the
  exported LaTeX table is wrapped in a `threeparttable` environment and
  the p-value notes are wrapped in a `tablenotes` environment. This
  requires the LaTeX package `threeparttable` in order to work.

- signif.notes.spec:

  character string; specifies format of cells that include notes about
  p-values; relevant only if `signif.notes.type="include"`

- ...:

  further arguments, ignored.

## Value

A character string with code suitable for inclusion into a LaTeX-file.
