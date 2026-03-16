# Format "Flattened Tables" as HTML

This is the method of
[`format_html`](https://melff.github.io/memisc/reference/format_html.md)
for "ftable" objects (i.e. flattened contingency tables)

## Usage

``` r
# S3 method for class 'ftable'
format_html(x, 
                   show.titles = TRUE, 
                   digits = 0, 
                   format = "f", 
                   toprule = 2, midrule = 1, bottomrule = 2, 
                   split.dec = TRUE, 
                   style = ftable_format_stdstyle,
                   margin="2ex auto", 
                   ...)
# S3 method for class 'ftable_matrix'
format_html(x,
                   show.titles=TRUE,
                   digits=0,
                   format="f",
                   toprule=2,midrule=1,bottomrule=2,
                   split.dec=TRUE,
                   style = ftable_format_stdstyle,
                   margin="2ex auto", 
                   varontop,
                   varinfront,
                   grouprules=1,
                   multi_digits=NULL,
                   ...)
```

## Arguments

- x:

  an object of class [`ftable`](https://rdrr.io/r/stats/ftable.html).

- show.titles:

  logical; should the names of the cross-classified variables be shown?

- digits:

  number of digits to be shown after the decimal dot. This is only
  useful, if the "ftable" object was created from a table created with
  [`genTable`](https://melff.github.io/memisc/reference/genTable.md) or
  the like.

- format:

  a format string for [`formatC`](https://rdrr.io/r/base/formatc.html)

- toprule:

  integer; thickness in pixels of rule at the top of the table.

- midrule:

  integer; thickness in pixels of rules within the table.

- bottomrule:

  integer; thickness in pixels of rule at the bottom of the table.

- split.dec:

  logical; whether numbers should be centered at the decimal point by
  splitting the table cells.

- style:

  string containing the standard CSS styling of table cells.

- margin:

  character string, determines the margin and thus the position of the
  HTML table.

- varontop:

  logical; whether names of column variables should appear on top of
  factor levels

- varinfront:

  logical; whether names of row variables should appear in front of
  factor levels

- grouprules:

  integer, should be either 1 or 2; whether one or two rules should
  drawn to distinguish groups of rows.

- multi_digits:

  NULL, a numeric vector, or a list. If it is a list it should have as
  many elements as the "ftable_matrix" contains columns, where each
  vector has as many columns as the respective "ftable". If it is a
  vector, it is put into a list with replicated elements according to
  the "ftable" components. The elements of these vectors can be used to
  specify a separate number of digits for each column of the respective
  "ftable".

- ...:

  further arguments, ignored.

## See also

See Also as
[`format_html`](https://melff.github.io/memisc/reference/format_html.md),
[`show_html`](https://melff.github.io/memisc/reference/format_html.md),
[`write_html`](https://melff.github.io/memisc/reference/format_html.md).
