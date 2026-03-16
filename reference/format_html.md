# Format Objects in HTML, show the HTML Format or Write it to a File

`show_html` is for showing objects in a convenient way in HTML format.
`write_html` writes them in HTML format into a file. Both functions call
the generic `format_html` for the format conversion.

## Usage

``` r
show_html(x, output = NULL, ...)
write_html(x, file, ..., standalone = TRUE)

format_html(x, ...)

# S3 method for class 'data.frame'
format_html(x,
    toprule=2,midrule=1,bottomrule=2,
    split.dec=TRUE,
    row.names=TRUE,
    digits=getOption("digits"),
    format="f",
    style=df_format_stdstyle,
    margin="2ex auto", 
    ...)
    
# S3 method for class 'matrix'
format_html(x,
    toprule=2,midrule=1,bottomrule=2,
    split.dec=TRUE,
    formatC=FALSE,
    digits=getOption("digits"),
    format="f",
    style=mat_format_stdstyle,
    margin="2ex auto", 
    ...)
```

## Arguments

- x:

  an object.

- output:

  character string or a function that determines how the HTML formatted
  object is shown.

  If `output` is a function, it is called with the path of a (temporary)
  file with HTML code, e.g. *RStudio*'s `viewer` function (which is
  available in the package `rstudioapi`.

  If `output` equals "stdout", the HTML code is written to the standard
  output stream (for use e.g. in output produced with `knitr`), if
  "file-show", the contents of a file with the HTML code is shown via
  [`file.show`](https://rdrr.io/r/base/file.show.html), and if
  "browser", the contents of a file with the HTML code is shown by the
  standard browser (via
  [`browseURL`](https://rdrr.io/r/utils/browseURL.html)).

  This arguments has different defaults, depending of the type of the
  session. In non-interactive sessions, the default is "console", in
  interactive sessions other than RStudio, it is "browser", in
  interactive sessions with RStudio it is "file-show".

  These default settings can be overriden by the option "html_viewer"
  (see [`options`](https://rdrr.io/r/base/options.html)).

- file:

  character string; name or path of the file where to write the HTML
  code to.

- toprule:

  integer; thickness in pixels of rule at the top of the table.

- midrule:

  integer; thickness in pixels of rules within the table.

- bottomrule:

  integer; thickness in pixels of rule at the bottom of the table.

- split.dec:

  logical; whether numbers should be centered at the decimal point by
  splitting the table cells.

- row.names:

  logical; whether row names should be shown/exported.

- digits:

  number of digits to be shown after the decimal dot. This is only
  useful, if the "ftable" object was created from a table created with
  [`genTable`](https://melff.github.io/memisc/reference/genTable.md) or
  the like.

- formatC:

  logical; whether to use
  [`formatC`](https://rdrr.io/r/base/formatc.html) instead of
  [`format`](https://rdrr.io/r/base/format.html) to format cell
  contents.

- format:

  a format string for [`formatC`](https://rdrr.io/r/base/formatc.html)

- style:

  string containing the stanard CSS styling of table cells.

- margin:

  character string, determines the margin and thus the position of the
  HTML table.

- ...:

  other arguments, passed on to formatter functions.

- standalone:

  logical; should HTML file contain a "!DOCTYPE" header?

## Value

`format_html` character string with code suitable for inclusion into a
HTML-file.
