# HTML Output for 'view.

An alternative to 'View' for use with 'view'.

## Usage

``` r
view_html(x,title=deparse(substitute(x)),output,...)
```

## Arguments

- x:

  the result of `viewPrep`, a matrix of character strings.

- title:

  an optional character string; shown as the title of the display.

- output:

  a function or the name of a function. It determines how where the HTML
  code is directed to.

  If the working environment is RStudio, the default value is
  `"file.show"`. In other interactive environments it is `"browser"`. In
  non-interactive sessions it is `"stdout"`.

  If `output` equals `"browser"` the generated HTML code is shown using
  [`browseURL`](https://rdrr.io/r/utils/browseURL.html). If `output`
  equals `"stdout"` the HTML code is written to the console output
  window. If `output` equals `"file.show"`, the function `file.show` is
  used.

  If `view_html` is called within a *Jupyter* session, the HTML code
  created is envelopped in a pair of `<div>` tags and included into the
  Jupyter output.

- ...:

  other arguments; ignored.

## Examples

``` r
if (FALSE) { # \dontrun{
    example(data.set)
    view(Data,vfunc=view_html)
} # }
```
