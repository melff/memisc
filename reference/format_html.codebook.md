# Format Codebooks as HTML

This is the method of
[`format_html`](https://melff.github.io/memisc/reference/format_html.md)
for "codebook" objects as created by the eponymous function (see
[`codebook`](https://melff.github.io/memisc/reference/codebook.md))

## Usage

``` r
# S3 method for class 'codebook'
format_html(x, 
    toprule = 2,
    midrule = 1,
    indent = "3ex",
    style = codebook_format_stdstyle,
    var_tag = "code", 
    varid_prefix = "", title_tag = "p",...)
```

## Arguments

- x:

  a "codebook" object

- toprule:

  a non-negative integer; thickness of the line (in pixels) at the top
  of each codebook entry

- midrule:

  a non-negative integer; thickness of the line (in pixels) that
  separates the header of an codebook entry from its body

- indent:

  character string; indentation (by padding) of the codebook entry
  contents

- style:

  string containing the standard CSS styling of codebook table cells.

- var_tag:

  character string; the HTML tag that contains the name of the variable

- varid_prefix:

  character string; a prefix added to the anchor IDs of the code entry
  titles (to facilitate the creation of tables of contents etc.)

- title_tag:

  character string; the HTML tag that contains the title of the codebook
  entry (the variable name and its description)

- ...:

  further arguments, ignored.

## See also

See Also as
[`format_html`](https://melff.github.io/memisc/reference/format_html.md),
[`show_html`](https://melff.github.io/memisc/reference/format_html.md),
[`write_html`](https://melff.github.io/memisc/reference/format_html.md).
