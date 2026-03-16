# Print Format for 'mtable' Objects

`mtable_format_print` formats 'mtable' in a way suitable for screen
output with 'print'.

## Usage

``` r
mtable_format_print(x,
  topsep="=",
  bottomsep="=",
  sectionsep="-",
  interaction.sep = " x ",
  center.at=getOption("OutDec"),
  align.integers=c("dot","right","left"),
  padding = "  ",
  ...
  )
```

## Arguments

- x:

  an object of class `mtable`

- topsep:

  a character string that is recycled to a top rule.

- bottomsep:

  a character string that is recycled to a bottom rule.

- sectionsep:

  a character string that is recycled to seperate coefficients from
  summary statistics.

- interaction.sep:

  a character string that separates factors that are involved in an
  interaction effect

- center.at:

  a character string on which resulting values are centered. Typically
  equal to ".". This is the default when `forLaTeX==TRUE`. If `NULL`,
  reported values are not centered.

- align.integers:

  how to align integer values.

- padding:

  a character string, usually whitespace, used to insert left- and
  right-padding of table contents.

- ...:

  further arguments, ignored.

## Value

A character string.
