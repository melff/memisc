# Format for 'mtable' Objects for Writing into File

`mtable_mtable_print` formats 'mtable' in a way suitable for output into
a file with `write.table`

## Usage

``` r
mtable_format_delim(x,
          colsep="\t",
          rowsep="\n",
          interaction.sep = " x ",
          ...
          )
```

## Arguments

- x:

  an object of class `mtable`

- colsep:

  a character string which seperates the columns in the output.

- rowsep:

  a character string which seperates the rows in the output.

- interaction.sep:

  a character string that separates factors that are involved in an
  interaction effect

- ...:

  further arguments, ignored.

## Value

A character string.
