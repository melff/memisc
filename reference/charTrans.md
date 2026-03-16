# Character Translation of Aspects of Objects

This function uses the base package function
[`chartr`](https://rdrr.io/r/base/chartr.html) to translate characters
in variable descriptions (a.k.a variable labels) and value labels of
[`item`](https://melff.github.io/memisc/reference/items.md),
[`data.set`](https://melff.github.io/memisc/reference/dataSets.md), and
[`importer`](https://melff.github.io/memisc/reference/importers.md)
objects.

It will be useful when the encoding of an important data set cannot be
fully identified or if the encoding in the data file is incorrect or
unknown.

## Usage

``` r
charTrans(x, old = "", new = "", ...)
# S3 method for class 'annotation'
charTrans(x, old = "", new = "", ...)
# S3 method for class 'data.set'
charTrans(x, old = "", new = "", ...)
# S3 method for class 'importer'
charTrans(x, old = "", new = "", ...)
# S3 method for class 'item'
charTrans(x, old = "", new = "", ...)
# S3 method for class 'value.labels'
charTrans(x, old = "", new = "", ...)
```

## Arguments

- x:

  a character vector or an object of which character data or attributes
  character-translated.

- old:

  a string with the characters to be translated.

- new:

  a string with the translated characters.

- ...:

  further arguments, currently ignored.

## Value

`charTrans` returns a copy of its first argument with
character-translated character data or attributes.

## See also

[`chartr`](https://rdrr.io/r/base/chartr.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Locate an SPSS 'portable' file and get info on variables, their labels 
S2601.POR <- spss.portable.file("POR-Files/S2601.POR", encoded = "cp850")
# 'ß' appears to be correctly coded, but 'ä', 'ö', 'ü' are not, so we need to
# to some fine-tuning
S2601.POR <- charTrans(S2601.POR, old="{|}\r", new="äöü ")
# Now labels etc. are correctly encoded.
codebook(S2601.POR)
} # }
```
