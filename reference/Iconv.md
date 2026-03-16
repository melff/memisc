# Convert Annotations, and Value Labels between Encodings

This function uses the base package function
[`iconv`](https://rdrr.io/r/base/iconv.html) to translate variable
descriptions (a.k.a variable labels) and value labels of
[`item`](https://melff.github.io/memisc/reference/items.md),
[`data.set`](https://melff.github.io/memisc/reference/dataSets.md), and
[`importer`](https://melff.github.io/memisc/reference/importers.md)
objects into a specified encoding.

It will be useful in UTF-8 systems when data file come in some ancient
encoding like 'Latin-1' as long used by Windows systems.

## Usage

``` r
Iconv(x,from="",to="",...) 
# S3 method for class 'character'
Iconv(x,from="",to="",...) 
# S3 method for class 'annotation'
Iconv(x,from="",to="",...) 
# S3 method for class 'data.set'
Iconv(x,from="",to="",...) 
# S3 method for class 'importer'
Iconv(x,from="",to="",...) 
# S3 method for class 'item'
Iconv(x,from="",to="",...) 
# S3 method for class 'value.labels'
Iconv(x,from="",to="",...)
```

## Arguments

- x:

  a character vector or an object of which character data or attributes
  are to be re-encoded.

- from:

  a character string describing the original encoding

- to:

  a character string describing the target encoding

- ...:

  further arguments, passed to
  [`iconv`](https://rdrr.io/r/base/iconv.html)

## Value

`Iconv` returns a copy of its first argument with re-encoded character
data or attributes.

## See also

[`iconv`](https://rdrr.io/r/base/iconv.html),
[`iconvlist`](https://rdrr.io/r/base/iconv.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Locate an SPSS 'system' file and get info on variables, their labels etc.
ZA5302 <- spss.system.file("Daten/ZA5302_v6-0-0.sav",to.lower=FALSE)

# Convert labels etc. from 'latin1' to the encoding of the current locale.
ZA5302 <- Iconv(ZA5302,from="latin1")

# Write out the codebook
writeLines(as.character(codebook(ZA5302)),
           con="ZA5302-cdbk.txt")

# Write out the description of the varialbes (their 'variable labels')
writeLines(as.character(description(ZA5302)),
            con="ZA5302-description.txt")
} # }
```
