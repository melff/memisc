# Write Codebooks and Variable Descriptions into a Text File

This is a convenience function to facilitate the creation of data set
documents in text files.

## Usage

``` r
Write(x,...)
# S3 method for class 'codebook'
Write(x,file=stdout(),...)
# S3 method for class 'descriptions'
Write(x,file=stdout(),...)
```

## Arguments

- x:

  a "codebook" or "descriptions" object.

- file:

  a connection, see
  [connections](https://rdrr.io/r/base/connections.html).

- ...:

  further arguments, ignored or passed on to particular methods.
