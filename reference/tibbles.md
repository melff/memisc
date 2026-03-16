# Interface to Packages 'tibble' and 'haven'

A `as_tibble` method (`as_table.data.set`) allows to transform
`"data.set"` objects into objects of class `"tbl_df"` as defined by the
package "tibble".

`as.item` methods for objects of classes `"haven_labelled"` and
`"have_labelled_spss"` allow to transform a "tibble" imported using
`read_dta`, `read_spss`, etc. from the package "haven" into an object of
class `"data.set"`.

`as_haven` can be used to transform `"data.set"` objects into objects of
class `"tbl_df"` with that additional information that objects imported
using the "haven" package usually have, i.e. variable labels and value
labels (as the `"label"` and `"labels"` attributes of the columns).

## Usage

``` r
as_tibble.data.set(x,...)
# S4 method for class 'haven_labelled'
as.item(x,...)
# S4 method for class 'haven_labelled_spss'
as.item(x,...)
as_haven(x,...)
# S4 method for class 'data.set'
as_haven(x,user_na=FALSE,...)
# S4 method for class 'item.vector'
as_haven(x,user_na=FALSE,...)
# S4 method for class 'tbl_df'
as.data.set(x,row.names=NULL,...)
```

## Arguments

- x:

  for `as_tibble.data.set` and `as_haven`, an object of class
  `"data.set"`; for `as.item`, an object of class `"haven_labelled"` or
  `"haven_labelled_spss"`; an object of class `"tbl_df"` for
  `as.data.set`.

- user_na:

  logical; if `TRUE` then the resulting vectors have an `"na_values"`
  and/or `"na_range"` attribute.

- row.names:

  `NULL` or an optional character vector of row names.

- ...:

  further arguments, passed through to other the the `as_tibble` method
  for lists, or ignored.

## Value

`as_tibble.data.set` and the `"data.set"`-method of `as_haven` return a
"tibble". The `"item.vector"`-method (which is for internal use only)
returns a vector with S3 class either `"haven_labelled"` or
`"haven_labelled_spss"`.
