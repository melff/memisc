# A Generic Viewing Function

The function `view` provides generic interface to the non-generic
function `View`.

In contrast to the implementation of `View` provided by either basic *R*
or *RStudio*, this function can be extended to handle new kinds of
objects by defining `viewPrep` methods for them. Further, `view` can be
adapted to other GUIs by specifying the `"vfunc"` option or the `vfunc=`
optional argument.

Internally, `view` usues the generic function `viewPrep` to prepare data
so it can be passed on to the (non-generic) function `View` or
(optionally) a different graphical user interface function that can be
used to display matrix- or data frame-like objects.

The `vfunc` argument determines how the result of `viewPrep` is
displayed. Its default is the function `View`, but an alternative is
[`view_html`](https://melff.github.io/memisc/reference/view_html.md)
which creates and displays an HTML grid.

## Usage

``` r
view(x,
     title=deparse(substitute(x)),
     vfunc=getOption("vfunc","View"),
     ...)

# The internal generic, not intended to be used by the end-user.
viewPrep(x,title,...)
# S3 method for class 'data.set'
viewPrep(x,title,...)
# S3 method for class 'data.frame'
viewPrep(x,title,...)
# S3 method for class 'descriptions'
viewPrep(x,title,...)
# S3 method for class 'codeplan'
viewPrep(x,title,compact=FALSE,...)
# S3 method for class 'importer'
viewPrep(x,title,compact=TRUE,...)
```

## Arguments

- x:

  an object, e.g. a data frame, data.set, or importer.

- title:

  an optional character string; shown as the title of the display.

- vfunc:

  a character string; a name of a GUI function to call with the results
  of `viewPrep()`

- compact:

  a logical value; should the codeplan be shown in a compact form - one
  line per variable - or in a more expanive form - one line per labelled
  value?

- ...:

  further arguments; `view()` passes them on to `viewPrep`.

## Examples

``` r
if (FALSE) { # \dontrun{
    example(data.set)
    view(Data)
    view(description(Data))
    view(codeplan(Data))
    # Note that this file is *not* included in the package
    # and has to be obtained from GESIS in order to run the 
    # following
    ZA7500sav <- spss.file("ZA7500_v2-0-0.sav")
    view(ZA7500sav)
} # }
```
