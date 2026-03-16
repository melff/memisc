# Adding Annotations to Objects

Annotations, that is, objects of class `"annotation"`, are character
vectors with all their elements named. Only one method is defined for
this subclass of character vectors, a method for `show`, that shows the
annotation in a nicely formatted way. Annotations of an object can be
obtained via the function `annotation(x)` and can be set via
`annotation(x)<-value`.

Elements of an annotation with names `"description"` and `"wording"`
have a special meaning. The first kind can be obtained and set via
`description(x)` and `description(x)<-value`, the second kind can be
obtained via `wording(x)` and `wording(x)<-value`. `"description"`
elements are used in way the "variable labels" are used in SPSS and
Stata. `"wording"` elements of annotation objects are meant to contain
the question wording of a questionnaire item represented by an `"item"`
objects. These elements of annotations are treated in a special way in
the output of the `coodbook` function.

## Usage

``` r
annotation(x)
# S4 method for class 'ANY'
annotation(x)
# S4 method for class 'item'
annotation(x)
# S4 method for class 'data.set'
annotation(x)
annotation(x) <- value
# S4 method for class 'ANY,character'
annotation(x) <- value
# S4 method for class 'ANY,annotation'
annotation(x) <- value
# S4 method for class 'item,annotation'
annotation(x) <- value
# S4 method for class 'vector,annotation'
annotation(x) <- value

description(x)
description(x) <- value

wording(x)
wording(x) <- value

# S4 method for class 'data.set'
description(x)
# S4 method for class 'importer'
description(x)
# S4 method for class 'data.frame'
description(x)
# S4 method for class 'tbl_df'
description(x)
```

## Arguments

- x:

  an object

- value:

  a character or annotation object

## Value

`annotation(x)` returns an object of class `"annotation"`, which is a
named character. `description(x)` and `wording(x)` each usually return a
character string. If `description(x)` is applied to a
[`data.set`](https://melff.github.io/memisc/reference/dataSets.md) or an
[`importer`](https://melff.github.io/memisc/reference/importers.md)
object, however, a character vector is returned, which is named after
the variables in the data set or the external file.

## Examples

``` r
vote <- sample(c(1,2,3,8,9,97,99),size=30,replace=TRUE)
labels(vote) <- c(Conservatives         =  1,
                    Labour                =  2,
                    "Liberal Democrats"   =  3,
                    "Don't know"          =  8,
                    "Answer refused"      =  9,
                    "Not applicable"      = 97,
                    "Not asked in survey" = 99
                    )
missing.values(vote) <- c(97,99)
description(vote) <- "Vote intention"
wording(vote) <- "If a general election would take place next tuesday,
                    the candidate of which party would you vote for?"
annotation(vote)
#> description:
#>      Vote intention
#> 
#> wording:
#>      If a general election would take place next tuesday, the candidate of
#>      which party would you vote for?
#> 
annotation(vote)["Remark"] <- "This is not a real questionnaire item, of course ..."
codebook(vote)
#> ================================================================================
#> 
#>    vote 'Vote intention'
#> 
#>    "If a general election would take place next tuesday, the candidate of which
#>    party would you vote for?"
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'Conservatives'           9  33.3  30.0
#>     2   'Labour'                  6  22.2  20.0
#>     3   'Liberal Democrats'       6  22.2  20.0
#>     8   'Don't know'              1   3.7   3.3
#>     9   'Answer refused'          5  18.5  16.7
#>    97 M 'Not applicable'          2         6.7
#>    99 M 'Not asked in survey'     1         3.3
#> 
#>    Remark:
#>        This is not a real questionnaire item, of course ...
#> 
```
