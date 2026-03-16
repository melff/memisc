# Operators for Setting Annotations and Attributes

The operator `%#%` can be used to attach a
[`description`](https://melff.github.io/memisc/reference/annotations.md)
annotation to an object. `%##%` can be used to attach a character vector
of annotations to an object. `%@%` returns the attribute with the name
given as second argument. With `%@%` it is also possible to assign
attributes.

## Usage

``` r
x %#% descr
x %##% annot
x %@% nm
x %@% nm <- value
```

## Arguments

- x:

  an object, usually and
  [`item`](https://melff.github.io/memisc/reference/items.md) or a
  vector.

- descr:

  a character string

- annot:

  a named character vector; its contents are added to the "annotation"
  attribute of `x`. Existing elements are kept.

- nm:

  a character string, the name of the attribute being set or requested.

- value:

  any kind of object that can be attached as an attribute.

## Examples

``` r
test1 <- 1 %#% "One"
# This is equivalent to:
# test <- 1
# description(test) <- "One"
description(test1)
#> [1] "One"
# Results in "One"

# Not that it makes sense, but ...
test2 <- 2 %##% c(
                    Precedessor = 0,
                    Successor   = 2
                 )
# This is equivalent to:
# test2 <- 2
# annotation(test2) <- c(
#                    Precedessor = 0,
#                    Successor   = 2
#                 )
annotation(test2)
#> Precedessor:
#>      0
#> 
#> Successor:
#>      2
#> 

# The following examples are equivalent to
# attr(test2,"annotation")
test2 %@% annotation
#> Precedessor:
#>      0
#> 
#> Successor:
#>      2
#> 

test2 %@% "annotation"
#> Precedessor:
#>      0
#> 
#> Successor:
#>      2
#> 

test2 %@% another.attribute <- 42
# This is equivalent to attr(test2,"another.attribute") <- 42

attributes(test2)
#> $annotation
#> Precedessor:
#>      0
#> 
#> Successor:
#>      2
#> 
#> 
#> $another.attribute
#> [1] 42
#> 
```
