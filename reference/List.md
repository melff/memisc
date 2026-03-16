# Create a list and conveniently supply names to its elements

`List` creates a list and names its elements after the arguments given,
in a manner analogously to
[`data.frame`](https://rdrr.io/r/base/data.frame.html)

## Usage

``` r
List(...)
```

## Arguments

- ...:

  tagged or untagged arguments from which the list is formed. If the
  untagged arguments are variables from the englosing environment, their
  names become the names of the list elements.

## Examples

``` r
  num <- 1:3
  strng <- c("a","b","A","B")
  logi <- rep(FALSE,7)
  List(num,strng,logi)
#> $num
#> [1] 1 2 3
#> 
#> $strng
#> [1] "a" "b" "A" "B"
#> 
#> $logi
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 
```
