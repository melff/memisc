# Reshape data frames or data sets

`Reshape` is a convenience wrapper around
[`reshape`](https://rdrr.io/r/stats/reshape.html) with a somewhat
simpler syntax.

## Usage

``` r
Reshape(data,...,id,within_id,drop,keep,direction)
```

## Arguments

- data:

  a data frame or data set to be reshaped.

- ...:

  Further arguments that specify the variables in long and in wide
  format as well as the time variable. The name tags of the arguments
  given here specify variable names in long format, the arguments
  themselves specify the variables in wide format (or observations in
  long vormat) and the variable of the "time" variable. The time
  variable is usually the last of these arguments. An "automatic" time
  variable can be specified if only a single argument in `...` is given.

- id:

  a variable name or a concatenation of variable names (either as
  character strings or as unquoted symbols), that identify individual
  units. Defaults to `"id"` or the id variable specified in the
  `"reshapeLong"` attribute of the `data` argument. Needed only if the
  data are reshaped from long to wide format.

- within_id:

  an optional variable name (either as character string or as unquoted
  symbol), that identifies individual observations on units. Relevant
  only if the data are reshaped from long to wide format.

- drop:

  a variable name or a concatenation of variable names (either as
  character strings or as unquoted symbols), thast specifies the
  variables to be dropped before reshaping.

- keep:

  a variable name or a concatenation of variable names (either as
  character strings or as unquoted symbols), thast specifies the
  variables to be kept after reshaping (including the ones used to
  define the reshaping).

- direction:

  a character string, should be either equal "long" or "wide".

## Examples

``` r
example.data.wide <- data.frame(
    v  = c(35,42),
    x1 = c(1.1,2.1),
    x2 = c(1.2,2.2),
    x3 = c(1.3,2.3),
    x4 = c(1.4,2.4),
    y1 = c(2.5,3.5),
    y2 = c(2.7,3.7),
    y3 = c(2.9,3.9))
example.data.wide
#>    v  x1  x2  x3  x4  y1  y2  y3
#> 1 35 1.1 1.2 1.3 1.4 2.5 2.7 2.9
#> 2 42 2.1 2.2 2.3 2.4 3.5 3.7 3.9

# The following two calls are equivalent:
example.data.long <- Reshape(data=example.data.wide,
                             x=c(x1,x2,x3,x4),
                             # N.B. it is possible to
                             # specify 'empty' i.e. missing
                             # measurements
                             y=c(y1,y2,y3,),
                             t=1:4,
                             direction="long")

example.data.long <- Reshape(data=example.data.wide,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 # N.B. it is possible to
                                 # specify 'empty' i.e. missing
                                 # measurements
                                 y=c(y1,y2,y3,)
                             ),
                             t=1:4,
                             direction="long")

example.data.long
#>      v t   x   y id
#> 1.1 35 1 1.1 2.5  1
#> 1.2 35 2 1.2 2.7  1
#> 1.3 35 3 1.3 2.9  1
#> 1.4 35 4 1.4  NA  1
#> 2.1 42 1 2.1 3.5  2
#> 2.2 42 2 2.2 3.7  2
#> 2.3 42 3 2.3 3.9  2
#> 2.4 42 4 2.4  NA  2

# Since the data frame contains an "reshapeLong" attribute
# an id variable is already specified and part of the data
# frame.
example.data.wide <- Reshape(data=example.data.long,
                             x=c(x1,x2,x3,x4),
                             y=c(y1,y2,y3,),
                             t=1:4,
                             direction="wide")
example.data.wide
#>      v id  x1  x2  x3  x4  y1  y2  y3
#> 1.1 35  1 1.1 1.2 1.3 1.4 2.5 2.7 2.9
#> 2.1 42  2 2.1 2.2 2.3 2.4 3.5 3.7 3.9

# Here we examine the case where no "reshapeLong" attribute
# is present:
example.data.wide <- Reshape(data=example.data.long,
                             x=c(x1,x2,x3,x4),
                             y=c(y1,y2,y3,),
                             t=1:4,
                             id=v,
                             direction="wide")
example.data.wide
#>      v id  x1  x2  x3  x4  y1  y2  y3
#> 1.1 35  1 1.1 1.2 1.3 1.4 2.5 2.7 2.9
#> 2.1 42  2 2.1 2.2 2.3 2.4 3.5 3.7 3.9


# Here, an "automatic" time variable is created. This works
# only if there is a single argument other than the data=
# and direction= arguments
example.data.long <- Reshape(data=example.data.wide,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 y=c(y1,y2,y3,)
                             ),
                             direction="long")

example.data.long
#>      v id times   x   y
#> 1.1 35  1     1 1.1 2.5
#> 1.2 35  1     2 1.2 2.7
#> 1.3 35  1     3 1.3 2.9
#> 1.4 35  1     4 1.4  NA
#> 2.1 42  2     1 2.1 3.5
#> 2.2 42  2     2 2.2 3.7
#> 2.3 42  2     3 2.3 3.9
#> 2.4 42  2     4 2.4  NA

example.data.wide <- Reshape(data=example.data.long,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 y=c(y1,y2,y3,)
                             ),
                             direction="wide")
example.data.wide
#>      v id  x1  x2  x3  x4  y1  y2  y3
#> 1.1 35  1 1.1 1.2 1.3 1.4 2.5 2.7 2.9
#> 2.1 42  2 2.1 2.2 2.3 2.4 3.5 3.7 3.9
```
