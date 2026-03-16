# Recode Items, Factors and Numeric Vectors

`recode` substitutes old values of a factor or a numeric vector by new
ones, just like the recoding facilities in some commercial statistical
packages.

## Usage

``` r
recode(x,...,
       copy=getOption("recode_copy",identical(otherwise,"copy")),
       otherwise=NA)
# S4 method for class 'vector'
recode(x,...,
    copy=getOption("recode_copy",identical(otherwise,"copy")),
    otherwise=NA)
# S4 method for class 'factor'
recode(x,...,
    copy=getOption("recode_copy",identical(otherwise,"copy")),
    otherwise=NA)
# S4 method for class 'item'
recode(x,...,
    copy=getOption("recode_copy",identical(otherwise,"copy")),
    otherwise=NA)
```

## Arguments

- x:

  An object

- ...:

  One or more assignment expressions, each of the form
  `new.value <- old.values`. `new.value` should be a scalar numeric
  value or character string. If one of the `new.value`s is a character
  string, the return value of `recode` will be a factor and each
  `new.value` will be coerced to a character string that labels a level
  of the factor.

  Each `old.value` in an assignment expression may be a (numeric or
  character) vector. If `x` is numeric such an assignment expression may
  have the form `new.value <- range(lower,upper)` In that case, values
  between `lower` and `upper` are exchanged by `new.value`. If one of
  the arguments to `range` is `min`, it is substituted by the minimum of
  `x`. If one of the arguments to `range` is `max`, it is substituted by
  the maximum of `x`.

  In case of the method for `labelled` vectors, the *tags* of arguments
  of the form `tag = new.value <- old.values` will define the labels of
  the new codes.

  If the `old.values` of different assignment expressions overlap, an
  error will be raised because the recoding is ambigous.

- copy:

  logical; should those values of `x` not given an explicit new code
  copied into the resulting vector?

- otherwise:

  a character string or some other value that the result may obtain. If
  equal to `NA` or `"NA"`, original codes not given an explicit new code
  are recoded into `NA`. If equal to `"copy"`, original codes not given
  an explicit new code are copied.

## Value

A numerical vector, factor or an `item` object.

## Details

`recode` relies on the lazy evaluation mechanism of *R*: Arguments are
not evaluated until required by the function they are given to. `recode`
does not cause arguments that appear in `...` to be evaluated. Instead,
`recode` parses the `...` arguments. Therefore, although expressions
like `1 <- 1:4` would cause an error action, if evaluated at any place
elsewhere in *R*, they will not cause an error action, if given to
`recode` as an argument. However, a call of the form `recode(x,1=1:4)`,
would be a syntax error.

If John Fox' package "car" is installed, `recode` will also be callable
with the syntax of the `recode` function of that package.

## See also

`recode` of package "car".

## Examples

``` r
x <- as.item(sample(1:6,20,replace=TRUE),
        labels=c( a=1,
                  b=2,
                  c=3,
                  d=4,
                  e=5,
                  f=6))
print(x)
#>  [1] d b c b d a f d e e d f f b d d a b e f


codebook(
    recode(x,
           a = 1 <- 1:2,
           b = 2 <- 4:6))
#> Warning: recoding created 1 NAs
#> ================================================================================
#> 
#>    recode(x, a = 1 <- 1:2, b = 2 <- 4:6)
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels     N Valid Total
#>                                       
#>     1   'a'              6  31.6  30.0
#>     2   'b'             13  68.4  65.0
#>    NA M                  1         5.0
#> 


codebook(
    recode(x,
           a = 1 <- 1:2,
           b = 2 <- 4:6,
           copy = TRUE))
#> ================================================================================
#> 
#>    recode(x, a = 1 <- 1:2, b = 2 <- 4:6, copy = TRUE)
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   6    30.0
#>    2 'b'                  13    65.0
#>    3 'c'                   1     5.0
#> 

# Note the handling of labels if the recoding rules are bijective
codebook(
    recode(x,
           1 <- 2,
           2 <- 1,
           copy=TRUE))
#> ================================================================================
#> 
#>    recode(x, 1 <- 2, 2 <- 1, copy = TRUE)
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'b'                   4    20.0
#>    2 'a'                   2    10.0
#>    3 'c'                   1     5.0
#>    4 'd'                   6    30.0
#>    5 'e'                   3    15.0
#>    6 'f'                   4    20.0
#> 

codebook(
    recode(x,
           a = 1 <- 2,
           b = 2 <- 1,
           copy=TRUE))
#> ================================================================================
#> 
#>    recode(x, a = 1 <- 2, b = 2 <- 1, copy = TRUE)
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   4    20.0
#>    2 'b'                   2    10.0
#>    3 'c'                   1     5.0
#>    4 'd'                   6    30.0
#>    5 'e'                   3    15.0
#>    6 'f'                   4    20.0
#> 


# A recoded version of x is returned
# containing the values 1, 2, 3, which are
# labelled as "A", "B", "C".
recode(x,
  A = 1 <- range(min,2),
  B = 2 <- 3:4,
  C = 3 <- range(5,max), # this last comma is ignored
  )
#> 
#> Item (measurement: nominal, type: integer, length = 20) 
#> 
#>  [1:20] B A B A B A C B C C B C C A B B A A C C

# This causes an error action: the sets
# of original values overlap.
try(recode(x,
  A = 1 <- range(min,2),
  B = 2 <- 2:4,
  C = 3 <- range(5,max)
  ))
#> Error in recode(x, A = 1 <- range(min, 2), B = 2 <- 2:4, C = 3 <- range(5,  : 
#>   recoding request is ambiguous

recode(x,
  A = 1 <- range(min,2),
  B = 2 <- 3:4,
  C = 3 <- range(5,6),
  D = 4 <- 7
  )
#> Warning: recoding 4 <- 7 has no consequences
#> 
#> Item (measurement: nominal, type: integer, length = 20) 
#> 
#>  [1:20] B A B A B A C B C C B C C A B B A A C C
  
# This results in an all-missing vector:
recode(x,
  D = 4 <- 7,
  E = 5 <- 8
  )
#> Warning: recodings 4 <- 7, 5 <- 8 have no consequences
#> Warning: recoding created 20 NAs
#> 
#> Item (measurement: nominal, type: integer, length = 20) 
#> 
#>  [1:20] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA

f <- as.factor(x)
x <- as.integer(x)

recode(x,
  1 <- range(min,2),
  2 <- 3:4,
  3 <- range(5,max)
  )
#>  [1] 2 1 2 1 2 1 3 2 3 3 2 3 3 1 2 2 1 1 3 3

# This causes another error action:
# the third argument is an invalid
# expression for a recoding.
try(recode(x,
  1 <- range(min,2),
  3:4,
  3 <- range(5,max)
  ))
#> Error in recode(x, 1 <- range(min, 2), 3:4, 3 <- range(5, max)) : 
#>   invalid recoding request

# The new values are character strings,
# therefore a factor is returned.
recode(x,
  "a" <- range(min,2),
  "b" <- 3:4,
  "c" <- range(5,6)
  )
#>  [1] b a b a b a c b c c b c c a b b a a c c
#> Levels: a b c
  
recode(x,
  1 <- 1:3,
  2 <- 4:6
  )
#>  [1] 2 1 1 1 2 1 2 2 2 2 2 2 2 1 2 2 1 1 2 2
  
recode(x,
  4 <- 7,
  5 <- 8,
  otherwise = "copy"
  )
#> Warning: recodings 4 <- 7, 5 <- 8 have no consequences
#>  [1] 4 2 3 2 4 1 6 4 5 5 4 6 6 2 4 4 1 2 5 6

recode(f,
  "A" <- c("a","b"),
  "B" <- c("c","d"),
  otherwise="copy"
  )
#>  [1] B A B A B A f B e e B f f A B B A A e f
#> Levels: A B e f

recode(f,
  "A" <- c("a","b"),
  "B" <- c("c","d"),
  otherwise="C"
  )
#>  [1] B A B A B A C B C C B C C A B B A A C C
#> Levels: A B C
 
recode(f,
  "A" <- c("a","b"),
  "B" <- c("c","d")
  )
#> Warning: recoding created 7 NAs
#>  [1] B    A    B    A    B    A    <NA> B    <NA> <NA> B    <NA> <NA> A    B   
#> [16] B    A    A    <NA> <NA>
#> Levels: A B

DS <- data.set(x=as.item(sample(1:6,20,replace=TRUE),
        labels=c( a=1,
                  b=2,
                  c=3,
                  d=4,
                  e=5,
                  f=6)))
print(DS)
#>    x
#>  1 e
#>  2 d
#>  3 a
#>  4 e
#>  5 a
#>  6 c
#>  7 e
#>  8 e
#>  9 d
#> 10 b
#> 11 a
#> 12 f
#> 13 c
#> 14 f
#> 15 a
#> 16 d
#> 17 a
#> 18 a
#> 19 b
#> 20 a

DS <- within(DS,{
    xf <- recode(x,
                 "a" <- range(min,2),
                 "b" <- 3:4,
                 "c" <- range(5,6)
                 )
    xn <- x@.Data
    xc <- recode(xn,
                 "a" <- range(min,2),
                 "b" <- 3:4,
                 "c" <- range(5,6)
                 )
    xc <- as.character(x)
    xcc <- recode(xc,
                  1 <- letters[1:2],
                  2 <- letters[3:4],
                  3 <- letters[5:6]
                  )
})

DS
#> 
#> Data set with 20 observations and 5 variables
#> 
#>    x xf xn xc xcc
#>  1 e  c  5  e   3
#>  2 d  b  4  d   2
#>  3 a  a  1  a   1
#>  4 e  c  5  e   3
#>  5 a  a  1  a   1
#>  6 c  b  3  c   2
#>  7 e  c  5  e   3
#>  8 e  c  5  e   3
#>  9 d  b  4  d   2
#> 10 b  a  2  b   1
#> 11 a  a  1  a   1
#> 12 f  c  6  f   3
#> 13 c  b  3  c   2
#> 14 f  c  6  f   3
#> 15 a  a  1  a   1
#> 16 d  b  4  d   2
#> 17 a  a  1  a   1
#> 18 a  a  1  a   1
#> 19 b  a  2  b   1
#> 20 a  a  1  a   1

DS <- within(DS,{
    xf <- recode(x,
                 "a" <- range(min,2),
                 "b" <- 3:4,
                 "c" <- range(5,6)
                 )
    x1 <- recode(x,
                 1 <- range(1,2),
                 2 <- range(3,4),
                 copy=TRUE
                 )
    xf1 <- recode(x,
                 "A" <- range(1,2),
                 "B" <- range(3,4),
                 copy=TRUE
                 )
})
DS
#> 
#> Data set with 20 observations and 7 variables
#> 
#>    x xf xn xc xcc x1 xf1
#>  1 e  c  5  e   3  e   e
#>  2 d  b  4  d   2  b   B
#>  3 a  a  1  a   1  a   A
#>  4 e  c  5  e   3  e   e
#>  5 a  a  1  a   1  a   A
#>  6 c  b  3  c   2  b   B
#>  7 e  c  5  e   3  e   e
#>  8 e  c  5  e   3  e   e
#>  9 d  b  4  d   2  b   B
#> 10 b  a  2  b   1  a   A
#> 11 a  a  1  a   1  a   A
#> 12 f  c  6  f   3  f   f
#> 13 c  b  3  c   2  b   B
#> 14 f  c  6  f   3  f   f
#> 15 a  a  1  a   1  a   A
#> 16 d  b  4  d   2  b   B
#> 17 a  a  1  a   1  a   A
#> 18 a  a  1  a   1  a   A
#> 19 b  a  2  b   1  a   A
#> 20 a  a  1  a   1  a   A
codebook(DS)
#> ================================================================================
#> 
#>    x
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   7    35.0
#>    2 'b'                   2    10.0
#>    3 'c'                   2    10.0
#>    4 'd'                   3    15.0
#>    5 'e'                   4    20.0
#>    6 'f'                   2    10.0
#> 
#> ================================================================================
#> 
#>    xf
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   9    45.0
#>    2 'b'                   5    25.0
#>    3 'c'                   6    30.0
#> 
#> ================================================================================
#> 
#>    xn
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: interval
#> 
#>         Min: 1.000
#>         Max: 6.000
#>        Mean: 3.050
#>    Std.Dev.: 1.830
#> 
#> ================================================================================
#> 
#>    xc
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: character
#>    Measurement: nominal
#> 
#>    Min: "a"
#>    Max: "f"
#> 
#> ================================================================================
#> 
#>    xcc
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 '1'                   9    45.0
#>    2 '2'                   5    25.0
#>    3 '3'                   6    30.0
#> 
#> ================================================================================
#> 
#>    x1
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   9    45.0
#>    2 'b'                   5    25.0
#>    5 'e'                   4    20.0
#>    6 'f'                   2    10.0
#> 
#> ================================================================================
#> 
#>    xf1
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A'                   9    45.0
#>    2 'B'                   5    25.0
#>    5 'e'                   4    20.0
#>    6 'f'                   2    10.0
#> 

DF <- data.frame(x=rep(1:6,4,replace=TRUE))
DF <- within(DF,{
    xf <- recode(x,
                 "a" <- range(min,2),
                 "b" <- 3:4,
                 "c" <- range(5,6)
                 )
    x1 <- recode(x,
                 1 <- range(1,2),
                 2 <- range(3,4),
                 copy=TRUE
                 )
    xf1 <- recode(x,
                 "A" <- range(1,2),
                 "B" <- range(3,4),
                 copy=TRUE
                 )
    xf2 <- recode(x,
                 "B" <- range(3,4),
                 "A" <- range(1,2),
                 copy=TRUE
                 )
})
DF
#>    x xf2 xf1 x1 xf
#> 1  1   A   A  1  a
#> 2  2   A   A  1  a
#> 3  3   B   B  2  b
#> 4  4   B   B  2  b
#> 5  5   5   5  5  c
#> 6  6   6   6  6  c
#> 7  1   A   A  1  a
#> 8  2   A   A  1  a
#> 9  3   B   B  2  b
#> 10 4   B   B  2  b
#> 11 5   5   5  5  c
#> 12 6   6   6  6  c
#> 13 1   A   A  1  a
#> 14 2   A   A  1  a
#> 15 3   B   B  2  b
#> 16 4   B   B  2  b
#> 17 5   5   5  5  c
#> 18 6   6   6  6  c
#> 19 1   A   A  1  a
#> 20 2   A   A  1  a
#> 21 3   B   B  2  b
#> 22 4   B   B  2  b
#> 23 5   5   5  5  c
#> 24 6   6   6  6  c
codebook(DF)
#> ================================================================================
#> 
#>    x
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#> 
#>         Min: 1.000000
#>         Max: 6.000000
#>        Mean: 3.500000
#>    Std.Dev.: 1.707825
#> 
#> ================================================================================
#> 
#>    xf2
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Factor with 4 levels
#> 
#>    Levels and labels     N Valid
#>                                 
#>    1 'B'                 8  33.3
#>    2 'A'                 8  33.3
#>    3 '5'                 4  16.7
#>    4 '6'                 4  16.7
#> 
#> ================================================================================
#> 
#>    xf1
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Factor with 4 levels
#> 
#>    Levels and labels     N Valid
#>                                 
#>    1 'A'                 8  33.3
#>    2 'B'                 8  33.3
#>    3 '5'                 4  16.7
#>    4 '6'                 4  16.7
#> 
#> ================================================================================
#> 
#>    x1
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#> 
#>         Min: 1.000000
#>         Max: 6.000000
#>        Mean: 2.833333
#>    Std.Dev.: 1.950783
#> 
#> ================================================================================
#> 
#>    xf
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Factor with 3 levels
#> 
#>    Levels and labels     N Valid
#>                                 
#>    1 'a'                 8  33.3
#>    2 'b'                 8  33.3
#>    3 'c'                 8  33.3
#> 
```
