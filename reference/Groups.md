# Operate on grouped data in data frames and data sets

`Group` creates a grouped variant of an object of class "data.frame" or
of class "data.set", for which methods for `with` and `within` are
defined, so that these well-known functions can be applied "groupwise".

## Usage

``` r
# Create an object of class "grouped.data" from a
# data frame or a data set.
Groups(data,by,...)
# S3 method for class 'data.frame'
Groups(data,by,...)
# S3 method for class 'data.set'
Groups(data,by,...)
# S3 method for class 'grouped.data'
Groups(data,by,...)

# Recombine grouped data into a data fame or a data set
recombine(x,...)
# S3 method for class 'grouped.data.frame'
recombine(x,...)
# S3 method for class 'grouped.data.set'
recombine(x,...)

# Recombine grouped data and coerce the result appropriately:
# S3 method for class 'grouped.data'
as.data.frame(x,...)
# S4 method for class 'grouped.data.frame'
as.data.set(x,row.names=NULL,...)
# S4 method for class 'grouped.data.set'
as.data.set(x,row.names=NULL,...)

# Methods of the generics "with" and "within" for grouped data
# S3 method for class 'grouped.data'
with(data,expr,...)
# S3 method for class 'grouped.data'
within(data,expr,recombine=FALSE,...)

# This is equivalent to with(Groups(data,by),expr,...)
withGroups(data,by,expr,...)
# This is equivalent to within(Groups(data,by),expr,recombine,...)
withinGroups(data,by,expr,recombine=TRUE,...)
```

## Arguments

- data:

  an object of the classes "data.frame", "data.set" if an argument to
  `Groups`, `withGroups`, `withinGroups`,

- by:

  a formula with the factors the levels of which define the groups.

- expr:

  an expression, or several expressions enclosed in curly braces.

- recombine:

  a logical vector; should the resulting grouped data be recombined?

- x:

  an object of class "grouped.data".

- row.names:

  an optional character vector with row names.

- ...:

  other arguments, ignored.

## Details

When applied to a data frame `Groups` returns an object with class
attributes "grouped.data.frame", "grouped.data", and "data.frame", when
applied do an object with class "data.set", it returns an object with
class attributes "grouped.data.set", "grouped.data", and "data.set".

When applied to objects with class attributed "grouped.data", both the
functions [`with()`](https://rdrr.io/r/base/with.html) amd
[`within()`](https://rdrr.io/r/base/with.html) evaluate `expr`
separately for each group defined by `Groups`.
[`with()`](https://rdrr.io/r/base/with.html) returns an array composed
of the results of `expr`, while
[`within()`](https://rdrr.io/r/base/with.html) returns a modified copy
of its `data` argument, which will be a "grouped.data" object
("grouped.data.frame" or "grouped.data.set"), unless the argument
`recombine=TRUE` is set.

The expression `expr` may contain references to the variables `n_`,
`N_`, and `i_`. `n_` is equal to the size of the respective group (the
number of rows belonging to it), while `N_` is equal to the total number
of observations in all groups. The variable `i_` equals to the indices
of the rows belonging to the respective group of observations.

## Examples

``` r
some.data <- data.frame(x=rnorm(n=100))
some.data <- within(some.data,{
    f <- factor(rep(1:4,each=25),labels=letters[1:4])
    g <- factor(rep(1:5,each=4,5),labels=LETTERS[1:5])
    y <- x + rep(1:4,each=25) +  0.75*rep(1:5,each=4,5)
})

# For demonstration purposes, we create an
# 'empty' group:
some.data <- subset(some.data,
                       f!="a" | g!="C")

some.grouped.data <- Groups(some.data,
                           ~f+g)    

# Computing the means of y for each combination f and g
group.means <- with(some.grouped.data,
                    mean(y))
group.means
#>    g
#> f          A        B        C        D        E
#>   a 1.910285 1.813113       NA 4.628047 4.765704
#>   b 3.163240 3.794269 4.147919 4.571508 5.705042
#>   c 3.497435 3.662945 5.201009 6.433493 7.067344
#>   d 4.165605 6.739396 6.982883 7.306113 8.190686

# Obtaining a groupwise centered variant of y
some.grouped.data <- within(some.grouped.data,{
    y.cent <- y - mean(y)
},recombine=FALSE)

# The groupwise centered variable should have zero mean
# whithin each group
group.means <- with(some.grouped.data,
                    round(mean(y.cent),15))
group.means
#>    g
#> f   A B  C D E
#>   a 0 0 NA 0 0
#>   b 0 0  0 0 0
#>   c 0 0  0 0 0
#>   d 0 0  0 0 0

# The following demonstrates the use of n_, N_, and i_
# An external copy of y
y1 <- some.data$y
group.means.n <- with(some.grouped.data,
                      c(mean(y),  # Group means for y
                        n_,       # Group sizes
                        sum(y)/n_,# Group means for y
                        n_/N_,    # Relative group sizes
                        sum(y1)/N_,# NOT the grand mean
                        sum(y1[i_])/n_)) # Group mean for y1
group.means.n
#> , , g = A
#> 
#>                 f
#>                           a          b          c          d
#>   mean(y)        1.91028489 3.16323983 3.49743466 4.16560478
#>   n_             8.00000000 4.00000000 4.00000000 4.00000000
#>   sum(y)/n_      1.91028489 3.16323983 3.49743466 4.16560478
#>   n_/N_          0.08333333 0.04166667 0.04166667 0.04166667
#>   sum(y1)/N_     4.93633741 4.93633741 4.93633741 4.93633741
#>   sum(y1[i_])/n_ 1.91028489 3.16323983 3.49743466 4.16560478
#> 
#> , , g = B
#> 
#>                 f
#>                           a          b          c          d
#>   mean(y)        1.81311307 3.79426884 3.66294505 6.73939584
#>   n_             5.00000000 7.00000000 4.00000000 4.00000000
#>   sum(y)/n_      1.81311307 3.79426884 3.66294505 6.73939584
#>   n_/N_          0.05208333 0.07291667 0.04166667 0.04166667
#>   sum(y1)/N_     4.93633741 4.93633741 4.93633741 4.93633741
#>   sum(y1[i_])/n_ 1.81311307 3.79426884 3.66294505 6.73939584
#> 
#> , , g = C
#> 
#>                 f
#>                   a        b        c          d
#>   mean(y)        NA 4.147919 5.201009 6.98288284
#>   n_             NA 6.000000 6.000000 4.00000000
#>   sum(y)/n_      NA 4.147919 5.201009 6.98288284
#>   n_/N_          NA 0.062500 0.062500 0.04166667
#>   sum(y1)/N_     NA 4.936337 4.936337 4.93633741
#>   sum(y1[i_])/n_ NA 4.147919 5.201009 6.98288284
#> 
#> , , g = D
#> 
#>                 f
#>                           a          b          c          d
#>   mean(y)        4.62804704 4.57150804 6.43349330 7.30611330
#>   n_             4.00000000 4.00000000 7.00000000 5.00000000
#>   sum(y)/n_      4.62804704 4.57150804 6.43349330 7.30611330
#>   n_/N_          0.04166667 0.04166667 0.07291667 0.05208333
#>   sum(y1)/N_     4.93633741 4.93633741 4.93633741 4.93633741
#>   sum(y1[i_])/n_ 4.62804704 4.57150804 6.43349330 7.30611330
#> 
#> , , g = E
#> 
#>                 f
#>                           a          b          c          d
#>   mean(y)        4.76570424 5.70504209 7.06734352 8.19068598
#>   n_             4.00000000 4.00000000 4.00000000 8.00000000
#>   sum(y)/n_      4.76570424 5.70504209 7.06734352 8.19068598
#>   n_/N_          0.04166667 0.04166667 0.04166667 0.08333333
#>   sum(y1)/N_     4.93633741 4.93633741 4.93633741 4.93633741
#>   sum(y1[i_])/n_ 4.76570424 5.70504209 7.06734352 8.19068598
#> 

# Names can be attached to the groupwise results
with(some.grouped.data,
     c(Centered=round(mean(y.cent),15),
       Uncentered=mean(y)))
#> , , g = A
#> 
#>             f
#>                     a       b        c        d
#>   Centered   0.000000 0.00000 0.000000 0.000000
#>   Uncentered 1.910285 3.16324 3.497435 4.165605
#> 
#> , , g = B
#> 
#>             f
#>                     a        b        c        d
#>   Centered   0.000000 0.000000 0.000000 0.000000
#>   Uncentered 1.813113 3.794269 3.662945 6.739396
#> 
#> , , g = C
#> 
#>             f
#>               a        b        c        d
#>   Centered   NA 0.000000 0.000000 0.000000
#>   Uncentered NA 4.147919 5.201009 6.982883
#> 
#> , , g = D
#> 
#>             f
#>                     a        b        c        d
#>   Centered   0.000000 0.000000 0.000000 0.000000
#>   Uncentered 4.628047 4.571508 6.433493 7.306113
#> 
#> , , g = E
#> 
#>             f
#>                     a        b        c        d
#>   Centered   0.000000 0.000000 0.000000 0.000000
#>   Uncentered 4.765704 5.705042 7.067344 8.190686
#> 

some.data.ungrouped <- recombine(some.grouped.data)
str(some.data.ungrouped)
#> 'data.frame':    96 obs. of  5 variables:
#>  $ x     : num  -0.387 -0.785 -1.057 -0.796 -1.756 ...
#>  $ y     : num  1.363 0.965 0.693 0.954 0.744 ...
#>  $ g     : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 2 2 2 2 4 4 ...
#>  $ f     : Factor w/ 4 levels "a","b","c","d": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ y.cent: num  -0.547 -0.946 -1.217 -0.956 -1.069 ...

# It all works with "data.set" objects
some.dataset <- as.data.set(some.data)
some.grouped.dataset <- Groups(some.dataset,~f+g)

with(some.grouped.dataset,
     c(Mean=mean(y),
       Variance=var(y)))
#> , , g = A
#> 
#>           f
#>                   a        b        c        d
#>   Mean     1.910285 3.163240 3.497435 4.165605
#>   Variance 1.255861 1.378847 1.465161 2.056243
#> 
#> , , g = B
#> 
#>           f
#>                    a         b         c        d
#>   Mean     1.8131131 3.7942688 3.6629450 6.739396
#>   Variance 0.4534572 0.6756092 0.8136362 1.578382
#> 
#> , , g = C
#> 
#>           f
#>             a        b         c         d
#>   Mean     NA 4.147919 5.2010090 6.9828828
#>   Variance NA 1.381988 0.2987582 0.5685829
#> 
#> , , g = D
#> 
#>           f
#>                   a        b         c        d
#>   Mean     4.628047 4.571508 6.4334933 7.306113
#>   Variance 1.177018 0.691624 0.8311554 1.186533
#> 
#> , , g = E
#> 
#>           f
#>                   a         b         c        d
#>   Mean     4.765704 5.7050421 7.0673435 8.190686
#>   Variance 1.409835 0.5370272 0.3285284 1.233791
#> 

# The following two expressions are equivalent:
with(Groups(some.data,~f+g),mean(y))
#>    g
#> f          A        B        C        D        E
#>   a 1.910285 1.813113       NA 4.628047 4.765704
#>   b 3.163240 3.794269 4.147919 4.571508 5.705042
#>   c 3.497435 3.662945 5.201009 6.433493 7.067344
#>   d 4.165605 6.739396 6.982883 7.306113 8.190686
withGroups(some.data,~f+g,mean(y))
#>    g
#> f          A        B        C        D        E
#>   a 1.910285 1.813113       NA 4.628047 4.765704
#>   b 3.163240 3.794269 4.147919 4.571508 5.705042
#>   c 3.497435 3.662945 5.201009 6.433493 7.067344
#>   d 4.165605 6.739396 6.982883 7.306113 8.190686

# The following two expressions are equivalent:
some.data <- within(Groups(some.data,~f+g),{
    y.cent <- y - mean(y)
    y.cent.1 <- y - sum(y)/n_
})

some.data <- withinGroups(some.data,~f+g,{
    y.cent <- y - mean(y)
    y.cent.1 <- y - sum(y)/n_
})

# Both variants of groupwise centred varaibles should
# have zero groupwise means:
withGroups(some.data,~f+g,{
    c(round(mean(y.cent),15),
      round(mean(y.cent.1),15))
})
#> , , g = A
#> 
#>    f
#>     a b c d
#>   1 0 0 0 0
#>   2 0 0 0 0
#> 
#> , , g = B
#> 
#>    f
#>     a b c d
#>   1 0 0 0 0
#>   2 0 0 0 0
#> 
#> , , g = C
#> 
#>    f
#>      a      b      c d
#>   1 NA  0e+00  0e+00 0
#>   2 NA -1e-15 -1e-15 0
#> 
#> , , g = D
#> 
#>    f
#>     a b c     d
#>   1 0 0 0 0e+00
#>   2 0 0 0 1e-15
#> 
#> , , g = E
#> 
#>    f
#>     a b c d
#>   1 0 0 0 0
#>   2 0 0 0 0
#> 
```
