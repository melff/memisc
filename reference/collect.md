# Collect Objects

`collect` gathers several objects into one, matching the elements or
subsets of the objects by [`names`](https://rdrr.io/r/base/names.html)
or [`dimnames`](https://rdrr.io/r/base/dimnames.html).

## Usage

``` r
collect(...,names=NULL,inclusive=TRUE)
# Default S3 method
collect(...,names=NULL,inclusive=TRUE)
# S3 method for class 'array'
collect(...,names=NULL,inclusive=TRUE)
# S3 method for class 'matrix'
collect(...,names=NULL,inclusive=TRUE)
# S3 method for class 'table'
collect(...,names=NULL,sourcename=".origin",fill=0)
# S3 method for class 'data.frame'
collect(...,names=NULL,inclusive=TRUE,
                                  fussy=FALSE,warn=TRUE,
                                  detailed.warnings=FALSE,use.last=FALSE,
                                  sourcename=".origin")
# S3 method for class 'data.set'
collect(...,names=NULL,inclusive=TRUE,
                                  fussy=FALSE,warn=TRUE,
                                  detailed.warnings=FALSE,use.last=FALSE,
                                  sourcename=".origin")
```

## Arguments

- ...:

  more atomic vectors, arrays, matrices, tables, data.frames or
  data.sets

- names:

  optional character vector; in case of the default and array methods,
  giving [`dimnames`](https://rdrr.io/r/base/dimnames.html) for the new
  dimension that identifies the collected objects; in case of the
  data.frame and data.set methods, levels of a factor indentifying the
  collected objects.

- inclusive:

  logical, defaults to TRUE; should unmatched elements included? See
  details below.

- fussy:

  logical, defaults to FALSE; should it count as an error, if variables
  with same names of collected data.frames/data.sets have different
  attributes?

- warn:

  logical, defaults to TRUE; should an warning be given, if variables
  with same names of collected data.frames/data.sets have different
  attributes?

- detailed.warnings:

  logical, whether the attributes of each variable should be printed if
  they differ, and if `warn` or `fuzzy` is TRUE.

- use.last:

  logical, defaults to FALSE. If the function is applied to data frames
  or similar objects, attributes of variables may differ between data
  frames (or other objects, respectively). If this argument is TRUE,
  then the attributes are harmonised based on the variables in the last
  data frame/object, otherwise the attributes of variables in the first
  data frame/object are used for harmonisation.

- sourcename:

  name of the factor that identifies the collected data.frames or
  data.sets

- fill:

  numeric; with what to fill empty table cells, defaults to zero,
  assuming the table contains counts

## Value

If `x` and all following ... arguments are vectors of the same mode
(numeric,character, or logical) the result is a matrix with as many
columns as vectors. If argument `inclusive` is TRUE, then the number of
rows equals the number of names that appear at least once in each of the
vector names and the matrix is filled with `NA` where necessary,
otherwise the number of rows equals the number of names that are present
in *all* vector names.

If `x` and all ... arguments are matrices or arrays of the same mode
(numeric,character, or logical) and \\n\\ dimension the result will be a
\\n+1\\ dimensional array or table. The extend of the \\n+1\\th
dimension equals the number of matrix, array or table arguments, the
extends of the lower dimension depends on the `inclusive` argument:
either they equal to the number of dimnames that appear at least once
for each given dimension and the array is filled with `NA` where
necessary, or they equal to the number of dimnames that appear in all
arguments for each given dimension.

If `x` and all ... arguments are data frames or data sets, the result is
a data frame or data set. The number of variables of the resulting data
frame or data set depends on the `inclusive` argument. If it is true,
the number of variables equals the number of variables that appear in
each of the arguments at least once and variables are filled with `NA`
where necessary, otherwise the number of variables equals the number of
variables that are present in all arguments.

## Examples

``` r
x <- c(a=1,b=2)
y <- c(a=10,c=30)

x
#> a b 
#> 1 2 
y
#>  a  c 
#> 10 30 

collect(x,y)
#>    x  y
#> a  1 10
#> b  2 NA
#> c NA 30
collect(x,y,inclusive=FALSE)
#>   x  y
#> a 1 10

X <- matrix(1,nrow=2,ncol=2,dimnames=list(letters[1:2],LETTERS[1:2]))
Y <- matrix(2,nrow=3,ncol=2,dimnames=list(letters[1:3],LETTERS[1:2]))
Z <- matrix(3,nrow=2,ncol=3,dimnames=list(letters[1:2],LETTERS[1:3]))

X
#>   A B
#> a 1 1
#> b 1 1
Y
#>   A B
#> a 2 2
#> b 2 2
#> c 2 2
Z
#>   A B C
#> a 3 3 3
#> b 3 3 3

collect(X,Y,Z)
#> , , X
#> 
#>    A  B  C
#> a  1  1 NA
#> b  1  1 NA
#> c NA NA NA
#> 
#> , , Y
#> 
#>   A B  C
#> a 2 2 NA
#> b 2 2 NA
#> c 2 2 NA
#> 
#> , , Z
#> 
#>    A  B  C
#> a  3  3  3
#> b  3  3  3
#> c NA NA NA
#> 
collect(X,Y,Z,inclusive=FALSE)
#> , , X
#> 
#>   A B
#> a 1 1
#> b 1 1
#> 
#> , , Y
#> 
#>   A B
#> a 2 2
#> b 2 2
#> 
#> , , Z
#> 
#>   A B
#> a 3 3
#> b 3 3
#> 

X <- matrix(1,nrow=2,ncol=2,dimnames=list(a=letters[1:2],b=LETTERS[1:2]))
Y <- matrix(2,nrow=3,ncol=2,dimnames=list(a=letters[1:3],c=LETTERS[1:2]))
Z <- matrix(3,nrow=2,ncol=3,dimnames=list(a=letters[1:2],c=LETTERS[1:3]))

collect(X,Y,Z)
#> , , X
#> 
#>    A  B  C
#> a  1  1 NA
#> b  1  1 NA
#> c NA NA NA
#> 
#> , , Y
#> 
#>   A B  C
#> a 2 2 NA
#> b 2 2 NA
#> c 2 2 NA
#> 
#> , , Z
#> 
#>    A  B  C
#> a  3  3  3
#> b  3  3  3
#> c NA NA NA
#> 
collect(X,Y,Z,inclusive=FALSE)
#> , , X
#> 
#>   A B
#> a 1 1
#> b 1 1
#> 
#> , , Y
#> 
#>   A B
#> a 2 2
#> b 2 2
#> 
#> , , Z
#> 
#>   A B
#> a 3 3
#> b 3 3
#> 

df1 <- data.frame(a=rep(1,5),b=rep(1,5))
df2 <- data.frame(a=rep(2,5),b=rep(2,5),c=rep(2,5))
collect(df1,df2)
#>    a b  c .origin
#> 1  1 1 NA     df1
#> 2  1 1 NA     df1
#> 3  1 1 NA     df1
#> 4  1 1 NA     df1
#> 5  1 1 NA     df1
#> 6  2 2  2     df2
#> 7  2 2  2     df2
#> 8  2 2  2     df2
#> 9  2 2  2     df2
#> 10 2 2  2     df2
collect(df1,df2,inclusive=FALSE)
#>    a b .origin
#> 1  1 1     df1
#> 2  1 1     df1
#> 3  1 1     df1
#> 4  1 1     df1
#> 5  1 1     df1
#> 6  2 2     df2
#> 7  2 2     df2
#> 8  2 2     df2
#> 9  2 2     df2
#> 10 2 2     df2

data(UCBAdmissions)
Male <- as.table(UCBAdmissions[,1,])
Female <- as.table(UCBAdmissions[,2,])
collect(Male,Female,sourcename="Gender")
#> , , Gender = Male
#> 
#>           Dept
#> Admit        A   B   C   D   E   F
#>   Admitted 512 353 120 138  53  22
#>   Rejected 313 207 205 279 138 351
#> 
#> , , Gender = Female
#> 
#>           Dept
#> Admit        A   B   C   D   E   F
#>   Admitted  89  17 202 131  94  24
#>   Rejected  19   8 391 244 299 317
#> 
collect(unclass(Male),unclass(Female))
#> , , unclass(Male)
#> 
#>            A   B   C   D   E   F
#> Admitted 512 353 120 138  53  22
#> Rejected 313 207 205 279 138 351
#> 
#> , , unclass(Female)
#> 
#>           A  B   C   D   E   F
#> Admitted 89 17 202 131  94  24
#> Rejected 19  8 391 244 299 317
#> 

Male1 <- as.table(UCBAdmissions[,1,-1])
Female2 <- as.table(UCBAdmissions[,2,-2])
Female3 <- as.table(UCBAdmissions[,2,-3])
collect(Male=Male1,Female=Female2,sourcename="Gender")
#> , , Gender = Male
#> 
#>           Dept
#> Admit        B   C   D   E   F   A
#>   Admitted 353 120 138  53  22   0
#>   Rejected 207 205 279 138 351   0
#> 
#> , , Gender = Female
#> 
#>           Dept
#> Admit        B   C   D   E   F   A
#>   Admitted   0 202 131  94  24  89
#>   Rejected   0 391 244 299 317  19
#> 
collect(Male=Male1,Female=Female3,sourcename="Gender")
#> , , Gender = Male
#> 
#>           Dept
#> Admit        B   C   D   E   F   A
#>   Admitted 353 120 138  53  22   0
#>   Rejected 207 205 279 138 351   0
#> 
#> , , Gender = Female
#> 
#>           Dept
#> Admit        B   C   D   E   F   A
#>   Admitted  17   0 131  94  24  89
#>   Rejected   8   0 244 299 317  19
#> 
collect(Male=Male1,Female=Female3,sourcename="Gender",fill=NA)
#> , , Gender = Male
#> 
#>           Dept
#> Admit        B   C   D   E   F A
#>   Admitted 353 120 138  53  22  
#>   Rejected 207 205 279 138 351  
#> 
#> , , Gender = Female
#> 
#>           Dept
#> Admit        B C   D   E   F   A
#>   Admitted  17   131  94  24  89
#>   Rejected   8   244 299 317  19
#> 

f1 <- gl(3,5,labels=letters[1:3])
f2 <- gl(3,6,labels=letters[1:3])
collect(f1=table(f1),f2=table(f2))
#>     .origin
#> Freq f1 f2
#>    a  5  6
#>    b  5  6
#>    c  5  6

ds1 <- data.set(x = 1:3)
ds2 <- data.set(x = 4:9,
                y = 1:6)
collect(ds1,ds2)
#> 
#> Data set with 9 observations and 3 variables
#> 
#>   x  y .origin
#> 1 1 NA     ds1
#> 2 2 NA     ds1
#> 3 3 NA     ds1
#> 4 4  1     ds2
#> 5 5  2     ds2
#> 6 6  3     ds2
#> 7 7  4     ds2
#> 8 8  5     ds2
#> 9 9  6     ds2
```
