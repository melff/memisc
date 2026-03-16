# Combining flattened tables.

With the method functions described here, flattened (contingency) tables
can be combined into more complex objects, of class `"ftable_matrix"`.
For objects of these class `format` and `print` methods are provided

## Usage

``` r
# S3 method for class 'ftable'
cbind(..., deparse.level=1)

# S3 method for class 'ftable'
rbind(..., deparse.level=1)

# S3 method for class 'ftable_matrix'
cbind(..., deparse.level=1)

# S3 method for class 'ftable_matrix'
rbind(..., deparse.level=1)

# S3 method for class 'ftable_matrix'
format(x,quote=TRUE,digits=0,format="f",...)

# S3 method for class 'ftable_matrix'
Write(x,
                            file = "",
                            quote = TRUE,
                            append = FALSE,
                            digits = 0,
                            ...)
                            
# S3 method for class 'ftable_matrix'
print(x,quote=FALSE,...)
```

## Arguments

- ...:

  for `cbind` and `rbind` methods, two or more objects of class
  `"ftable"` or `"ftable_matrix"`; for the other methods: further
  arguments, ignored.

- deparse.level:

  ignored, retained for compatibility reasons only.

- x:

  an object used to select a method.

- quote:

  logical, indicating whether or not strings should be printed with
  surrounding quotes.

- digits:

  numeric or integer, number of significant digits to be shown.

- format:

  a format string as in [`formatC`](https://rdrr.io/r/base/formatc.html)

- file:

  character string, containing a file path.

- append:

  logical, should the output appended to the file?

## Value

`cbind` and `rbind`, when used with `"ftable"` or `"ftable_matrix"`
objects, return objects of class `"ftable_matrix"`.

## Examples

``` r
ft1 <- ftable(Sex~Survived,Titanic)
ft2 <- ftable(Age+Class~Survived,Titanic)
ft3 <- ftable(Survived~Class,Titanic)
ft4 <- ftable(Survived~Age,Titanic)
ft5 <- ftable(Survived~Sex,Titanic)

tab10 <- xtabs(Freq~Survived,Titanic)

(c12.10 <- cbind(ft1,ft2,Total=tab10))
#>                           Age   Child              Adult                      
#>          Sex Male Female  Class   1st 2nd 3rd Crew   1st 2nd 3rd Crew   Total 
#> Survived                                                                      
#> No           1364    126            0   0  52    0   122 167 476  673    1490 
#> Yes           367    344            6  24  27    0   197  94 151  212     711 
#>                                                                               
(r345.10 <- rbind(ft3,ft4,ft5,Total=tab10))
#>        Survived   No Yes 
#> Class                    
#> 1st              122 203 
#> 2nd              167 118 
#> 3rd              528 178 
#> Crew             673 212 
#>                          
#> Age                      
#> Child             52  57 
#> Adult           1438 654 
#>                          
#> Sex                      
#> Male            1364 367 
#> Female           126 344 
#>                          
#>                          
#> Total           1490 711 
#>                          

if (FALSE) { # \dontrun{
tf <- tempfile()
Write(c12.10,file=tf)
file.show(tf)
} # }
```
