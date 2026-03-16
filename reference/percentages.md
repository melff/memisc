# Easy Creation of Tables of Percentages

The generic function `percentages` and its methods create one- or
multidimensional tables of percentages. As such, the function
`percentages` can be viewed as a convenience interface to
[`prop.table`](https://rdrr.io/r/base/proportions.html). However, it
also allows to obtain standard errors and confidence intervals.

## Usage

``` r
percentages(obj, ...)
# S3 method for class 'table'
percentages(obj,
      by=NULL, which=NULL, se=FALSE, ci=FALSE, ci.level=.95, ...)
# S3 method for class 'formula'
percentages(obj,
      data=parent.frame(), weights=NULL, ...)
# Default S3 method
percentages(obj,
      weights=NULL, ...)
# S3 method for class 'data.frame'
percentages(obj,
      weights=NULL, ...)
# S3 method for class 'list'
percentages(obj,
      weights=NULL, ...)
# S3 method for class 'percentage.table'
as.data.frame(x, ...)
# S3 method for class 'xpercentage.table'
as.data.frame(x, ...)
```

## Arguments

- obj:

  an object; a contingency table or a formula. If it is a formula, its
  left-hand side determines the factor or combination of factors for
  which percentages are computed while its right-hand side determines
  the factor or combination of factors that define the groups within
  which percentages are computed.

- by:

  a character vector with the names of the factor variables that define
  the groups within which percentages are computed. Percentages sum to
  100 within combination of levels of these factors.

- which:

  a character vector with the names of the factor variables for which
  percentages are computed.

- se:

  a logical value; determines whether standard errors are computed.

- ci:

  a logical value; determines whether confidence intervals are computed.
  Note that the confidence intervals are for infinite (or very large)
  populations.

- ci.level:

  a numerical value, the required confidence level of the confidence
  intervals.

- data:

  a contingency table (an object that inherits from "table") or a data
  frame or an object coercable into a data frame.

- weights:

  an optional vector of weights. Should be NULL or a numeric vector.

- ...:

  Further arguments passed on to the "table" method of `percentages` or
  ignored in case of a call to `as.data.frame`.

- x:

  an object coerced into a data frame.

## Value

An array that inherits classes "percentage.table" and "table". If
`percentages` was called with `se=TRUE` or `ci=TRUE` then the result
additionally inherits class "xpercentage.table".

## Examples

``` r
percentages(UCBAdmissions)
#> , , Dept = A
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted 11.3124171  1.9664163
#>   Rejected  6.9155988  0.4197967
#> 
#> , , Dept = B
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted  7.7993814  0.3756076
#>   Rejected  4.5735749  0.1767565
#> 
#> , , Dept = C
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted  2.6513478  4.4631021
#>   Rejected  4.5293858  8.6389748
#> 
#> , , Dept = D
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted  3.0490499  2.8943880
#>   Rejected  6.1643836  5.3910738
#> 
#> , , Dept = E
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted  1.1710119  2.0768891
#>   Rejected  3.0490499  6.6062749
#> 
#> , , Dept = F
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted  0.4860804  0.5302696
#>   Rejected  7.7551922  7.0039770
#> 

# Three equivalent ways to create the same table of conditional
# percentages
percentages(Admit~Gender+Dept,data=UCBAdmissions)
#> , , Dept = A
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 62.060606 82.407407
#>   Rejected 37.939394 17.592593
#> 
#> , , Dept = B
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 63.035714 68.000000
#>   Rejected 36.964286 32.000000
#> 
#> , , Dept = C
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 36.923077 34.064081
#>   Rejected 63.076923 65.935919
#> 
#> , , Dept = D
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 33.093525 34.933333
#>   Rejected 66.906475 65.066667
#> 
#> , , Dept = E
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 27.748691 23.918575
#>   Rejected 72.251309 76.081425
#> 
#> , , Dept = F
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted  5.898123  7.038123
#>   Rejected 94.101877 92.961877
#> 
percentages(UCBAdmissions,by=c("Gender","Dept"))
#> , , Dept = A
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 62.060606 82.407407
#>   Rejected 37.939394 17.592593
#> 
#> , , Dept = B
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 63.035714 68.000000
#>   Rejected 36.964286 32.000000
#> 
#> , , Dept = C
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 36.923077 34.064081
#>   Rejected 63.076923 65.935919
#> 
#> , , Dept = D
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 33.093525 34.933333
#>   Rejected 66.906475 65.066667
#> 
#> , , Dept = E
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 27.748691 23.918575
#>   Rejected 72.251309 76.081425
#> 
#> , , Dept = F
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted  5.898123  7.038123
#>   Rejected 94.101877 92.961877
#> 
percentages(UCBAdmissions,which="Admit")
#> , , Dept = A
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 62.060606 82.407407
#>   Rejected 37.939394 17.592593
#> 
#> , , Dept = B
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 63.035714 68.000000
#>   Rejected 36.964286 32.000000
#> 
#> , , Dept = C
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 36.923077 34.064081
#>   Rejected 63.076923 65.935919
#> 
#> , , Dept = D
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 33.093525 34.933333
#>   Rejected 66.906475 65.066667
#> 
#> , , Dept = E
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted 27.748691 23.918575
#>   Rejected 72.251309 76.081425
#> 
#> , , Dept = F
#> 
#>           Gender
#> Admit           Male    Female
#>   Admitted  5.898123  7.038123
#>   Rejected 94.101877 92.961877
#> 
# Percentage table as data frame
as.data.frame(percentages(Admit~Gender+Dept,data=UCBAdmissions))
#>       Admit Gender Dept Percentage
#> 1  Admitted   Male    A  62.060606
#> 2  Rejected   Male    A  37.939394
#> 3  Admitted Female    A  82.407407
#> 4  Rejected Female    A  17.592593
#> 5  Admitted   Male    B  63.035714
#> 6  Rejected   Male    B  36.964286
#> 7  Admitted Female    B  68.000000
#> 8  Rejected Female    B  32.000000
#> 9  Admitted   Male    C  36.923077
#> 10 Rejected   Male    C  63.076923
#> 11 Admitted Female    C  34.064081
#> 12 Rejected Female    C  65.935919
#> 13 Admitted   Male    D  33.093525
#> 14 Rejected   Male    D  66.906475
#> 15 Admitted Female    D  34.933333
#> 16 Rejected Female    D  65.066667
#> 17 Admitted   Male    E  27.748691
#> 18 Rejected   Male    E  72.251309
#> 19 Admitted Female    E  23.918575
#> 20 Rejected Female    E  76.081425
#> 21 Admitted   Male    F   5.898123
#> 22 Rejected   Male    F  94.101877
#> 23 Admitted Female    F   7.038123
#> 24 Rejected Female    F  92.961877

# Standard errors and confidence intervals
percentages(Admit~Dept,data=UCBAdmissions,se=TRUE)
#> , , Result = Percentage
#> 
#>           Dept
#> Admit               A          B          C          D          E          F
#>   Admitted 64.4158628 63.2478632 35.0762527 33.9646465 25.1712329  6.4425770
#>   Rejected 35.5841372 36.7521368 64.9237473 66.0353535 74.8287671 93.5574230
#> 
#> , , Result = SE
#> 
#>           Dept
#> Admit               A          B          C          D          E          F
#>   Admitted  1.5674143  1.9933625  1.5750235  1.6828260  1.7958916  0.9187976
#>   Rejected  1.5674143  1.9933625  1.5750235  1.6828260  1.7958916  0.9187976
#> 
percentages(Admit~Dept,data=UCBAdmissions,ci=TRUE)
#> , , Result = Percentage
#> 
#>           Dept
#> Admit              A         B         C         D         E         F
#>   Admitted 64.415863 63.247863 35.076253 33.964646 25.171233  6.442577
#>   Rejected 35.584137 36.752137 64.923747 66.035354 74.828767 93.557423
#> 
#> , , Result = Lower bound
#> 
#>           Dept
#> Admit              A         B         C         D         E         F
#>   Admitted 61.248477 59.196386 31.986890 30.668157 21.698820  4.755024
#>   Rejected 32.508100 32.835173 61.738250 62.618938 71.103049 91.499717
#> 
#> , , Result = Upper bound
#> 
#>           Dept
#> Admit              A         B         C         D         E         F
#>   Admitted 67.491900 67.164827 38.261750 37.381062 28.896951  8.500283
#>   Rejected 38.751523 40.803614 68.013110 69.331843 78.301180 95.244976
#> 
(p<- percentages(Admit~Dept,data=UCBAdmissions,ci=TRUE,se=TRUE))
#> , , Result = Percentage
#> 
#>           Dept
#> Admit               A          B          C          D          E          F
#>   Admitted 64.4158628 63.2478632 35.0762527 33.9646465 25.1712329  6.4425770
#>   Rejected 35.5841372 36.7521368 64.9237473 66.0353535 74.8287671 93.5574230
#> 
#> , , Result = SE
#> 
#>           Dept
#> Admit               A          B          C          D          E          F
#>   Admitted  1.5674143  1.9933625  1.5750235  1.6828260  1.7958916  0.9187976
#>   Rejected  1.5674143  1.9933625  1.5750235  1.6828260  1.7958916  0.9187976
#> 
#> , , Result = Lower bound
#> 
#>           Dept
#> Admit               A          B          C          D          E          F
#>   Admitted 61.2484768 59.1963862 31.9868896 30.6681566 21.6988202  4.7550241
#>   Rejected 32.5081003 32.8351731 61.7382497 62.6189380 71.1030491 91.4997166
#> 
#> , , Result = Upper bound
#> 
#>           Dept
#> Admit               A          B          C          D          E          F
#>   Admitted 67.4918997 67.1648269 38.2617503 37.3810620 28.8969509  8.5002834
#>   Rejected 38.7515232 40.8036138 68.0131104 69.3318434 78.3011798 95.2449759
#> 

# An extended table of percentages as data frame
as.data.frame(p)
#>       Admit Dept Percentage        SE Lower bound Upper bound
#> 1  Admitted    A  64.415863 1.5674143   61.248477   67.491900
#> 2  Admitted    B  63.247863 1.9933625   59.196386   67.164827
#> 3  Admitted    C  35.076253 1.5750235   31.986890   38.261750
#> 4  Admitted    D  33.964646 1.6828260   30.668157   37.381062
#> 5  Admitted    E  25.171233 1.7958916   21.698820   28.896951
#> 6  Admitted    F   6.442577 0.9187976    4.755024    8.500283
#> 7  Rejected    A  35.584137 1.5674143   32.508100   38.751523
#> 8  Rejected    B  36.752137 1.9933625   32.835173   40.803614
#> 9  Rejected    C  64.923747 1.5750235   61.738250   68.013110
#> 10 Rejected    D  66.035354 1.6828260   62.618938   69.331843
#> 11 Rejected    E  74.828767 1.7958916   71.103049   78.301180
#> 12 Rejected    F  93.557423 0.9187976   91.499717   95.244976

# A table of percentages of a factor
percentages(iris$Species)
#> iris$Species
#>     setosa versicolor  virginica 
#>   33.33333   33.33333   33.33333 

UCBA <- as.data.frame(UCBAdmissions)
percentages(UCBA$Admit,weights=UCBA$Freq)
#> UCBA$Admit
#> Admitted Rejected 
#> 38.77596 61.22404 

percentages(UCBA,weights=UCBA$Freq)
#> $Admit
#> Admit
#> Admitted Rejected 
#> 38.77596 61.22404 
#> 
#> $Gender
#> Gender
#>     Male   Female 
#> 59.45647 40.54353 
#> 
#> $Dept
#> Dept
#>        A        B        C        D        E        F 
#> 20.61423 12.92532 20.28281 17.49890 12.90323 15.77552 
#> 
#> $Freq
#> Freq
#>          8         17         19         22         24         53         89 
#>  0.1767565  0.3756076  0.4197967  0.4860804  0.5302696  1.1710119  1.9664163 
#>         94        120        131        138        202        205        207 
#>  2.0768891  2.6513478  2.8943880  6.0980999  4.4631021  4.5293858  4.5735749 
#>        244        279        299        313        317        351        353 
#>  5.3910738  6.1643836  6.6062749  6.9155988  7.0039770  7.7551922  7.7993814 
#>        391        512 
#>  8.6389748 11.3124171 
#> 

# Tables with NAs
UCBA_N <- UCBA; UCBA_N[cbind(6:9, c(1:2,4,1))] <- NA

with(UCBA_N, percentages(Gender))
#> Gender
#>     Male   Female 
#> 52.17391 47.82609 
with(UCBA_N, percentages(Gender, useNA = "always"))
#> Gender
#>      Male    Female      <NA> 
#> 50.000000 45.833333  4.166667 
with(UCBA_N, percentages(Gender, weights=Freq))
#> Gender
#>     Male   Female 
#> 59.78671 40.21329 
with(UCBA_N, percentages(Gender, weights=Freq, useNA = "always"))
#> Gender
#>       Male     Female       <NA> 
#> 59.5617530 40.0619743  0.3762727 
with(UCBA_N, percentages(Gender, weights=Freq, useNA = "ifany"))
#> Gender
#>       Male     Female       <NA> 
#> 59.5617530 40.0619743  0.3762727 
with(UCBA, percentages(Gender, weights=Freq))
#> Gender
#>     Male   Female 
#> 59.45647 40.54353 
with(UCBA, percentages(Gender, weights=Freq, useNA = "always"))
#> Gender
#>     Male   Female     <NA> 
#> 59.45647 40.54353  0.00000 

percentages(UCBA,weights=UCBA$Freq, useNA = "always")
#> $Admit
#> Admit
#> Admitted Rejected     <NA> 
#> 38.77596 61.22404  0.00000 
#> 
#> $Gender
#> Gender
#>     Male   Female     <NA> 
#> 59.45647 40.54353  0.00000 
#> 
#> $Dept
#> Dept
#>        A        B        C        D        E        F     <NA> 
#> 20.61423 12.92532 20.28281 17.49890 12.90323 15.77552  0.00000 
#> 
#> $Freq
#> Freq
#>          8         17         19         22         24         53         89 
#>  0.1767565  0.3756076  0.4197967  0.4860804  0.5302696  1.1710119  1.9664163 
#>         94        120        131        138        202        205        207 
#>  2.0768891  2.6513478  2.8943880  6.0980999  4.4631021  4.5293858  4.5735749 
#>        244        279        299        313        317        351        353 
#>  5.3910738  6.1643836  6.6062749  6.9155988  7.0039770  7.7551922  7.7993814 
#>        391        512       <NA> 
#>  8.6389748 11.3124171  0.0000000 
#> 
```
