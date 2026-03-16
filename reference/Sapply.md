# A Dimension Preserving Variant of "sapply" and "lapply"

`Sapply` is equivalent to
[`sapply`](https://rdrr.io/r/base/lapply.html), except that it preserves
the dimension and dimension names of the argument `X`. It also preserves
the dimension of results of the function `FUN`. It is intended for
application to results e.g. of a call to
[`by`](https://rdrr.io/r/base/by.html). `Lapply` is an analog to
`lapply` insofar as it does not try to simplify the resulting `list` of
results of `FUN`.

## Usage

``` r
Sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
Lapply(X, FUN, ...)
```

## Arguments

- X:

  a vector or list appropriate to a call to `sapply`.

- FUN:

  a function.

- ...:

  optional arguments to `FUN`.

- simplify:

  a logical value; should the result be simplified to a vector or matrix
  if possible?

- USE.NAMES:

  logical; if `TRUE` and if `X` is character, use `X` as names for the
  result unless it had names already.

## Value

If `FUN` returns a scalar, then the result has the same dimension as
`X`, otherwise the dimension of the result is enhanced relative to `X`.

## Examples

``` r
berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
berktest1 <- By(~Dept+Gender,
                glm(cbind(Admitted,Rejected)~1,family="binomial"),
                data=berkeley)
berktest2 <- By(~Dept,
                glm(cbind(Admitted,Rejected)~Gender,family="binomial"),
                data=berkeley)

sapply(berktest1,coef)
#> (Intercept) (Intercept) (Intercept) (Intercept) (Intercept) (Intercept) 
#>   0.4921214   0.5337493  -0.5355182  -0.7039581  -0.9569618  -2.7697438 
#> (Intercept) (Intercept) (Intercept) (Intercept) (Intercept) (Intercept) 
#>   1.5441974   0.7537718  -0.6604399  -0.6219709  -1.1571488  -2.5808479 
Sapply(berktest1,coef)
#>     Gender
#> Dept       Male     Female
#>    A  0.4921214  1.5441974
#>    B  0.5337493  0.7537718
#>    C -0.5355182 -0.6604399
#>    D -0.7039581 -0.6219709
#>    E -0.9569618 -1.1571488
#>    F -2.7697438 -2.5808479

sapply(berktest1,function(x)drop(coef(summary(x))))
#>                    [,1]         [,2]          [,3]          [,4]          [,5]
#> Estimate   4.921214e-01 5.337493e-01 -5.355182e-01 -7.039581e-01 -9.569618e-01
#> Std. Error 7.174966e-02 8.754301e-02  1.149408e-01  1.040702e-01  1.615992e-01
#> z value    6.858868e+00 6.096995e+00 -4.659080e+00 -6.764263e+00 -5.921822e+00
#> Pr(>|z|)   6.940823e-12 1.080812e-09  3.176259e-06  1.339898e-11  3.183932e-09
#>                     [,6]         [,7]       [,8]          [,9]         [,10]
#> Estimate   -2.769744e+00 1.544197e+00 0.75377180 -6.604399e-01 -6.219709e-01
#> Std. Error  2.197807e-01 2.527203e-01 0.42874646  8.664894e-02  1.083141e-01
#> z value    -1.260231e+01 6.110303e+00 1.75808285 -7.622019e+00 -5.742289e+00
#> Pr(>|z|)    2.050557e-36 9.944221e-10 0.07873341  2.497388e-14  9.340538e-09
#>                    [,11]         [,12]
#> Estimate   -1.157149e+00 -2.580848e+00
#> Std. Error  1.182487e-01  2.117103e-01
#> z value    -9.785721e+00 -1.219047e+01
#> Pr(>|z|)    1.296674e-22  3.493965e-34
Sapply(berktest1,function(x)drop(coef(summary(x))))
#> , , Gender = Male
#> 
#>             Dept
#>                         A            B             C             D
#>   Estimate   4.921214e-01 5.337493e-01 -5.355182e-01 -7.039581e-01
#>   Std. Error 7.174966e-02 8.754301e-02  1.149408e-01  1.040702e-01
#>   z value    6.858868e+00 6.096995e+00 -4.659080e+00 -6.764263e+00
#>   Pr(>|z|)   6.940823e-12 1.080812e-09  3.176259e-06  1.339898e-11
#>             Dept
#>                          E             F
#>   Estimate   -9.569618e-01 -2.769744e+00
#>   Std. Error  1.615992e-01  2.197807e-01
#>   z value    -5.921822e+00 -1.260231e+01
#>   Pr(>|z|)    3.183932e-09  2.050557e-36
#> 
#> , , Gender = Female
#> 
#>             Dept
#>                         A          B             C             D             E
#>   Estimate   1.544197e+00 0.75377180 -6.604399e-01 -6.219709e-01 -1.157149e+00
#>   Std. Error 2.527203e-01 0.42874646  8.664894e-02  1.083141e-01  1.182487e-01
#>   z value    6.110303e+00 1.75808285 -7.622019e+00 -5.742289e+00 -9.785721e+00
#>   Pr(>|z|)   9.944221e-10 0.07873341  2.497388e-14  9.340538e-09  1.296674e-22
#>             Dept
#>                          F
#>   Estimate   -2.580848e+00
#>   Std. Error  2.117103e-01
#>   z value    -1.219047e+01
#>   Pr(>|z|)    3.493965e-34
#> 

sapply(berktest2,coef)
#>                      A         B          C           D          E          F
#> (Intercept)  0.4921214 0.5337493 -0.5355182 -0.70395810 -0.9569618 -2.7697438
#> GenderFemale 1.0520760 0.2200225 -0.1249216  0.08198719 -0.2001870  0.1888958
Sapply(berktest2,coef)
#>               Dept
#>                        A         B          C           D          E          F
#>   (Intercept)  0.4921214 0.5337493 -0.5355182 -0.70395810 -0.9569618 -2.7697438
#>   GenderFemale 1.0520760 0.2200225 -0.1249216  0.08198719 -0.2001870  0.1888958
sapply(berktest2,function(x)coef(summary(x)))
#>                 A            B             C             D             E
#> [1,] 4.921214e-01 5.337493e-01 -5.355182e-01 -7.039581e-01 -9.569618e-01
#> [2,] 1.052076e+00 2.200225e-01 -1.249216e-01  8.198719e-02 -2.001870e-01
#> [3,] 7.174966e-02 8.754301e-02  1.149408e-01  1.040702e-01  1.615992e-01
#> [4,] 2.627081e-01 4.375926e-01  1.439424e-01  1.502084e-01  2.002426e-01
#> [5,] 6.858868e+00 6.096994e+00 -4.659080e+00 -6.764263e+00 -5.921822e+00
#> [6,] 4.004734e+00 5.028022e-01 -8.678583e-01  5.458231e-01 -9.997227e-01
#> [7,] 6.940825e-12 1.080813e-09  3.176259e-06  1.339898e-11  3.183932e-09
#> [8,] 6.208742e-05 6.151033e-01  3.854719e-01  5.851875e-01  3.174447e-01
#>                  F
#> [1,] -2.769744e+00
#> [2,]  1.888958e-01
#> [3,]  2.197807e-01
#> [4,]  3.051635e-01
#> [5,] -1.260231e+01
#> [6,]  6.189987e-01
#> [7,]  2.050557e-36
#> [8,]  5.359172e-01
Sapply(berktest2,function(x)coef(summary(x)))
#> , , Dept = A
#> 
#>               
#>                 Estimate Std. Error  z value     Pr(>|z|)
#>   (Intercept)  0.4921214 0.07174966 6.858868 6.940825e-12
#>   GenderFemale 1.0520760 0.26270810 4.004734 6.208742e-05
#> 
#> , , Dept = B
#> 
#>               
#>                 Estimate Std. Error   z value     Pr(>|z|)
#>   (Intercept)  0.5337493 0.08754301 6.0969945 1.080813e-09
#>   GenderFemale 0.2200225 0.43759263 0.5028022 6.151033e-01
#> 
#> , , Dept = C
#> 
#>               
#>                  Estimate Std. Error    z value     Pr(>|z|)
#>   (Intercept)  -0.5355182  0.1149408 -4.6590799 3.176259e-06
#>   GenderFemale -0.1249216  0.1439424 -0.8678583 3.854719e-01
#> 
#> , , Dept = D
#> 
#>               
#>                   Estimate Std. Error    z value     Pr(>|z|)
#>   (Intercept)  -0.70395810  0.1040702 -6.7642627 1.339898e-11
#>   GenderFemale  0.08198719  0.1502084  0.5458231 5.851875e-01
#> 
#> , , Dept = E
#> 
#>               
#>                  Estimate Std. Error    z value     Pr(>|z|)
#>   (Intercept)  -0.9569618  0.1615992 -5.9218225 3.183932e-09
#>   GenderFemale -0.2001870  0.2002426 -0.9997227 3.174447e-01
#> 
#> , , Dept = F
#> 
#>               
#>                  Estimate Std. Error     z value     Pr(>|z|)
#>   (Intercept)  -2.7697438  0.2197807 -12.6023077 2.050557e-36
#>   GenderFemale  0.1888958  0.3051635   0.6189987 5.359172e-01
#> 
```
