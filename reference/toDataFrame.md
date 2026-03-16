# Convert an Array into a Data Frame

`to.data.frame` converts an array into a data frame, in such a way that
a chosen dimensional extent forms variables in the data frame. The
elements of the array must be either atomic, data frames with matching
variables, or coercable into such data frames.

## Usage

``` r
to.data.frame(X,as.vars=1,name="Freq")
```

## Arguments

- X:

  an array.

- as.vars:

  a numeric value or a character string. If it is a numeric value then
  it indicates the dimensional extend which defines the variables. If it
  is a character string then it is matched against the names of the
  dimensional extents. This is applicable e.g. if `X` is a contingency
  table and the dimensional extents are named after the cross-classified
  factors. Takes effect only if `X` is an atomic array. If `as.vars`
  equals zero, a new variable is created that contains the values of the
  array, that is, `to.data.frame` acts on the array `X` like
  `as.data.frame(as.table(X))`

- name:

  a character string; the name of the variable created if `X` is an
  atomic array and `as.vars` equals zero.

## Value

A data frame.

## Examples

``` r
berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
berktest1 <- By(~Dept+Gender,
                glm(cbind(Admitted,Rejected)~1,family="binomial"),
                data=berkeley)
berktest2 <- By(~Dept,
                glm(cbind(Admitted,Rejected)~Gender,family="binomial"),
                data=berkeley)
Stest1 <- Lapply(berktest2,function(x)predict(x,,se.fit=TRUE)[c("fit","se.fit")])
Stest2 <- Sapply(berktest2,function(x)coef(summary(x)))
Stest2.1 <- Lapply(berktest1,function(x)predict(x,,se.fit=TRUE)[c("fit","se.fit")])
to.data.frame(Stest1)
#>    Dept        fit     se.fit
#> 1     A  0.4921214 0.07174966
#> 2     A  1.5441974 0.25272027
#> 3     B  0.5337493 0.08754301
#> 4     B  0.7537718 0.42874646
#> 5     C -0.5355182 0.11494077
#> 6     C -0.6604399 0.08664894
#> 7     D -0.7039581 0.10407019
#> 8     D -0.6219709 0.10831411
#> 9     E -0.9569618 0.16159920
#> 10    E -1.1571488 0.11824880
#> 11    F -2.7697438 0.21978068
#> 12    F -2.5808479 0.21171027
to.data.frame(Stest2,as.vars=2)
#>            Var1 Dept    Estimate Std. Error     z value     Pr(>|z|)
#> 1   (Intercept)    A  0.49212143 0.07174966   6.8588682 6.940825e-12
#> 2  GenderFemale    A  1.05207596 0.26270810   4.0047336 6.208742e-05
#> 3   (Intercept)    B  0.53374926 0.08754301   6.0969945 1.080813e-09
#> 4  GenderFemale    B  0.22002254 0.43759263   0.5028022 6.151033e-01
#> 5   (Intercept)    C -0.53551824 0.11494077  -4.6590799 3.176259e-06
#> 6  GenderFemale    C -0.12492163 0.14394242  -0.8678583 3.854719e-01
#> 7   (Intercept)    D -0.70395810 0.10407019  -6.7642627 1.339898e-11
#> 8  GenderFemale    D  0.08198719 0.15020836   0.5458231 5.851875e-01
#> 9   (Intercept)    E -0.95696177 0.16159920  -5.9218225 3.183932e-09
#> 10 GenderFemale    E -0.20018702 0.20024255  -0.9997227 3.174447e-01
#> 11  (Intercept)    F -2.76974377 0.21978068 -12.6023077 2.050557e-36
#> 12 GenderFemale    F  0.18889583 0.30516354   0.6189987 5.359172e-01
to.data.frame(Stest2.1)
#>    Dept Gender        fit     se.fit
#> 1     A   Male  0.4921214 0.07174966
#> 2     B   Male  0.5337493 0.08754301
#> 3     C   Male -0.5355182 0.11494077
#> 4     D   Male -0.7039581 0.10407019
#> 5     E   Male -0.9569618 0.16159920
#> 6     F   Male -2.7697438 0.21978068
#> 7     A Female  1.5441974 0.25272027
#> 8     B Female  0.7537718 0.42874646
#> 9     C Female -0.6604399 0.08664894
#> 10    D Female -0.6219709 0.10831411
#> 11    E Female -1.1571488 0.11824870
#> 12    F Female -2.5808479 0.21171027
# Recasting a contingency table
to.data.frame(UCBAdmissions,as.vars="Admit")
#>    Gender Dept Admitted Rejected
#> 1    Male    A      512      313
#> 2  Female    A       89       19
#> 3    Male    B      353      207
#> 4  Female    B       17        8
#> 5    Male    C      120      205
#> 6  Female    C      202      391
#> 7    Male    D      138      279
#> 8  Female    D      131      244
#> 9    Male    E       53      138
#> 10 Female    E       94      299
#> 11   Male    F       22      351
#> 12 Female    F       24      317
```
