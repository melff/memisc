# Table of Percentages with Percentage Base

`percent` returns a table of percentages along with the percentage base.
It will be useful in conjunction with
[`Aggregate`](https://melff.github.io/memisc/reference/genTable.md) or
[`genTable`](https://melff.github.io/memisc/reference/genTable.md).

## Usage

``` r
percent(x,...)
  # Default S3 method
percent(x,weights=NULL,total=!(se || ci),
      se=FALSE,ci=FALSE,ci.level=.95,
      total.name="N",perc.label="Percentage",...)
  # S3 method for class 'logical'
percent(x,weights=NULL,total=!(se || ci),
      se=FALSE,ci=FALSE,ci.level=.95,
      total.name="N",perc.label="Percentage",...)
```

## Arguments

- x:

  a numeric vector or factor.

- weights:

  a optional numeric vector of weights of the same length as `x`.

- total:

  logical; should the total sum of counts from which the percentages are
  computed be included into the output?

- se:

  logical; should standard errors of the percentages be included?

- ci:

  logical; should confidence intervals of the percentages be included?

- ci.level:

  numeric; nominal coverage of confidence intervals

- total.name:

  character; name given for the total sum of counts

- perc.label:

  character; label given for the percentages if the table has more than
  one dimensions, e.g. if `se` or `ci` is TRUE.

- ...:

  for `percent.mresp`: one or several 1-0 vectors or matrices otherwise,
  further arguments, currently ignored.

## Value

A table of percentages.

## Examples

``` r
x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
f <- sample(1:3,100,replace=TRUE)
f <- factor(f,labels=c("a","b","c"))


percent(x>0)
#> Percentage          N 
#>         44        100 
percent(f)
#>   a   b   c   N 
#>  39  36  25 100 

genTable(
  cbind(percent(x>0),
        percent(y>0),
        percent(z>0)) ~ f
  )
#> , , f = a
#> 
#>             
#>              percent(x > 0) percent(y > 0) percent(z > 0)
#>   Percentage       33.33333       38.46154       51.28205
#>   N                39.00000       39.00000       39.00000
#> 
#> , , f = b
#> 
#>             
#>              percent(x > 0) percent(y > 0) percent(z > 0)
#>   Percentage       55.55556       47.22222       55.55556
#>   N                36.00000       36.00000       36.00000
#> 
#> , , f = c
#> 
#>             
#>              percent(x > 0) percent(y > 0) percent(z > 0)
#>   Percentage             44             60             44
#>   N                      25             25             25
#> 

gt <- genTable(
  cbind("x > 0" = percent(x>0,ci=TRUE),
        "y > 0" = percent(y>0,ci=TRUE),
        "z > 0" = percent(z>0,ci=TRUE)) ~ f
  )

ftable(gt,row.vars=3:2,col.vars=1)
#>          Percentage    lower    upper
#> f                                    
#> a x > 0    33.33333 19.08810 50.21723
#>   y > 0    38.46154 23.36393 55.38094
#>   z > 0    51.28205 34.78022 67.58192
#> b x > 0    55.55556 38.09768 72.06458
#>   y > 0    47.22222 30.40506 64.51362
#>   z > 0    55.55556 38.09768 72.06458
#> c x > 0    44.00000 24.40237 65.07184
#>   y > 0    60.00000 38.66535 78.87452
#>   z > 0    44.00000 24.40237 65.07184

ex.data <- expand.grid(mean=c(0,25,50),sd=c(1,10,100))[rep(1:9,rep(250,9)),]
ex.data <- within(ex.data,x <- rnorm(n=nrow(ex.data),mean=ex.data$mean,sd=ex.data$sd))
ex.data <- within(ex.data,x.grp <- cases( x < 0,
                                            x >= 0 & x < 50,
                                            x >= 50 & x < 100,
                                            x >= 100
                                          ))
genTable(percent(x.grp)~mean+sd,data=ex.data)
#> , , sd = 1
#> 
#>                    mean
#>                         0  25    50
#>   x < 0              48.8   0   0.0
#>   x >= 0 & x < 50    51.2 100  55.2
#>   x >= 50 & x < 100   0.0   0  44.8
#>   x >= 100            0.0   0   0.0
#>   N                 250.0 250 250.0
#> 
#> , , sd = 10
#> 
#>                    mean
#>                       0    25    50
#>   x < 0              44   0.4   0.0
#>   x >= 0 & x < 50    56  98.4  46.8
#>   x >= 50 & x < 100   0   1.2  53.2
#>   x >= 100            0   0.0   0.0
#>   N                 250 250.0 250.0
#> 
#> , , sd = 100
#> 
#>                    mean
#>                         0    25    50
#>   x < 0              51.2  40.8  35.6
#>   x >= 0 & x < 50    18.8  22.4  16.0
#>   x >= 50 & x < 100  11.6  14.0  21.6
#>   x >= 100           18.4  22.8  26.8
#>   N                 250.0 250.0 250.0
#> 

Aggregate(percent(Admit,weight=Freq)~Gender+Dept,data=UCBAdmissions)
#>    Gender Dept  Admitted Rejected   N
#> 1    Male    A 62.060606 37.93939 825
#> 2  Female    A 82.407407 17.59259 108
#> 3    Male    B 63.035714 36.96429 560
#> 4  Female    B 68.000000 32.00000  25
#> 5    Male    C 36.923077 63.07692 325
#> 6  Female    C 34.064081 65.93592 593
#> 7    Male    D 33.093525 66.90647 417
#> 8  Female    D 34.933333 65.06667 375
#> 9    Male    E 27.748691 72.25131 191
#> 10 Female    E 23.918575 76.08142 393
#> 11   Male    F  5.898123 94.10188 373
#> 12 Female    F  7.038123 92.96188 341
```
