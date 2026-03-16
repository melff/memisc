# Generic Tables and Data Frames of Descriptive Statistics

`genTable` creates a table of arbitrary summaries conditional on given
values of independent variables given by a formula.

`Aggregate` does the same, but returns a `data.frame` instead.

`fapply` is a generic function that dispatches on its `data` argument.
It is called internally by `Aggregate` and `genTable`. Methods for this
function can be used to adapt `Aggregate` and `genTable` to data sources
other than data frames.

## Usage

``` r
Aggregate(formula, data=parent.frame(), subset=NULL,
          names=NULL, addFreq=TRUE, drop = TRUE, as.vars=1,
          ...)

genTable(formula, data=parent.frame(), subset=NULL,
         names=NULL, addFreq=TRUE,...)
```

## Arguments

- formula:

  a formula. The right hand side includes one or more grouping variables
  separated by '+'. These may be factors, numeric, or character vectors.
  The left hand side may be empty, a numerical variable, a factor, or an
  expression. See details below.

- data:

  an environment or data frame or an object coercable into a data frame.

- subset:

  an optional vector specifying a subset of observations to be used.

- names:

  an optional character vector giving names to the result(s) yielded by
  the expression on the left hand side of `formula`. This argument may
  be redundant if the left hand side results in is a named vector. (See
  the example below.)

- addFreq:

  a logical value. If `TRUE` and `data` is a table or a data frame with
  a variable named "Freq", a call to `table`,
  [`Table`](https://melff.github.io/memisc/reference/Table.md),
  [`percent`](https://melff.github.io/memisc/reference/percent.md), or
  [`nvalid`](https://melff.github.io/memisc/reference/value-filters.md)
  is supplied by an additional argument `Freq` and a call to `table` is
  translated into a call to `Table`.

- drop:

  a logical value. If `TRUE`, empty groups (i.e. when there are no
  observations in the aggregated data frame that contain the defining
  combination of values or factor levels of the conditioning variables
  in `by`) are dropped from the result of `Aggregate`. Otherwise, result
  are filled with `NA`, where appropriate.

- as.vars:

  an integer; relevant only if the left hand side of the formula returns
  an array or a matrix - which dimension (rows, columns, or layers etc.)
  will transformed to variables? Defaults to columns in case of matrices
  and to the highest dimensional extend in case of arrays.

- ...:

  further arguments, passed to methods or ignored.

## Details

If an expression is given as left hand side of the formula, its value is
computed for any combination of values of the values on the right hand
side. If the right hand side is a dot, then all variables in `data` are
added to the right hand side of the formula.

If no expression is given as left hand side, then the frequency counts
for the respective value combinations of the right hand variables are
computed.

If a single factor is on the left hand side, then the left hand side is
translated into an appropriate call to
[`table()`](https://rdrr.io/r/base/table.html). Note that also in this
case `addFreq` takes effect.

If a single numeric variable is on the left hand side, frequency counts
weighted by this variable are computed. In these cases, `genTable` is
equivalent to [`xtabs`](https://rdrr.io/r/stats/xtabs.html) and
`Aggregate` is equivalent to `as.data.frame(xtabs(...))`.

## Value

`Aggregate` results in a data frame with conditional summaries and
unique value combinations of conditioning variables.

`genTable` returns a [table](https://rdrr.io/r/base/table.html), that
is, an array with class `"table"`.

## See also

[aggregate.data.frame](https://rdrr.io/r/stats/aggregate.html),
[xtabs](https://rdrr.io/r/stats/xtabs.html)

## Examples

``` r
ex.data <- expand.grid(mu=c(0,100),sigma=c(1,10))[rep(1:4,rep(100,4)),]
ex.data <- within(ex.data,
                  x<-rnorm(
                    n=nrow(ex.data),
                    mean=mu,
                    sd=sigma
                    )
                  )

Aggregate(~mu+sigma,data=ex.data)
#>   sigma   0 100
#> 1     1 100 100
#> 2    10 100 100
Aggregate(mean(x)~mu+sigma,data=ex.data)
#>    mu sigma      mean(x)
#> 1   0     1   0.06236806
#> 2 100     1 100.04415320
#> 3   0    10   0.69106100
#> 4 100    10  99.12622089
Aggregate(mean(x)~mu+sigma,data=ex.data,name="Average")
#>    mu sigma      Average
#> 1   0     1   0.06236806
#> 2 100     1 100.04415320
#> 3   0    10   0.69106100
#> 4 100    10  99.12622089
Aggregate(c(mean(x),sd(x))~mu+sigma,data=ex.data)
#>    mu sigma      mean(x)    sd(x)
#> 1   0     1   0.06236806 1.042831
#> 2 100     1 100.04415320 1.032355
#> 3   0    10   0.69106100 8.894698
#> 4 100    10  99.12622089 8.952115
Aggregate(c(Mean=mean(x),StDev=sd(x),N=length(x))~mu+sigma,data=ex.data)
#>    mu sigma         Mean    StDev   N
#> 1   0     1   0.06236806 1.042831 100
#> 2 100     1 100.04415320 1.032355 100
#> 3   0    10   0.69106100 8.894698 100
#> 4 100    10  99.12622089 8.952115 100
genTable(c(Mean=mean(x),StDev=sd(x),N=length(x))~mu+sigma,data=ex.data)
#> , , sigma = 1
#> 
#>        mu
#>                    0        100
#>   Mean    0.06236806 100.044153
#>   StDev   1.04283135   1.032355
#>   N     100.00000000 100.000000
#> 
#> , , sigma = 10
#> 
#>        mu
#>                  0        100
#>   Mean    0.691061  99.126221
#>   StDev   8.894698   8.952115
#>   N     100.000000 100.000000
#> 

Aggregate(table(Admit)~.,data=UCBAdmissions)
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
Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
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
Aggregate(Admit~.,data=UCBAdmissions)
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
Aggregate(percent(Admit)~.,data=UCBAdmissions)
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
Aggregate(percent(Admit)~Gender,data=UCBAdmissions)
#>   Gender Admitted Rejected    N
#> 1   Male 44.51877 55.48123 2691
#> 2 Female 30.35422 69.64578 1835
Aggregate(percent(Admit)~Dept,data=UCBAdmissions)
#>   Dept  Admitted Rejected   N
#> 1    A 64.415863 35.58414 933
#> 2    B 63.247863 36.75214 585
#> 3    C 35.076253 64.92375 918
#> 4    D 33.964646 66.03535 792
#> 5    E 25.171233 74.82877 584
#> 6    F  6.442577 93.55742 714
Aggregate(percent(Gender)~Dept,data=UCBAdmissions)
#>   Dept     Male    Female   N
#> 1    A 88.42444 11.575563 933
#> 2    B 95.72650  4.273504 585
#> 3    C 35.40305 64.596950 918
#> 4    D 52.65152 47.348485 792
#> 5    E 32.70548 67.294521 584
#> 6    F 52.24090 47.759104 714
Aggregate(percent(Admit)~Dept,data=UCBAdmissions,Gender=="Female")
#>   Dept  Admitted Rejected   N
#> 1    A 82.407407 17.59259 108
#> 2    B 68.000000 32.00000  25
#> 3    C 34.064081 65.93592 593
#> 4    D 34.933333 65.06667 375
#> 5    E 23.918575 76.08142 393
#> 6    F  7.038123 92.96188 341
genTable(percent(Admit)~Dept,data=UCBAdmissions,Gender=="Female")
#>           Dept
#>                    A  B         C         D         E          F
#>   Admitted  82.40741 68  34.06408  34.93333  23.91858   7.038123
#>   Rejected  17.59259 32  65.93592  65.06667  76.08142  92.961877
#>   N        108.00000 25 593.00000 375.00000 393.00000 341.000000
```
