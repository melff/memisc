# Add Alternative Variance Estimates to Models Estimates

A simple object-orientation infrastructure to add alternative standard
errors, e.g. sandwich estimates or New-West standard errors to fitted
regression-type models, such as fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html) or
[`glm()`](https://rdrr.io/r/stats/glm.html).

## Usage

``` r
withSE(object, vcov, ...) 

withVCov(object, vcov, ...)

# S3 method for class 'lm'
withVCov(object, vcov, ...)

# S3 method for class 'withVCov'
summary(object, ...)
# S3 method for class 'withVCov.lm'
summary(object, ...)
```

## Arguments

- object:

  a fitted model object

- vcov:

  a function that returns a variance matrix estimate, a given matrix
  that is such an estimate, or a character string that identifies a
  function that returns a variance matrix estimate (e.g. `"HAC"` for
  `vcovHAC`).

- ...:

  further arguments, passed to
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) or, respectively, to the
  parent method of [`summary()`](https://rdrr.io/r/base/summary.html)

## Details

Using `withVCov()` an alternative variance-covariance matrix is
attributed to a fitted model object. Such a matrix may be produced by
any of the variance estimators provided by the "sandwich" package or any
package that extends it.

`withVCov()` has no consequences on how a fitted model itself is printed
or represented, but it does have consequences what standard errors are
reported, when the function
[`summary()`](https://rdrr.io/r/base/summary.html) or the function
[`mtable()`](https://melff.github.io/memisc/reference/mtable.md) is
applied.

`withSE()` is a convenience front-end to `withVCov()`. It can be called
in the same way as `withVCov`, but also allows to specify the type of
variance estimate by a character string that identifies the function
that gives the covariance matrix (e.g. `"OPG"` for `vcovOPG`).

## Value

`withVCov` returns a slightly modified model object: It adds an
attribute named ".VCov" that contains the alternate covaraince matrix
and modifies the class attribute. If e.g. the original model object has
class "lm" then the model object modified by `withVCov` has the class
attribute `c("withVCov.lm", "withVCov", "lm")`.

## Examples

``` r
## Generate poisson regression relationship
x <- sin(1:100)
y <- rpois(100, exp(1 + x))
## compute usual covariance matrix of coefficient estimates
fm <- glm(y ~ x, family = poisson)

library(sandwich)
fmo <- withVCov(fm,vcovOPG)
vcov(fm)
#>              (Intercept)            x
#> (Intercept)  0.004335095 -0.003369000
#> x           -0.003369000  0.007855207
vcov(fmo)
#>              (Intercept)            x
#> (Intercept)  0.005044417 -0.003785476
#> x           -0.003785476  0.010668796

summary(fm)
#> 
#> Call:
#> glm(formula = y ~ x, family = poisson)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  1.02809    0.06584   15.62   <2e-16 ***
#> x            0.94781    0.08863   10.69   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for poisson family taken to be 1)
#> 
#>     Null deviance: 226.874  on 99  degrees of freedom
#> Residual deviance:  93.095  on 98  degrees of freedom
#> AIC: 365.05
#> 
#> Number of Fisher Scoring iterations: 5
#> 
summary(fmo)
#> 
#> Call:
#> glm(formula = y ~ x, family = poisson)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  1.02809    0.07102  14.475   <2e-16 ***
#> x            0.94781    0.10329   9.176   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for poisson family taken to be 1)
#> 
#>     Null deviance: 226.874  on 99  degrees of freedom
#> Residual deviance:  93.095  on 98  degrees of freedom
#> AIC: 365.05
#> 
#> Number of Fisher Scoring iterations: 5
#> 

mtable(Default=fm,
       OPG=withSE(fm,"OPG"),
       summary.stats=c("Deviance","N")
       )
#> 
#> Calls:
#> Default: glm(formula = y ~ x, family = poisson)
#> OPG: glm(formula = y ~ x, family = poisson)
#> 
#> =======================================
#>                 Default       OPG      
#> ---------------------------------------
#>   (Intercept)    1.028***    1.028***  
#>                 (0.066)     (0.071)    
#>   x              0.948***    0.948***  
#>                 (0.089)     (0.103)    
#> ---------------------------------------
#>   Deviance      93.095      93.095     
#>   N            100         100         
#> =======================================
#>   Significance: *** = p < 0.001;   
#>                 ** = p < 0.01;   
#>                 * = p < 0.05  

vo <- vcovOPG(fm)

mtable(Default=fm,
       OPG=withSE(fm,vo),
       summary.stats=c("Deviance","N")
       )
#> 
#> Calls:
#> Default: glm(formula = y ~ x, family = poisson)
#> OPG: glm(formula = y ~ x, family = poisson)
#> 
#> =======================================
#>                 Default       OPG      
#> ---------------------------------------
#>   (Intercept)    1.028***    1.028***  
#>                 (0.066)     (0.071)    
#>   x              0.948***    0.948***  
#>                 (0.089)     (0.103)    
#> ---------------------------------------
#>   Deviance      93.095      93.095     
#>   N            100         100         
#> =======================================
#>   Significance: *** = p < 0.001;   
#>                 ** = p < 0.01;   
#>                 * = p < 0.05  
```
