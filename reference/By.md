# Conditional Evaluation of an Expression

The function `By` evaluates an expression within subsets of a data
frame, where the subsets are defined by a formula.

## Usage

``` r
By(formula,expr,data=parent.frame())
```

## Arguments

- formula:

  an expression or (preferably) a formula containing the names of
  conditioning variables or factors.

- expr:

  an expression that is evaluated for any unique combination of values
  of the variables contained in `formula`.

- data:

  a data frame, an object that can be coerced into a data frame (for
  example, a [table](https://rdrr.io/r/base/table.html)), or an
  environment, from which values for the variables in `formula` or
  `expr` are taken.

## Value

A list of class "by", giving the results for each combination of values
of variables in `formula`.

## Examples

``` r
berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
(Bres <- By(~Dept,glm(cbind(Admitted,Rejected)~Gender,family="binomial"),data=berkeley))
#> Dept: A
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>       0.4921        1.0521  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       19.05 
#> Residual Deviance: 5.551e-15     AIC: 15.71
#> ------------------------------------------------------------ 
#> Dept: B
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>       0.5337        0.2200  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.2586 
#> Residual Deviance: 0     AIC: 14.27
#> ------------------------------------------------------------ 
#> Dept: C
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>      -0.5355       -0.1249  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.751 
#> Residual Deviance: -4.929e-14    AIC: 16.9
#> ------------------------------------------------------------ 
#> Dept: D
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>     -0.70396       0.08199  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.2979 
#> Residual Deviance: 1.883e-13     AIC: 16.65
#> ------------------------------------------------------------ 
#> Dept: E
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>      -0.9570       -0.2002  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.9904 
#> Residual Deviance: 1.821e-14     AIC: 15.6
#> ------------------------------------------------------------ 
#> Dept: F
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>      -2.7697        0.1889  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.3836 
#> Residual Deviance: -4.863e-14    AIC: 13.83
# The results all have 'data' components
str(Bres[[1]]$data)
#> 'data.frame':    2 obs. of  4 variables:
#>  $ Gender  : Factor w/ 2 levels "Male","Female": 1 2
#>  $ Dept    : Factor w/ 6 levels "A","B","C","D",..: 1 1
#>  $ Admitted: num  512 89
#>  $ Rejected: num  313 19

attach(berkeley)
(Bres <- By(~Dept,glm(cbind(Admitted,Rejected)~Gender,family="binomial")))
#> Dept: A
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>       0.4921        1.0521  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       19.05 
#> Residual Deviance: 5.551e-15     AIC: 15.71
#> ------------------------------------------------------------ 
#> Dept: B
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>       0.5337        0.2200  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.2586 
#> Residual Deviance: 0     AIC: 14.27
#> ------------------------------------------------------------ 
#> Dept: C
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>      -0.5355       -0.1249  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.751 
#> Residual Deviance: -4.929e-14    AIC: 16.9
#> ------------------------------------------------------------ 
#> Dept: D
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>     -0.70396       0.08199  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.2979 
#> Residual Deviance: 1.883e-13     AIC: 16.65
#> ------------------------------------------------------------ 
#> Dept: E
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>      -0.9570       -0.2002  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.9904 
#> Residual Deviance: 1.821e-14     AIC: 15.6
#> ------------------------------------------------------------ 
#> Dept: F
#> 
#> Call:  glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial")
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale  
#>      -2.7697        0.1889  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.3836 
#> Residual Deviance: -4.863e-14    AIC: 13.83
detach(berkeley)
```
