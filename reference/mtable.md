# Comparative Table of Model Estimates

`mtable` produces a table of estimates for several models.

## Usage

``` r
mtable(...,coef.style=getOption("coef.style"),
    summary.stats=TRUE,
    signif.symbols=getOption("signif.symbols"),
    factor.style=getOption("factor.style"),
    show.baselevel=getOption("show.baselevel"),
    baselevel.sep=getOption("baselevel.sep"),
    getSummary=eval.parent(quote(getSummary)),
    float.style=getOption("float.style"),
    digits=min(3,getOption("digits")),
    sdigits=digits,
    show.eqnames=getOption("mtable.show.eqnames",NA),
    gs.options=NULL,
    controls=NULL,
    collapse.controls=FALSE,
    control.var.indicator=getOption("control.var.indicator",c("Yes","No"))
  )
# S3 method for class 'memisc_mtable'
relabel(x, ..., gsub = FALSE, fixed = !gsub, warn = FALSE)

# S3 method for class 'memisc_mtable'
format(x,target=c("print","LaTeX","HTML","delim"),
    ...
    )

# S3 method for class 'memisc_mtable'
print(x,
    center.at=getOption("OutDec"),
    topsep="=",bottomsep="=",sectionsep="-",...)

write.mtable(object,file="",
             format=c("delim","LaTeX","HTML"),...)

# S3 method for class 'memisc_mtable'
toLatex(object,...)
```

## Arguments

- ...:

  as argument to `mtable`: several model objects, e.g. of class
  [`lm`](https://rdrr.io/r/stats/lm.html); as argument to
  `print.memisc_mtable`, `toLatex.memisc_mtable`, `write.memisc_mtable`:
  further arguments passed to `format.memisc_mtable`; as argument to
  `format.memisc_mtable`: further arguments passed to `format.default`;
  as argument to `relabel.memisc_mtable`: further arguments passed to
  [`dimrename`](https://melff.github.io/memisc/reference/dimrename.md).

- coef.style:

  a character string which specifies the style of coefficient values,
  whether standard errors, Wald/t-statistics, or significance levels are
  reported, etc. See
  [`coef.style`](https://melff.github.io/memisc/reference/styles.md).

- summary.stats:

  if `FALSE`, no summary statistics are repored. If `TRUE` then for each
  object in `...` either all summary statistics are reported, or those
  specified by the option `"summary.stats.<cls>"`, where `<cls>` is the
  class of the respective object.

  This argument may also contain a character vector with the names of
  the summary statistics to report, or a list of character vectors with
  names of summary statistics for each object passed as argument in
  `...`.

- signif.symbols:

  a named numeric vector to specify the "significance levels" and
  corresponding symbols. The numeric elements define the significance
  levels, the attached names define the associated symbols.

- factor.style:

  a character string that specifies the style in which factor contrasts
  are labled. See
  [`factor.style`](https://melff.github.io/memisc/reference/styles.md).

- show.baselevel:

  logical; determines whether base levels of factors are indicated for
  dummy coefficients

- baselevel.sep:

  character that is used to separate the base level from the level that
  a dummy variable represents

- getSummary:

  a function that computes model-related statistics that appear in the
  table. See
  [`getSummary`](https://melff.github.io/memisc/reference/getSummary.md).

- float.style:

  default format for floating point numbers if no format is specified by
  `coef.style`

.

- digits:

  number of significant digits if not specified by the template returned
  from
  [`getCoefTemplate`](https://melff.github.io/memisc/reference/styles.md)
  [`getSummaryTemplate`](https://melff.github.io/memisc/reference/styles.md)

- sdigits:

  integer; number of digits after decimal dot for summary statistics.

- show.eqnames:

  logical; if `TRUE`, left-hand sides of equations are (always) shown in
  the table header; if `FALSE`, left-hand sides of equations are not
  shown; if `NA`, left-hand sides of equations are shown only if
  left-hand sides differ among models or one of the models has multiple
  equations.

- gs.options:

  an optional list of arguments passed on to `getSummary`

- controls:

  an optional formula or character vector that designates "control
  variables" for which no coefficients are reported, but only whether
  they are present in the model.

- collapse.controls:

  a logical values; should the report about inclusion of control
  variables collapsed to a single value? If yes, models should either
  contain none or all of the control variables.

- control.var.indicator:

  a character vector with to elements; the first element being used to
  indicate the presence of a control variable or all control variables
  (if `collapse.controls=TRUE`), the second element being used
  otherwise. By default these elements are `"Yes"` and `"No"`.

- x, object:

  an object of class `mtable`

- gsub, warn, fixed:

  logical values, see
  [`relabel`](https://melff.github.io/memisc/reference/relabel.md)

- target:

  a character string which indicates the target format. Currenlty the
  targets "print" (see
  [`mtable_format_print`](https://melff.github.io/memisc/reference/mtable-format-print.md)),
  "LaTeX" (see
  [`mtable_format_latex`](https://melff.github.io/memisc/reference/mtable-format-latex.md)),
  "HTML" (see
  [`mtable_format_html`](https://melff.github.io/memisc/reference/mtable-format-html.html)),
  and "delim" (see
  [`mtable_format_delim`](https://melff.github.io/memisc/reference/mtable-format-delim.md))
  are supported.

- center.at:

  a character string on which resulting values are centered. Typically
  equal to ".". This is the default when `forLaTeX==TRUE`. If `NULL`,
  reported values are not centered.

- topsep:

  a character string that is recycled to a top rule.

- bottomsep:

  a character string that is recycled to a bottom rule.

- sectionsep:

  a character string that is recycled to seperate coefficients from
  summary statistics.

- file:

  name of the file where to write to; defaults to console output.

- format:

  character string that specifies the desired format.

## Details

`mtable` constructs a table of estimates for regression-type models.
`format.memisc_mtable` formats suitable for use with output or
conversion functions such as `print.memisc_mtable`,
`toLatex.memisc_mtable`, or `write.memisc_mtable`.

## Value

A call to `mtable` results in an object of class `"mtable"` with the
following components:

- coefficients:

  a list that contains the model coefficients,

- summaries:

  a matrix that contains the model summaries,

- calls:

  a list of calls that created the model estimates being summarised.

## Examples

``` r
#### Basic workflow

lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

options(summary.stats.lm=c("R-squared","N"))
mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2)
#> 
#> Calls:
#> Model 1: lm(formula = sr ~ pop15 + pop75, data = LifeCycleSavings)
#> Model 2: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> Model 3: lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
#> 
#> ================================================
#>                 Model 1    Model 2    Model 3   
#> ------------------------------------------------
#>   (Intercept)  30.628***   6.360***  28.566***  
#>                (7.409)    (1.252)    (7.355)    
#>   pop15        -0.471**              -0.461**   
#>                (0.147)               (0.145)    
#>   pop75        -1.934                -1.691     
#>                (1.041)               (1.084)    
#>   dpi                      0.001     -0.000     
#>                           (0.001)    (0.001)    
#>   ddpi                     0.529*     0.410*    
#>                           (0.210)    (0.196)    
#> ------------------------------------------------
#>   R-squared     0.262      0.162      0.338     
#>   N            50         50         50         
#> ================================================
#>   Significance: *** = p < 0.001;   
#>                 ** = p < 0.01; * = p < 0.05  

options(summary.stats.lm=c("sigma","R-squared","N"))
mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2)
#> 
#> Calls:
#> Model 1: lm(formula = sr ~ pop15 + pop75, data = LifeCycleSavings)
#> Model 2: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> Model 3: lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
#> 
#> ================================================
#>                 Model 1    Model 2    Model 3   
#> ------------------------------------------------
#>   (Intercept)  30.628***   6.360***  28.566***  
#>                (7.409)    (1.252)    (7.355)    
#>   pop15        -0.471**              -0.461**   
#>                (0.147)               (0.145)    
#>   pop75        -1.934                -1.691     
#>                (1.041)               (1.084)    
#>   dpi                      0.001     -0.000     
#>                           (0.001)    (0.001)    
#>   ddpi                     0.529*     0.410*    
#>                           (0.210)    (0.196)    
#> ------------------------------------------------
#>   R-squared     0.262      0.162      0.338     
#>   sigma         3.931      4.189      3.803     
#>   N            50         50         50         
#> ================================================
#>   Significance: *** = p < 0.001;   
#>                 ** = p < 0.01; * = p < 0.05  

options(summary.stats.lm=NULL)

mtable123 <- mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2,
    summary.stats=c("sigma","R-squared","F","p","N"))

(mtable123 <- relabel(mtable123,
  "(Intercept)" = "Constant",
          pop15 = "Percentage of population under 15",
          pop75 = "Percentage of population over 75",
            dpi = "Real per-capita disposable income",
           ddpi = "Growth rate of real per-capita disp. income"
  ))
#> 
#> Calls:
#> Model 1: lm(formula = sr ~ pop15 + pop75, data = LifeCycleSavings)
#> Model 2: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> Model 3: lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
#> 
#> ================================================================================
#>                                                 Model 1    Model 2    Model 3   
#> --------------------------------------------------------------------------------
#>   Constant                                     30.628***   6.360***  28.566***  
#>                                                (7.409)    (1.252)    (7.355)    
#>   Percentage of population under 15            -0.471**              -0.461**   
#>                                                (0.147)               (0.145)    
#>   Percentage of population over 75             -1.934                -1.691     
#>                                                (1.041)               (1.084)    
#>   Real per-capita disposable income                        0.001     -0.000     
#>                                                           (0.001)    (0.001)    
#>   Growth rate of real per-capita disp. income              0.529*     0.410*    
#>                                                           (0.210)    (0.196)    
#> --------------------------------------------------------------------------------
#>   R-squared                                     0.262      0.162      0.338     
#>   sigma                                         3.931      4.189      3.803     
#>   F                                             8.332      4.528      5.756     
#>   p                                             0.001      0.016      0.001     
#>   N                                            50         50         50         
#> ================================================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  

# This produces output in tab-delimited format:
write.mtable(mtable123)
#>  Model 1 Model 2 Model 3
#> Constant 30.628***   6.360***    28.566***
#>  (7.409) (1.252) (7.355)
#> Percentage of population under 15    -0.471**        -0.461**
#>  (0.147)     (0.145)
#> Percentage of population over 75 -1.934      -1.691
#>  (1.041)     (1.084)
#> Real per-capita disposable income        0.001   -0.000
#>      (0.001) (0.001)
#> Growth rate of real per-capita disp. income      0.529*  0.410*
#>      (0.210) (0.196)
#> R-squared    0.262   0.162   0.338
#> sigma    3.931   4.189   3.803
#> F    8.332   4.528   5.756
#> p    0.001   0.016   0.001
#> N    50  50  50

if (FALSE) { # \dontrun{
# This produces output in tab-delimited format:
file123 <- "mtable123.txt"
write.mtable(mtable123,file=file123)
file.show(file123)
# The contents of this file can be pasted into Word
# and converted into a Word table.
} # }
if (FALSE) texfile123 <- "mtable123.tex"
write.mtable(mtable123,format="LaTeX",file=texfile123)
#> Error: object 'texfile123' not found
file.show(texfile123) # \dontrun{}
#> Error: object 'texfile123' not found

#### Examples with UC Berkeley data

berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)

berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

mtable(berk0,summary.stats=c("Deviance","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> 
#> ============================
#>   (Intercept)    -0.457***  
#>                  (0.031)    
#> ----------------------------
#>   Deviance      877.056     
#>   N            4526         
#> ============================
#>   Significance:   
#>                 *** = p < 0.001;   
#>                 ** = p < 0.01;   
#>                 * = p < 0.05  
mtable(berk1,summary.stats=c("Deviance","N"))
#> 
#> Calls:
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> 
#> ====================================
#>   (Intercept)            -0.220***  
#>                          (0.039)    
#>   Gender: Female/Male    -0.610***  
#>                          (0.064)    
#> ------------------------------------
#>   Deviance              783.607     
#>   N                    4526         
#> ====================================
#>   Significance: *** = p < 0.001;   
#>                 ** = p < 0.01;   
#>                 * = p < 0.05  

mtable(berk0,berk1,berk2,summary.stats=c("Deviance","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> ==============================================================
#>                           berk0        berk1        berk2     
#> --------------------------------------------------------------
#>   (Intercept)            -0.457***    -0.220***     0.582***  
#>                          (0.031)      (0.039)      (0.069)    
#>   Gender: Female/Male                 -0.610***     0.100     
#>                                       (0.064)      (0.081)    
#>   Dept: B/A                                        -0.043     
#>                                                    (0.110)    
#>   Dept: C/A                                        -1.263***  
#>                                                    (0.107)    
#>   Dept: D/A                                        -1.295***  
#>                                                    (0.106)    
#>   Dept: E/A                                        -1.739***  
#>                                                    (0.126)    
#>   Dept: F/A                                        -3.306***  
#>                                                    (0.170)    
#> --------------------------------------------------------------
#>   Deviance              877.056      783.607       20.204     
#>   N                    4526         4526         4526         
#> ==============================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  

mtable(berk0,berk1,berk2,
          coef.style="horizontal",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> ======================================================================================
#>                               berk0                berk1                berk2         
#> --------------------------------------------------------------------------------------
#>   (Intercept)            -0.457*** (0.031)    -0.220*** (0.039)     0.582*** (0.069)  
#>   Gender: Female/Male                         -0.610*** (0.064)     0.100    (0.081)  
#>   Dept: B/A                                                        -0.043    (0.110)  
#>   Dept: C/A                                                        -1.263*** (0.107)  
#>   Dept: D/A                                                        -1.295*** (0.106)  
#>   Dept: E/A                                                        -1.739*** (0.126)  
#>   Dept: F/A                                                        -3.306*** (0.170)  
#> --------------------------------------------------------------------------------------
#>   Deviance              877.056              783.607               20.204             
#>   AIC                   947.996              856.547              103.144             
#>   N                    4526                 4526                 4526                 
#> ======================================================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  
mtable(berk0,berk1,berk2,
          coef.style="stat",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> ==============================================================
#>                           berk0        berk1        berk2     
#> --------------------------------------------------------------
#>   (Intercept)            -0.457***    -0.220***     0.582***  
#>                        (-14.972)     (-5.675)      (8.436)    
#>   Gender: Female/Male                 -0.610***     0.100     
#>                                      (-9.553)      (1.235)    
#>   Dept: B/A                                        -0.043     
#>                                                   (-0.395)    
#>   Dept: C/A                                        -1.263***  
#>                                                  (-11.841)    
#>   Dept: D/A                                        -1.295***  
#>                                                  (-12.234)    
#>   Dept: E/A                                        -1.739***  
#>                                                  (-13.792)    
#>   Dept: F/A                                        -3.306***  
#>                                                  (-19.452)    
#> --------------------------------------------------------------
#>   Deviance              877.056      783.607       20.204     
#>   AIC                   947.996      856.547      103.144     
#>   N                    4526         4526         4526         
#> ==============================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  
mtable(berk0,berk1,berk2,
          coef.style="ci",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> ========================================================
#>                          berk0      berk1      berk2    
#> --------------------------------------------------------
#>   (Intercept)            -0.457     -0.220      0.582   
#>                         [-0.517    [-0.296     [0.447   
#>                          -0.397]    -0.144]     0.717]  
#>   Gender: Female/Male               -0.610      0.100   
#>                                    [-0.736    [-0.059   
#>                                     -0.485]     0.258]  
#>   Dept: B/A                                    -0.043   
#>                                               [-0.259   
#>                                                 0.172]  
#>   Dept: C/A                                    -1.263   
#>                                               [-1.472   
#>                                                -1.054]  
#>   Dept: D/A                                    -1.295   
#>                                               [-1.502   
#>                                                -1.087]  
#>   Dept: E/A                                    -1.739   
#>                                               [-1.986   
#>                                                -1.492]  
#>   Dept: F/A                                    -3.306   
#>                                               [-3.640   
#>                                                -2.973]  
#> --------------------------------------------------------
#>   Deviance              877.056    783.607     20.204   
#>   AIC                   947.996    856.547    103.144   
#>   N                    4526       4526       4526       
#> ========================================================
mtable(berk0,berk1,berk2,
          coef.style="ci.se",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> ========================================================
#>                          berk0      berk1      berk2    
#> --------------------------------------------------------
#>   (Intercept)            -0.457     -0.220      0.582   
#>                          (0.031)    (0.039)    (0.069)  
#>                         [-0.517    [-0.296     [0.447   
#>                          -0.397]    -0.144]     0.717]  
#>   Gender: Female/Male               -0.610      0.100   
#>                                     (0.064)    (0.081)  
#>                                    [-0.736    [-0.059   
#>                                     -0.485]     0.258]  
#>   Dept: B/A                                    -0.043   
#>                                                (0.110)  
#>                                               [-0.259   
#>                                                 0.172]  
#>   Dept: C/A                                    -1.263   
#>                                                (0.107)  
#>                                               [-1.472   
#>                                                -1.054]  
#>   Dept: D/A                                    -1.295   
#>                                                (0.106)  
#>                                               [-1.502   
#>                                                -1.087]  
#>   Dept: E/A                                    -1.739   
#>                                                (0.126)  
#>                                               [-1.986   
#>                                                -1.492]  
#>   Dept: F/A                                    -3.306   
#>                                                (0.170)  
#>                                               [-3.640   
#>                                                -2.973]  
#> --------------------------------------------------------
#>   Deviance              877.056    783.607     20.204   
#>   AIC                   947.996    856.547    103.144   
#>   N                    4526       4526       4526       
#> ========================================================
mtable(berk0,berk1,berk2,
          coef.style="ci.se.horizontal",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> =============================================================================
#>                             berk0             berk1             berk2        
#> -----------------------------------------------------------------------------
#>   (Intercept)            -0.457 (0.031)    -0.220 (0.039)     0.582 (0.069)  
#>                         [-0.517 -0.397]   [-0.296 -0.144]    [0.447  0.717]  
#>   Gender: Female/Male                      -0.610 (0.064)     0.100 (0.081)  
#>                                           [-0.736 -0.485]   [-0.059  0.258]  
#>   Dept: B/A                                                  -0.043 (0.110)  
#>                                                             [-0.259  0.172]  
#>   Dept: C/A                                                  -1.263 (0.107)  
#>                                                             [-1.472 -1.054]  
#>   Dept: D/A                                                  -1.295 (0.106)  
#>                                                             [-1.502 -1.087]  
#>   Dept: E/A                                                  -1.739 (0.126)  
#>                                                             [-1.986 -1.492]  
#>   Dept: F/A                                                  -3.306 (0.170)  
#>                                                             [-3.640 -2.973]  
#> -----------------------------------------------------------------------------
#>   Deviance              877.056           783.607            20.204          
#>   AIC                   947.996           856.547           103.144          
#>   N                    4526              4526              4526              
#> =============================================================================
mtable(berk0,berk1,berk2,
          coef.style="ci.p.horizontal",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> =============================================================================
#>                             berk0             berk1             berk2        
#> -----------------------------------------------------------------------------
#>   (Intercept)            -0.457 (0.000)    -0.220 (0.000)     0.582 (0.000)  
#>                         [-0.517 -0.397]   [-0.296 -0.144]    [0.447  0.717]  
#>   Gender: Female/Male                      -0.610 (0.000)     0.100 (0.217)  
#>                                           [-0.736 -0.485]   [-0.059  0.258]  
#>   Dept: B/A                                                  -0.043 (0.693)  
#>                                                             [-0.259  0.172]  
#>   Dept: C/A                                                  -1.263 (0.000)  
#>                                                             [-1.472 -1.054]  
#>   Dept: D/A                                                  -1.295 (0.000)  
#>                                                             [-1.502 -1.087]  
#>   Dept: E/A                                                  -1.739 (0.000)  
#>                                                             [-1.986 -1.492]  
#>   Dept: F/A                                                  -3.306 (0.000)  
#>                                                             [-3.640 -2.973]  
#> -----------------------------------------------------------------------------
#>   Deviance              877.056           783.607            20.204          
#>   AIC                   947.996           856.547           103.144          
#>   N                    4526              4526              4526              
#> =============================================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  
mtable(berk0,berk1,berk2,
          coef.style="ci.horizontal",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> =====================================================================================================
#>                                 berk0                     berk1                     berk2            
#> -----------------------------------------------------------------------------------------------------
#>   (Intercept)            -0.457 [-0.517 -0.397]    -0.220 [-0.296 -0.144]     0.582  [0.447  0.717]  
#>   Gender: Female/Male                              -0.610 [-0.736 -0.485]     0.100 [-0.059  0.258]  
#>   Dept: B/A                                                                  -0.043 [-0.259  0.172]  
#>   Dept: C/A                                                                  -1.263 [-1.472 -1.054]  
#>   Dept: D/A                                                                  -1.295 [-1.502 -1.087]  
#>   Dept: E/A                                                                  -1.739 [-1.986 -1.492]  
#>   Dept: F/A                                                                  -3.306 [-3.640 -2.973]  
#> -----------------------------------------------------------------------------------------------------
#>   Deviance              877.056                   783.607                    20.204                  
#>   AIC                   947.996                   856.547                   103.144                  
#>   N                    4526                      4526                      4526                      
#> =====================================================================================================
mtable(berk0,berk1,berk2,
          coef.style="all",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> ==============================================================
#>                           berk0        berk1        berk2     
#> --------------------------------------------------------------
#>   (Intercept)            -0.457***    -0.220***     0.582***  
#>                          (0.031)      (0.039)      (0.069)    
#>                        (-14.972)     (-5.675)      (8.436)    
#>                          (0.000)      (0.000)      (0.000)    
#>   Gender: Female/Male                 -0.610***     0.100     
#>                                       (0.064)      (0.081)    
#>                                      (-9.553)      (1.235)    
#>                                       (0.000)      (0.217)    
#>   Dept: B/A                                        -0.043     
#>                                                    (0.110)    
#>                                                   (-0.395)    
#>                                                    (0.693)    
#>   Dept: C/A                                        -1.263***  
#>                                                    (0.107)    
#>                                                  (-11.841)    
#>                                                    (0.000)    
#>   Dept: D/A                                        -1.295***  
#>                                                    (0.106)    
#>                                                  (-12.234)    
#>                                                    (0.000)    
#>   Dept: E/A                                        -1.739***  
#>                                                    (0.126)    
#>                                                  (-13.792)    
#>                                                    (0.000)    
#>   Dept: F/A                                        -3.306***  
#>                                                    (0.170)    
#>                                                  (-19.452)    
#>                                                    (0.000)    
#> --------------------------------------------------------------
#>   Deviance              877.056      783.607       20.204     
#>   AIC                   947.996      856.547      103.144     
#>   N                    4526         4526         4526         
#> ==============================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  
mtable(berk0,berk1,berk2,
          coef.style="all.nostar",
          summary.stats=c("Deviance","AIC","N"))
#> 
#> Calls:
#> berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial", 
#>     data = berkeley)
#> berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = berkeley)
#> berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial", 
#>     data = berkeley)
#> 
#> ========================================================
#>                          berk0      berk1      berk2    
#> --------------------------------------------------------
#>   (Intercept)            -0.457     -0.220      0.582   
#>                          (0.031)    (0.039)    (0.069)  
#>                        (-14.972)   (-5.675)    (8.436)  
#>                          (0.000)    (0.000)    (0.000)  
#>   Gender: Female/Male               -0.610      0.100   
#>                                     (0.064)    (0.081)  
#>                                    (-9.553)    (1.235)  
#>                                     (0.000)    (0.217)  
#>   Dept: B/A                                    -0.043   
#>                                                (0.110)  
#>                                               (-0.395)  
#>                                                (0.693)  
#>   Dept: C/A                                    -1.263   
#>                                                (0.107)  
#>                                              (-11.841)  
#>                                                (0.000)  
#>   Dept: D/A                                    -1.295   
#>                                                (0.106)  
#>                                              (-12.234)  
#>                                                (0.000)  
#>   Dept: E/A                                    -1.739   
#>                                                (0.126)  
#>                                              (-13.792)  
#>                                                (0.000)  
#>   Dept: F/A                                    -3.306   
#>                                                (0.170)  
#>                                              (-19.452)  
#>                                                (0.000)  
#> --------------------------------------------------------
#>   Deviance              877.056    783.607     20.204   
#>   AIC                   947.996    856.547    103.144   
#>   N                    4526       4526       4526       
#> ========================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01;   
#>                 * = p < 0.05  

mtable(by(berkeley,berkeley$Dept,
  function(x)glm(cbind(Admitted,Rejected)~Gender,
        data=x,family="binomial")),
      summary.stats=c("Likelihood-ratio","N"))
#> 
#> Calls:
#> A: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = x)
#> B: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = x)
#> C: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = x)
#> D: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = x)
#> E: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = x)
#> F: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial", 
#>     data = x)
#> 
#> ===============================================================================================
#>                            A           B           C           D           E           F       
#> -----------------------------------------------------------------------------------------------
#>   (Intercept)            0.492***    0.534***   -0.536***   -0.704***   -0.957***   -2.770***  
#>                         (0.072)     (0.088)     (0.115)     (0.104)     (0.162)     (0.220)    
#>   Gender: Female/Male    1.052***    0.220      -0.125       0.082      -0.200       0.189     
#>                         (0.263)     (0.438)     (0.144)     (0.150)     (0.200)     (0.305)    
#> -----------------------------------------------------------------------------------------------
#>   Likelihood-ratio      19.054       0.259       0.751       0.298       0.990       0.384     
#>   N                    933         585         918         792         584         714         
#> ===============================================================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  


mtable(By(~Gender,
  glm(cbind(Admitted,Rejected)~Dept,
        family="binomial"),
        data=berkeley),
      summary.stats=c("Likelihood-ratio","N"))
#> 
#> Calls:
#> Male: glm(formula = cbind(Admitted, Rejected) ~ Dept, family = "binomial")
#> Female: glm(formula = cbind(Admitted, Rejected) ~ Dept, family = "binomial")
#> 
#> ==============================================
#>                        Male        Female     
#> ----------------------------------------------
#>   (Intercept)          0.492***     1.544***  
#>                       (0.072)      (0.253)    
#>   Dept: B/A            0.042       -0.790     
#>                       (0.113)      (0.498)    
#>   Dept: C/A           -1.028***    -2.205***  
#>                       (0.135)      (0.267)    
#>   Dept: D/A           -1.196***    -2.166***  
#>                       (0.126)      (0.275)    
#>   Dept: E/A           -1.449***    -2.701***  
#>                       (0.177)      (0.279)    
#>   Dept: F/A           -3.262***    -4.125***  
#>                       (0.231)      (0.330)    
#> ----------------------------------------------
#>   Likelihood-ratio   514.756      268.851     
#>   N                 2691         1835         
#> ==============================================
#>   Significance: *** = p < 0.001;   
#>                 ** = p < 0.01; * = p < 0.05  

berkfull <- glm(cbind(Admitted,Rejected)~Dept/Gender - 1,
                      data=berkeley,family="binomial")
relabel(mtable(berkfull),Dept="Department",gsub=TRUE)
#> 
#> Calls:
#> berkfull: glm(formula = cbind(Admitted, Rejected) ~ Dept/Gender - 1, family = "binomial", 
#>     data = berkeley)
#> 
#> ====================================================
#>   Department: A                           0.492***  
#>                                          (0.072)    
#>   Department: B                           0.534***  
#>                                          (0.088)    
#>   Department: C                          -0.536***  
#>                                          (0.115)    
#>   Department: D                          -0.704***  
#>                                          (0.104)    
#>   Department: E                          -0.957***  
#>                                          (0.162)    
#>   Department: F                          -2.770***  
#>                                          (0.220)    
#>   Department: A x Gender: Female/Male     1.052***  
#>                                          (0.263)    
#>   Department: B x Gender: Female/Male     0.220     
#>                                          (0.438)    
#>   Department: C x Gender: Female/Male    -0.125     
#>                                          (0.144)    
#>   Department: D x Gender: Female/Male     0.082     
#>                                          (0.150)    
#>   Department: E x Gender: Female/Male    -0.200     
#>                                          (0.200)    
#>   Department: F x Gender: Female/Male     0.189     
#>                                          (0.305)    
#> ----------------------------------------------------
#>   Log-likelihood                        -34.470     
#>   N                                    4526         
#> ====================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01;   
#>                 * = p < 0.05  

#### Array-like semantics

mtable123 <- mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2,
    summary.stats=c("sigma","R-squared","F","p","N"))

dim(mtable123)
#> [1] 5 1 3
dimnames(mtable123)
#> [[1]]
#> [1] "(Intercept)" "pop15"       "pop75"       "dpi"         "ddpi"       
#> 
#> [[2]]
#> [1] "sr"
#> 
#> [[3]]
#> [1] "Model 1" "Model 2" "Model 3"
#> 
mtable123[c("dpi","ddpi"),
          c("Model 2","Model 3")]
#> 
#> Calls:
#> Model 2: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> Model 3: lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
#> 
#> ===============================
#>              Model 2  Model 3  
#> -------------------------------
#>   dpi         0.001   -0.000   
#>              (0.001)  (0.001)  
#>   ddpi        0.529*   0.410*  
#>              (0.210)  (0.196)  
#> -------------------------------
#>   R-squared   0.162    0.338   
#>   sigma       4.189    3.803   
#>   F           4.528    5.756   
#>   p           0.016    0.001   
#>   N          50       50       
#> ===============================
#>   Significance:   
#>                 *** = p < 0.001;   
#>                 ** = p < 0.01;   
#>                 * = p < 0.05  

#### Concatention
mt01 <- mtable(lm0,lm1,summary.stats=c("R-squared","N"))
mt12 <- mtable(lm1,lm2,summary.stats=c("R-squared","F","N"))
c(mt01,mt12) # not that this makes sense, but ...
#> 
#> Calls:
#> lm0: lm(formula = sr ~ pop15 + pop75, data = LifeCycleSavings)
#> lm1: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> lm1: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> lm2: lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
#> 
#> ===========================================================
#>                   lm0        lm1        lm1        lm2     
#> -----------------------------------------------------------
#>   (Intercept)  30.628***   6.360***   6.360***  28.566***  
#>                (7.409)    (1.252)    (1.252)    (7.355)    
#>   pop15        -0.471**                         -0.461**   
#>                (0.147)                          (0.145)    
#>   pop75        -1.934                           -1.691     
#>                (1.041)                          (1.084)    
#>   dpi                      0.001      0.001     -0.000     
#>                           (0.001)    (0.001)    (0.001)    
#>   ddpi                     0.529*     0.529*     0.410*    
#>                           (0.210)    (0.210)    (0.196)    
#> -----------------------------------------------------------
#>   R-squared     0.262      0.162      0.162      0.338     
#>   N            50         50         50         50         
#>   F                                   4.528      5.756     
#> ===========================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01;   
#>                 * = p < 0.05  
c("Group 1"=mt01,
  "Group 2"=mt12)
#> 
#> Calls:
#> lm0: lm(formula = sr ~ pop15 + pop75, data = LifeCycleSavings)
#> lm1: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> lm1: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
#> lm2: lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
#> 
#> =========================================================
#>                      Group 1              Group 2        
#>                -------------------  -------------------  
#>                   lm0       lm1        lm1       lm2     
#> ---------------------------------------------------------
#>   (Intercept)  30.628***  6.360***   6.360*** 28.566***  
#>                (7.409)   (1.252)    (1.252)   (7.355)    
#>   pop15        -0.471**                       -0.461**   
#>                (0.147)                        (0.145)    
#>   pop75        -1.934                         -1.691     
#>                (1.041)                        (1.084)    
#>   dpi                     0.001      0.001    -0.000     
#>                          (0.001)    (0.001)   (0.001)    
#>   ddpi                    0.529*     0.529*    0.410*    
#>                          (0.210)    (0.210)   (0.196)    
#> ---------------------------------------------------------
#>   R-squared     0.262     0.162      0.162     0.338     
#>   N            50        50         50        50         
#>   F                                  4.528     5.756     
#> =========================================================
#>   Significance: *** = p < 0.001; ** = p < 0.01;   
#>                 * = p < 0.05  
```
