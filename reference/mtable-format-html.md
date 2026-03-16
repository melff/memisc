# HTML Formatting for 'mtable' Results

These functions formats 'mtable' objects into HTML format.

## Usage

``` r
mtable_format_html(x,
                    interaction.sep = NULL,
                    toprule=2,midrule=1,bottomrule=2,
                    split.dec=TRUE,
                    style=mtable_format_stdstyle,
                    margin="2ex auto", 
                    sig.notes.style=c(width="inherit"),
                    ...
  )
# S3 method for class 'memisc_mtable'
format_html(x,
                    interaction.sep = NULL,
                    toprule=2,midrule=1,bottomrule=2,
                    split.dec=TRUE,
                    style=mtable_format_stdstyle,
                    margin="2ex auto", 
                    sig.notes.style=c(width="inherit"),
                    ...
  )
```

## Arguments

- x:

  an object of class `mtable`

- toprule:

  integer; thickness in pixels of rule at the top of the table.

- midrule:

  integer; thickness in pixels of rules within the table.

- bottomrule:

  integer; thickness in pixels of rule at the bottom of the table.

- interaction.sep:

  a character string that separates factors that are involved in an
  interaction effect or NULL. If NULL then a reasonable default is used
  (either a unicode character or an ampersand encoded HTML entity).

- split.dec:

  logical; whether numbers should be centered at the decimal point by
  splitting the table cells.

- style:

  string containing default the CSS styling.

- margin:

  character string, determines the margin and thus the position of the
  HTML table.

- sig.notes.style:

  a character vector with named elements, allows extra styling of the
  p-values notes at the bottom of the table.

- ...:

  further arguments, ignored.

## Value

A character string with code suitable for inclusion into a HTML-file.

## Examples

``` r
lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

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

# Use HTML entity '&minus;' for minus sign
options(html.use.ampersand=TRUE)
show_html(mtable123)
#> <style>
#>       table#mtable-1 {
#>          border-collapse: collapse; border-style: none; margin: 2ex auto;
#>       } 
#>       table#mtable-1 tr {
#>           border-style: none;
#>       } 
#>       table#mtable-1 td {
#>         padding-top: 1px; 
#>         padding-bottom: 1px; 
#>         padding-left: 0.5ex; 
#>         padding-right: 0.5ex; 
#>         margin: 0px; 
#>         margin-top: 0px;
#>         margin-bottom: 0px;
#>         border-style: none; 
#>         border-width: 0px; 
#>       }
#> table#mtable-1 tr:nth-child(1){
#>      border-top: 2px solid;
#> }
#> 
#> table#mtable-1 tr:nth-child(1),
#> table#mtable-1 tr:nth-child(11){
#>      border-bottom: 1px solid;
#> }
#> table#mtable-1 tr:nth-child(16){
#>      border-bottom: 2px solid;
#> } table#mtable-1 td:nth-child(3),
#> table#mtable-1 td:nth-child(6),
#> table#mtable-1 td:nth-child(9){
#>          padding-left: 0px;
#>          padding-right: 0px;
#>          text-align: center; 
#>          /*background-color: blue;*/
#>         } table#mtable-1 td:nth-child(2),
#> table#mtable-1 td:nth-child(5),
#> table#mtable-1 td:nth-child(8){
#>          padding-left: 0.5ex;
#>          padding-right: 0px;
#>          text-align: right; 
#>          /*background-color: red;*/
#>         } table#mtable-1 td:nth-child(4),
#> table#mtable-1 td:nth-child(7),
#> table#mtable-1 td:nth-child(10){
#>          padding-left: 0px;
#>          padding-right: 0.5ex;
#>          text-align: left; 
#>          /*background-color: green;*/
#>         } 
#> table#mtable-1 td:nth-child(n of .header){
#>          padding-left: 0.5ex;
#>          padding-right: 0.5ex;
#>          text-align: center; 
#>          /*background-color: gray;*/
#>         } 
#> </style><table id="mtable-1">
#> <tr><td colspan="1" class="header"></td><td colspan="3" class="header">Model 1</td><td colspan="3" class="header">Model 2</td><td colspan="3" class="header">Model 3</td></tr>
#> <tr><td>Constant</td><td>30</td><td>.</td><td>628<span class="signif.symbol">***</span></td><td>6</td><td>.</td><td>360<span class="signif.symbol">***</span></td><td>28</td><td>.</td><td>566<span class="signif.symbol">***</span></td></tr>
#> <tr><td></td><td>(7</td><td>.</td><td>409)</td><td>(1</td><td>.</td><td>252)</td><td>(7</td><td>.</td><td>355)</td></tr>
#> <tr><td>Percentage of population under 15</td><td>&minus;0</td><td>.</td><td>471<span class="signif.symbol">**</span></td><td></td><td></td><td></td><td>&minus;0</td><td>.</td><td>461<span class="signif.symbol">**</span></td></tr>
#> <tr><td></td><td>(0</td><td>.</td><td>147)</td><td></td><td></td><td></td><td>(0</td><td>.</td><td>145)</td></tr>
#> <tr><td>Percentage of population over 75</td><td>&minus;1</td><td>.</td><td>934</td><td></td><td></td><td></td><td>&minus;1</td><td>.</td><td>691</td></tr>
#> <tr><td></td><td>(1</td><td>.</td><td>041)</td><td></td><td></td><td></td><td>(1</td><td>.</td><td>084)</td></tr>
#> <tr><td>Real per-capita disposable income</td><td></td><td></td><td></td><td>0</td><td>.</td><td>001</td><td>&minus;0</td><td>.</td><td>000</td></tr>
#> <tr><td></td><td></td><td></td><td></td><td>(0</td><td>.</td><td>001)</td><td>(0</td><td>.</td><td>001)</td></tr>
#> <tr><td>Growth rate of real per-capita disp. income</td><td></td><td></td><td></td><td>0</td><td>.</td><td>529<span class="signif.symbol">*</span></td><td>0</td><td>.</td><td>410<span class="signif.symbol">*</span></td></tr>
#> <tr><td></td><td></td><td></td><td></td><td>(0</td><td>.</td><td>210)</td><td>(0</td><td>.</td><td>196)</td></tr>
#> <tr><td>R-squared</td><td>0</td><td>.</td><td>262</td><td>0</td><td>.</td><td>162</td><td>0</td><td>.</td><td>338</td></tr>
#> <tr><td>sigma</td><td>3</td><td>.</td><td>931</td><td>4</td><td>.</td><td>189</td><td>3</td><td>.</td><td>803</td></tr>
#> <tr><td>F</td><td>8</td><td>.</td><td>332</td><td>4</td><td>.</td><td>528</td><td>5</td><td>.</td><td>756</td></tr>
#> <tr><td>p</td><td>0</td><td>.</td><td>001</td><td>0</td><td>.</td><td>016</td><td>0</td><td>.</td><td>001</td></tr>
#> <tr><td>N</td><td>50</td><td></td><td></td><td>50</td><td></td><td></td><td>50</td><td></td><td></td></tr>
#> <tr><td colspan="10"><p>Significance: *** = p &lt; 0.001; ** = p &lt; 0.01; * = p &lt; 0.05</p>
#> </td></tr>
#> </table>
show_html(mtable123[1:2],
          sig.notes.style=c(width="30ex"))
#> <style>
#>       table#mtable-2 {
#>          border-collapse: collapse; border-style: none; margin: 2ex auto;
#>       } 
#>       table#mtable-2 tr {
#>           border-style: none;
#>       } 
#>       table#mtable-2 td {
#>         padding-top: 1px; 
#>         padding-bottom: 1px; 
#>         padding-left: 0.5ex; 
#>         padding-right: 0.5ex; 
#>         margin: 0px; 
#>         margin-top: 0px;
#>         margin-bottom: 0px;
#>         border-style: none; 
#>         border-width: 0px; 
#>       }
#> table#mtable-2 tr:nth-child(1){
#>      border-top: 2px solid;
#> }
#> 
#> table#mtable-2 tr:nth-child(1),
#> table#mtable-2 tr:nth-child(11){
#>      border-bottom: 1px solid;
#> }
#> table#mtable-2 tr:nth-child(16){
#>      border-bottom: 2px solid;
#> } table#mtable-2 td:nth-child(3),
#> table#mtable-2 td:nth-child(6){
#>          padding-left: 0px;
#>          padding-right: 0px;
#>          text-align: center; 
#>          /*background-color: blue;*/
#>         } table#mtable-2 td:nth-child(2),
#> table#mtable-2 td:nth-child(5){
#>          padding-left: 0.5ex;
#>          padding-right: 0px;
#>          text-align: right; 
#>          /*background-color: red;*/
#>         } table#mtable-2 td:nth-child(4),
#> table#mtable-2 td:nth-child(7){
#>          padding-left: 0px;
#>          padding-right: 0.5ex;
#>          text-align: left; 
#>          /*background-color: green;*/
#>         } 
#> table#mtable-2 td:nth-child(n of .header){
#>          padding-left: 0.5ex;
#>          padding-right: 0.5ex;
#>          text-align: center; 
#>          /*background-color: gray;*/
#>         } 
#> </style><table id="mtable-2">
#> <tr><td colspan="1" class="header"></td><td colspan="3" class="header">Model 1</td><td colspan="3" class="header">Model 2</td></tr>
#> <tr><td>Constant</td><td>30</td><td>.</td><td>628<span class="signif.symbol">***</span></td><td>6</td><td>.</td><td>360<span class="signif.symbol">***</span></td></tr>
#> <tr><td></td><td>(7</td><td>.</td><td>409)</td><td>(1</td><td>.</td><td>252)</td></tr>
#> <tr><td>Percentage of population under 15</td><td>&minus;0</td><td>.</td><td>471<span class="signif.symbol">**</span></td><td></td><td></td><td></td></tr>
#> <tr><td></td><td>(0</td><td>.</td><td>147)</td><td></td><td></td><td></td></tr>
#> <tr><td>Percentage of population over 75</td><td>&minus;1</td><td>.</td><td>934</td><td></td><td></td><td></td></tr>
#> <tr><td></td><td>(1</td><td>.</td><td>041)</td><td></td><td></td><td></td></tr>
#> <tr><td>Real per-capita disposable income</td><td></td><td></td><td></td><td>0</td><td>.</td><td>001</td></tr>
#> <tr><td></td><td></td><td></td><td></td><td>(0</td><td>.</td><td>001)</td></tr>
#> <tr><td>Growth rate of real per-capita disp. income</td><td></td><td></td><td></td><td>0</td><td>.</td><td>529<span class="signif.symbol">*</span></td></tr>
#> <tr><td></td><td></td><td></td><td></td><td>(0</td><td>.</td><td>210)</td></tr>
#> <tr><td>R-squared</td><td>0</td><td>.</td><td>262</td><td>0</td><td>.</td><td>162</td></tr>
#> <tr><td>sigma</td><td>3</td><td>.</td><td>931</td><td>4</td><td>.</td><td>189</td></tr>
#> <tr><td>F</td><td>8</td><td>.</td><td>332</td><td>4</td><td>.</td><td>528</td></tr>
#> <tr><td>p</td><td>0</td><td>.</td><td>001</td><td>0</td><td>.</td><td>016</td></tr>
#> <tr><td>N</td><td>50</td><td></td><td></td><td>50</td><td></td><td></td></tr>
#> <tr><td colspan="7"><p>Significance: *** = p &lt; 0.001; ** = p &lt; 0.01; * = p &lt; 0.05</p>
#> </td></tr>
#> </table>

# Use unicode for minus sign (default)
options(html.use.ampersand=FALSE)
show_html(mtable123)
#> <style>
#>       table#mtable-3 {
#>          border-collapse: collapse; border-style: none; margin: 2ex auto;
#>       } 
#>       table#mtable-3 tr {
#>           border-style: none;
#>       } 
#>       table#mtable-3 td {
#>         padding-top: 1px; 
#>         padding-bottom: 1px; 
#>         padding-left: 0.5ex; 
#>         padding-right: 0.5ex; 
#>         margin: 0px; 
#>         margin-top: 0px;
#>         margin-bottom: 0px;
#>         border-style: none; 
#>         border-width: 0px; 
#>       }
#> table#mtable-3 tr:nth-child(1){
#>      border-top: 2px solid;
#> }
#> 
#> table#mtable-3 tr:nth-child(1),
#> table#mtable-3 tr:nth-child(11){
#>      border-bottom: 1px solid;
#> }
#> table#mtable-3 tr:nth-child(16){
#>      border-bottom: 2px solid;
#> } table#mtable-3 td:nth-child(3),
#> table#mtable-3 td:nth-child(6),
#> table#mtable-3 td:nth-child(9){
#>          padding-left: 0px;
#>          padding-right: 0px;
#>          text-align: center; 
#>          /*background-color: blue;*/
#>         } table#mtable-3 td:nth-child(2),
#> table#mtable-3 td:nth-child(5),
#> table#mtable-3 td:nth-child(8){
#>          padding-left: 0.5ex;
#>          padding-right: 0px;
#>          text-align: right; 
#>          /*background-color: red;*/
#>         } table#mtable-3 td:nth-child(4),
#> table#mtable-3 td:nth-child(7),
#> table#mtable-3 td:nth-child(10){
#>          padding-left: 0px;
#>          padding-right: 0.5ex;
#>          text-align: left; 
#>          /*background-color: green;*/
#>         } 
#> table#mtable-3 td:nth-child(n of .header){
#>          padding-left: 0.5ex;
#>          padding-right: 0.5ex;
#>          text-align: center; 
#>          /*background-color: gray;*/
#>         } 
#> </style><table id="mtable-3">
#> <tr><td colspan="1" class="header"></td><td colspan="3" class="header">Model 1</td><td colspan="3" class="header">Model 2</td><td colspan="3" class="header">Model 3</td></tr>
#> <tr><td>Constant</td><td>30</td><td>.</td><td>628<span class="signif.symbol">***</span></td><td>6</td><td>.</td><td>360<span class="signif.symbol">***</span></td><td>28</td><td>.</td><td>566<span class="signif.symbol">***</span></td></tr>
#> <tr><td></td><td>(7</td><td>.</td><td>409)</td><td>(1</td><td>.</td><td>252)</td><td>(7</td><td>.</td><td>355)</td></tr>
#> <tr><td>Percentage of population under 15</td><td>−0</td><td>.</td><td>471<span class="signif.symbol">**</span></td><td></td><td></td><td></td><td>−0</td><td>.</td><td>461<span class="signif.symbol">**</span></td></tr>
#> <tr><td></td><td>(0</td><td>.</td><td>147)</td><td></td><td></td><td></td><td>(0</td><td>.</td><td>145)</td></tr>
#> <tr><td>Percentage of population over 75</td><td>−1</td><td>.</td><td>934</td><td></td><td></td><td></td><td>−1</td><td>.</td><td>691</td></tr>
#> <tr><td></td><td>(1</td><td>.</td><td>041)</td><td></td><td></td><td></td><td>(1</td><td>.</td><td>084)</td></tr>
#> <tr><td>Real per-capita disposable income</td><td></td><td></td><td></td><td>0</td><td>.</td><td>001</td><td>−0</td><td>.</td><td>000</td></tr>
#> <tr><td></td><td></td><td></td><td></td><td>(0</td><td>.</td><td>001)</td><td>(0</td><td>.</td><td>001)</td></tr>
#> <tr><td>Growth rate of real per-capita disp. income</td><td></td><td></td><td></td><td>0</td><td>.</td><td>529<span class="signif.symbol">*</span></td><td>0</td><td>.</td><td>410<span class="signif.symbol">*</span></td></tr>
#> <tr><td></td><td></td><td></td><td></td><td>(0</td><td>.</td><td>210)</td><td>(0</td><td>.</td><td>196)</td></tr>
#> <tr><td>R-squared</td><td>0</td><td>.</td><td>262</td><td>0</td><td>.</td><td>162</td><td>0</td><td>.</td><td>338</td></tr>
#> <tr><td>sigma</td><td>3</td><td>.</td><td>931</td><td>4</td><td>.</td><td>189</td><td>3</td><td>.</td><td>803</td></tr>
#> <tr><td>F</td><td>8</td><td>.</td><td>332</td><td>4</td><td>.</td><td>528</td><td>5</td><td>.</td><td>756</td></tr>
#> <tr><td>p</td><td>0</td><td>.</td><td>001</td><td>0</td><td>.</td><td>016</td><td>0</td><td>.</td><td>001</td></tr>
#> <tr><td>N</td><td>50</td><td></td><td></td><td>50</td><td></td><td></td><td>50</td><td></td><td></td></tr>
#> <tr><td colspan="10"><p>Significance: *** = p &lt; 0.001; ** = p &lt; 0.01; * = p &lt; 0.05</p>
#> </td></tr>
#> </table>
```
