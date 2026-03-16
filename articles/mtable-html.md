# Tables of model estimates

First, we load the package and estimate some regression models.

``` r
library(memisc)
lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
```

Next, we put them together into a table with
[`mtable()`](https://melff.github.io/memisc/reference/mtable.md).

``` r
mtable123 <- mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2,
                    summary.stats=c("sigma","R-squared","F","p","N"))
```

Third, we improve the appearance of the results by relabeling the
`mtable` object.

``` r
mtable123 <- relabel(mtable123,
                      "(Intercept)" = "Constant",
                      pop15 = "Percentage of population under 15",
                      pop75 = "Percentage of population over 75",
                      dpi = "Real per-capita disposable income",
                      ddpi = "Growth rate of real per-capita disp. income"
)
```

Finally we view the results in HTML format.

``` r
show_html(mtable123)
```

|                                                                     |         |     |           |         |     |           |         |     |           |
|---------------------------------------------------------------------|---------|-----|-----------|---------|-----|-----------|---------|-----|-----------|
|                                                                     | Model 1 |     |           | Model 2 |     |           | Model 3 |     |           |
| Constant                                                            | 30      | .   | 628\*\*\* | 6       | .   | 360\*\*\* | 28      | .   | 566\*\*\* |
|                                                                     | (7      | .   | 409\)     | (1      | .   | 252\)     | (7      | .   | 355\)     |
| Percentage of population under 15                                   | −0      | .   | 471\*\*   |         |     |           | −0      | .   | 461\*\*   |
|                                                                     | (0      | .   | 147\)     |         |     |           | (0      | .   | 145\)     |
| Percentage of population over 75                                    | −1      | .   | 934       |         |     |           | −1      | .   | 691       |
|                                                                     | (1      | .   | 041\)     |         |     |           | (1      | .   | 084\)     |
| Real per-capita disposable income                                   |         |     |           | 0       | .   | 001       | −0      | .   | 000       |
|                                                                     |         |     |           | (0      | .   | 001\)     | (0      | .   | 001\)     |
| Growth rate of real per-capita disp. income                         |         |     |           | 0       | .   | 529\*     | 0       | .   | 410\*     |
|                                                                     |         |     |           | (0      | .   | 210\)     | (0      | .   | 196\)     |
| R-squared                                                           | 0       | .   | 262       | 0       | .   | 162       | 0       | .   | 338       |
| sigma                                                               | 3       | .   | 931       | 4       | .   | 189       | 3       | .   | 803       |
| F                                                                   | 8       | .   | 332       | 4       | .   | 528       | 5       | .   | 756       |
| p                                                                   | 0       | .   | 001       | 0       | .   | 016       | 0       | .   | 001       |
| N                                                                   | 50      |     |           | 50      |     |           | 50      |     |           |
| Significance: \*\*\* = p \< 0.001; \*\* = p \< 0.01; \* = p \< 0.05 |         |     |           |         |     |           |         |     |           |

The results can be written into an HTML file using
`write_html(mtable123,file="mtable123.html")` or the like. MS Word or
LibreOffice can import such a file.
