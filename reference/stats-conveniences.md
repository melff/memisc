# Convenience wrappers for common statistical functions

`Mean()`, `Median()`, etc. are mere wrappers of the functions
[`mean()`](https://rdrr.io/r/base/mean.html),
[`median()`](https://rdrr.io/r/stats/median.html), etc. with the
`na.rm=` optional argument set `TRUE` by default.

## Usage

``` r
Mean(x, na.rm=TRUE, ...)
Median(x, na.rm=TRUE, ...)
Min(x, na.rm=TRUE, ...)
Max(x, na.rm=TRUE, ...)
Weighted.Mean(x, w, ..., na.rm = TRUE)
Var(x, na.rm=TRUE, ...)
StdDev(x, na.rm=TRUE, ...)

Cov(x, y = NULL, use = "pairwise.complete.obs", ...)
Cor(x, y = NULL, use = "pairwise.complete.obs", ...)
Range(..., na.rm = TRUE, finite = FALSE)
```

## Arguments

- x:

  a (numeric) vector.

- y:

  a (numeric) vector or `NULL`.

- w:

  a (numeric) vector of weights.

- na.rm:

  a logical value, see [`mean`](https://rdrr.io/r/base/mean.html).

- use:

  a character string, see [`cor`](https://rdrr.io/r/stats/cor.html).

- ...:

  other arguments, passed to the wrapped functions.

- finite:

  a logical value, see [`range`](https://rdrr.io/r/base/range.html).
