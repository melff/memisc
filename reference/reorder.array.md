# Reorder an Array or Matrix

`reorder.array` reorders an array along a specified dimension according
given names, indices or results of a function applied.

## Usage

``` r
# S3 method for class 'array'
reorder(x,dim=1,names=NULL,indices=NULL,FUN=mean,...)
# S3 method for class 'matrix'
reorder(x,dim=1,names=NULL,indices=NULL,FUN=mean,...)
```

## Arguments

- x:

  An array

- dim:

  An integer specifying the dimension along which `x` should be ordered.

- names:

  A character vector

- indices:

  A numeric vector

- FUN:

  A function that can be used in `apply(x,dim,FUN)`

- ...:

  further arguments, ignored.

## Details

Typical usages are

      reorder(x,dim,names)
      reorder(x,dim,indices)
      reorder(x,dim,FUN)
      

The result of `rename(x,dim,names)` is `x` reordered such that
`dimnames(x)[[dim]]` is equal to the concatenation of those elements of
`names` that are in `dimnames(x)[[dim]]` and the remaining elements of
`dimnames(x)[[dim]]`.

The result of `rename(x,dim,indices)` is `x` reordered along `dim`
according to `indices`.

The result of `rename(x,dim,FUN)` is `x` reordered along `dim` according
to `order(apply(x,dim,FUN))`.

## Value

The reordered object `x`.

## See also

The default method of `reorder` in package `stats`.

## Examples

``` r
  (M <- matrix(rnorm(n=25),5,5,dimnames=list(LETTERS[1:5],letters[1:5])))
#>             a          b           c          d          e
#> A  0.03494413 -0.5377802 -1.05889619  0.1451637  0.2475594
#> B -0.40491716 -0.2110467 -0.92620123 -1.0907214  1.0673434
#> C  0.14117142  0.6550183  1.07285711 -0.6413128 -0.4105769
#> D -0.15592885 -0.3115597 -0.03283362  0.5947005 -0.9088979
#> E  0.33552558 -0.3540890  2.11450519  2.4428499 -0.4895329
  reorder(M,dim=1,names=c("E","A"))
#>             a          b           c          d          e
#> E  0.33552558 -0.3540890  2.11450519  2.4428499 -0.4895329
#> A  0.03494413 -0.5377802 -1.05889619  0.1451637  0.2475594
#> B -0.40491716 -0.2110467 -0.92620123 -1.0907214  1.0673434
#> C  0.14117142  0.6550183  1.07285711 -0.6413128 -0.4105769
#> D -0.15592885 -0.3115597 -0.03283362  0.5947005 -0.9088979
  reorder(M,dim=2,indices=3:1)
#>             c          b           a          d          e
#> A -1.05889619 -0.5377802  0.03494413  0.1451637  0.2475594
#> B -0.92620123 -0.2110467 -0.40491716 -1.0907214  1.0673434
#> C  1.07285711  0.6550183  0.14117142 -0.6413128 -0.4105769
#> D -0.03283362 -0.3115597 -0.15592885  0.5947005 -0.9088979
#> E  2.11450519 -0.3540890  0.33552558  2.4428499 -0.4895329
  reorder(M,dim=1)
#>             a          b           c          d          e
#> B -0.40491716 -0.2110467 -0.92620123 -1.0907214  1.0673434
#> A  0.03494413 -0.5377802 -1.05889619  0.1451637  0.2475594
#> D -0.15592885 -0.3115597 -0.03283362  0.5947005 -0.9088979
#> C  0.14117142  0.6550183  1.07285711 -0.6413128 -0.4105769
#> E  0.33552558 -0.3540890  2.11450519  2.4428499 -0.4895329
  reorder(M,dim=2)
#>            b          e           a           c          d
#> A -0.5377802  0.2475594  0.03494413 -1.05889619  0.1451637
#> B -0.2110467  1.0673434 -0.40491716 -0.92620123 -1.0907214
#> C  0.6550183 -0.4105769  0.14117142  1.07285711 -0.6413128
#> D -0.3115597 -0.9088979 -0.15592885 -0.03283362  0.5947005
#> E -0.3540890 -0.4895329  0.33552558  2.11450519  2.4428499
```
