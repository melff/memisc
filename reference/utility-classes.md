# Named Lists, Lists of Items, and Atomic Vectors

The classes "named.list" and "item.list" are merely some 'helper
classes' for the construction of the classes "data.set" and "importer".

Class "named.list" extends the basic class "list" by an additional slot
"names". Its `initialize` method assures that the names of the list are
unique.

Class "item.list" extends the class "named.list", but does not add any
slots. From "named.list" it differs only by the `initialize` method,
which calls that for "named.list" and makes sure that all elements of
the list belong to class
"[item](https://melff.github.io/memisc/reference/items.md)".

Classes "atomic" and "double" are merely used for method selection.

## Examples

``` r
new("named.list",a=1,b=2)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 

# This should generate an error, since the names
# are not unique.
try(new("named.list",a=1,a=2))
#> Error in validObject(.Object) : 
#>   invalid class “named.list” object: list has duplicate names: "a"

# Another error, one name is missing.
try(new("named.list",a=1,2))
#> Error in validObject(.Object) : 
#>   invalid class “named.list” object: element 2 is unnamed

# Also an error, the resulting list would be unnamed.
try(new("named.list",1,2))
#> Error in validObject(.Object) : 
#>   invalid class “named.list” object: list is unnamed


new("item.list",a=1,b=2)
#> $a
#> 
#> Item (measurement: interval, type: double, length = 1) 
#> 
#>  [1:1] 1
#> 
#> $b
#> 
#> Item (measurement: interval, type: double, length = 1) 
#> 
#>  [1:1] 2
#> 

# Also an error: "item.list"s are "named.lists",
# and here the names would be non-unique.
try(new("item.list",a=1,a=2))
#> Error in validObject(.Object) : 
#>   invalid class “item.list” object: list has duplicate names: "a"
```
