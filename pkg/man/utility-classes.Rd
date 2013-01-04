\name{Utility classes}
\alias{named.list}
\alias{item.list}
\alias{named.list-class}
\alias{item.list-class}
\alias{initialize,item.list-method}
\alias{initialize,named.list-method}
\alias{coerce,data.set,named.list-method}
\alias{show,named.list-method}
\alias{atomic-class}
\alias{double-class}
\title{Named Lists, Lists of Items, and Atomic Vectors}
\description{
  The classes "named.list" and "item.list" are merely some
  'helper classes' for the construction of the classes "data.set"
  and "importer".

  Class "named.list" extends the basic class "list" by an additional
  slot "names". Its \code{initialize} method assures that the names
  of the list are unique.
  
  Class "item.list" extends the class "named.list", but does not
  add any slots. From "named.list" it differs only by the 
  \code{initialize} method, which calls that for "named.list"
  and makes sure that all elements of the list belong to 
  class "\link{item}".
  
  Classes "atomic" and "double" are merely used for
  method selection.
}
\examples{
new("named.list",a=1,b=2)

# This should generate an error, since the names
# are not unique.
try(new("named.list",a=1,a=2))

# Another error, one name is missing.
try(new("named.list",a=1,2))

# Also an error, the resulting list would be unnamed.
try(new("named.list",1,2))


new("item.list",a=1,b=2)

# Also an error: "item.list"s are "named.lists",
# and here the names would be non-unique.
try(new("item.list",a=1,a=2))
}