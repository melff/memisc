\name{tibbles}
\alias{as_tibble.data.set}
\alias{as.item,labelled-method}
\alias{as.item,haven_labelled-method}
\alias{as.item,haven_labelled_spss-method}
\alias{as_haven}
\alias{as_haven,data.set-method}
\alias{as_haven,item.vector-method}
\alias{as.data.set,tbl_df-method}
\title{Interface to Packages 'tibble' and 'haven'}
\description{
  A \code{as_tibble} method (\code{as_table.data.set}) allows to transform \code{"data.set"} objects
  into objects of class \code{"tbl_df"} as defined by the package
  "tibble".

  \code{as.item} methods for objects of classes \code{"haven_labelled"}
  and \code{"have_labelled_spss"} allow to transform a "tibble" imported
  using \code{read_dta}, \code{read_spss}, etc. from the package "haven"
  into an object of class \code{"data.set"}.

  \code{as_haven} can be used to transform \code{"data.set"} objects
  into objects of class \code{"tbl_df"} with that additional information
  that objects imported using the "haven" package usually have, i.e.
  variable labels and value labels (as the \code{"label"} and
  \code{"labels"} attributes of the columns).
}
\usage{
as_tibble.data.set(x,\dots)
\S4method{as.item}{haven_labelled}(x,\dots)
\S4method{as.item}{haven_labelled_spss}(x,\dots)
as_haven(x,\dots)
\S4method{as_haven}{data.set}(x,user_na=FALSE,\dots)
\S4method{as_haven}{item.vector}(x,user_na=FALSE,\dots)
\S4method{as.data.set}{tbl_df}(x,row.names=NULL,\dots)
}
\arguments{
  \item{x}{for \code{as_tibble.data.set} and \code{as_haven}, an object
    of class \code{"data.set"}; for \code{as.item}, an object of class
    \code{"haven_labelled"} or \code{"haven_labelled_spss"};
    an object of class \code{"tbl_df"} for \code{as.data.set}.
  }
  \item{user_na}{logical; if \code{TRUE} then the resulting vectors
    have an \code{"na_values"} and/or \code{"na_range"} attribute.}
  \item{row.names}{\code{NULL} or an optional character vector of row names.}
  \item{\dots}{further arguments, passed through to other the the
    \code{as_tibble} method for lists, or ignored.}
}
\value{
  \code{as_tibble.data.set} and the \code{"data.set"}-method of
  \code{as_haven} return a "tibble". The \code{"item.vector"}-method
  (which is for internal use only) returns a vector with S3 class either
  \code{"haven_labelled"} or \code{"haven_labelled_spss"}. 
}
