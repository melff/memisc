\name{view}
\alias{view}
\alias{viewPrep}
\alias{viewPrep.data.set}
\alias{viewPrep.data.frame}
\alias{viewPrep.descriptions}
\alias{viewPrep.codeplan}
\alias{viewPrep.importer}
\title{A Generic Viewing Function}
\description{
  The function \code{view} provides generic interface to the non-generic
  function \code{View}.

  In contrast to the implementation of \code{View} provided by either
  basic \emph{R} or \emph{RStudio}, this function can be extended to
  handle new kinds of objects by defining \code{viewPrep} methods for
  them. Further, \code{view} can be adapted to other GUIs by specifying
  the \code{"vfunc"} option or the \code{vfunc=} optional argument.
    
  Internally, \code{view} usues the generic function \code{viewPrep}
  to prepare data so it can be passed on to the (non-generic) function
  \code{View} or (optionally) a different graphical user interface
  function that can be used to display matrix- or data frame-like
  objects.  

  The \code{vfunc} argument determines how the result of \code{viewPrep}
  is displayed. Its default is the function \code{View}, but an
  alternative is \code{\link{view_html}} which creates and displays an HTML grid.
}
\usage{
view(x,
     title=deparse(substitute(x)),
     vfunc=getOption("vfunc","View"),
     \dots)

# The internal generic, not intended to be used by the end-user.
viewPrep(x,title,\dots)
\method{viewPrep}{data.set}(x,title,\dots)
\method{viewPrep}{data.frame}(x,title,\dots)
\method{viewPrep}{descriptions}(x,title,\dots)
\method{viewPrep}{codeplan}(x,title,compact=FALSE,\dots)
\method{viewPrep}{importer}(x,title,compact=TRUE,\dots)
}
\arguments{
  \item{x}{an object, e.g. a data frame, data.set, or importer.
  }
  \item{title}{an optional character string; shown as the title of the
    display.}
  \item{vfunc}{a character string; a name of a GUI function to call
    with the results of \code{viewPrep()}}
  \item{compact}{a logical value; should the codeplan be shown in a
    compact form - one line per variable - or in a more expanive form -
    one line per labelled value?}
  \item{\dots}{further arguments; \code{view()} passes them on to
    \code{viewPrep}.}
}
\examples{
\dontrun{
    example(data.set)
    view(Data)
    view(description(Data))
    view(codeplan(Data))
    # Note that this file is *not* included in the package
    # and has to be obtained from GESIS in order to run the 
    # following
    ZA7500sav <- spss.file("ZA7500_v2-0-0.sav")
    view(ZA7500sav)
}
}
