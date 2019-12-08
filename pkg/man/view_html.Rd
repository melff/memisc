\name{view_html}
\alias{view_html}
\title{HTML Output for 'view.}
\description{An alternative to 'View' for use with 'view'.}
\usage{
view_html(x,title=deparse(substitute(x)),output,\dots)
}
\arguments{
  \item{x}{the result of \code{viewPrep}, a matrix of character strings.
  }
  \item{title}{an optional character string; shown as the title of the
    display.}
  \item{output}{a function or the name of a function. It determines how
    where the HTML code is directed to.

    If the working environment is RStudio, the default value is
    \code{"file.show"}. In other interactive environments it is
    \code{"browser"}. In non-interactive sessions it is 
    \code{"stdout"}.

    If \code{output} equals \code{"browser"} the generated HTML code
    is shown using \code{\link[utils]{browseURL}}. If \code{output}
    equals \code{"stdout"} the HTML code is written to the console
    output window. If \code{output} equals \code{"file.show"}, the
    function \code{file.show} is used.

    If \code{view_html} is called within a \emph{Jupyter} session,
    the HTML code created is envelopped in a pair of \code{<div>} tags
    and included into the Jupyter output.}
  \item{\dots}{other arguments; ignored.}
}
\examples{
\dontrun{
    example(data.set)
    view(Data,vfunc=view_html)
}
}
