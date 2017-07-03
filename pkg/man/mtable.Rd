\name{mtable}
\alias{mtable}
\alias{relabel.memisc_mtable}
\alias{format.memisc_mtable}
\alias{print.memisc_mtable}
\alias{write.mtable}
\alias{toLatex.memisc_mtable}
\alias{dim.memisc_mtable}
\alias{dimnames.memisc_mtable}
\alias{[.memisc_mtable}
\alias{c.memisc_mtable}

\title{Comparative Table of Model Estimates}
\description{
  \code{mtable} produces a table of estimates for several models.
  }
\usage{
mtable(\dots,coef.style=getOption("coef.style"),
    summary.stats=TRUE,
    signif.symbols=getOption("signif.symbols"),
    factor.style=getOption("factor.style"),
    show.baselevel=getOption("show.baselevel"),
    baselevel.sep=getOption("baselevel.sep"),
    getSummary=eval.parent(quote(getSummary)),
    float.style=getOption("float.style"),
    digits=min(3,getOption("digits")),
    sdigits=min(1,digits),
    gs.options=NULL
  )
\method{relabel}{memisc_mtable}(x, \dots, gsub = FALSE, fixed = !gsub, warn = FALSE)

\method{format}{memisc_mtable}(x,target=c("print","LaTeX","HTML","delim"),
    \dots
    )

\method{print}{memisc_mtable}(x,
    center.at=getOption("OutDec"),
    topsep="=",bottomsep="=",sectionsep="-",\dots)

write.mtable(object,file="",
             format=c("delim","LaTeX","HTML"),\dots)

\method{toLatex}{memisc_mtable}(object,...)
}
\arguments{
     \item{\dots}{as argument to \code{mtable}: several model objects, e.g. of class 
     \code{\link{lm}}; as argument to \code{print.memisc_mtable},
     \code{toLatex.memisc_mtable}, \code{write.memisc_mtable}: further arguments
     passed to \code{format.memisc_mtable}; as argument to \code{format.memisc_mtable}:
     further arguments passed to \code{format.default};
     as argument to \code{relabel.memisc_mtable}: further arguments
     passed to \code{\link{dimrename}}.
     }
     \item{coef.style}{a character string which specifies the style of
        coefficient values, whether standard errors, Wald/t-statistics,
         or significance levels are reported, etc. See \code{\link{coef.style}}.}
     \item{summary.stats}{if \code{FALSE}, no summary statistics
        are repored. If \code{TRUE}, all summary statistics
        produced by \code{getSummary} are reported.
        This argument may also contain a character vector with
        the names of the summary statistics to report}
     \item{signif.symbols}{a named numeric vector to specify
          the "significance levels" and corresponding symbols. 
          The numeric elements define the significance levels,
          the attached names define the associated symbols.}
     \item{factor.style}{a character string that specifies the style in
        which factor contrasts are labled. See \code{\link{factor.style}}.}
     \item{show.baselevel}{logical; determines whether base levels of factors are indicated
      for dummy coefficients}
     \item{baselevel.sep}{character that is used to separate the base level from the 
           level that a dummy variable represents}
     \item{getSummary}{a function that computes model-related statistics that
        appear in the table. See \code{\link{getSummary}}.}
     \item{float.style}{default format for floating point numbers if
        no format is specified by \code{coef.style}}.
     \item{digits}{number of significant digits if not specified by
        the template returned from \code{\link{getCoefTemplate}}
        \code{\link{getSummaryTemplate}}
        }
     \item{sdigits}{integer; number of digits after decimal dot for summary statistics.}
     \item{gs.options}{an optional list of arguments passed on to
           \code{getSummary}}
     \item{x, object}{an object of class \code{mtable}}
     \item{gsub, warn, fixed}{logical values, see \code{\link{relabel}}}
     \item{target}{a character string which indicates the target format.
     Currenlty the targets
     "print" (see \code{\link{mtable_format_print}}),
     "LaTeX" (see \code{\link{mtable_format_latex}}),
     "HTML" (see \code{\link{mtable_format_html}}), and
     "delim" (see \code{\link{mtable_format_delim}})
     are supported.
     }
     \item{center.at}{a character string on which resulting values are centered.
        Typically equal to ".". This is the default when \code{forLaTeX==TRUE}.
        If \code{NULL}, reported values are not centered.}
     \item{topsep}{a character string that is recycled to a top rule.}
     \item{bottomsep}{a character string that is recycled to a bottom rule.}
     \item{sectionsep}{a character string that is recycled to seperate coefficients
        from summary statistics.}
     \item{file}{name of the file where to write to; defaults to console output.}
     \item{format}{character string that specifies the desired format.}
}
\details{
  \code{mtable} constructs a table of estimates for regression-type models.
  \code{format.memisc_mtable} formats suitable for use with output or conversion functions
  such as \code{print.memisc_mtable}, \code{toLatex.memisc_mtable}, or \code{write.memisc_mtable}.
}
\value{
    A call to \code{mtable} results in an object of class \code{"mtable"}
    with the following components:

    \item{coefficients}{a list that contains the model coefficients,}
    \item{summaries}{a matrix that contains the model summaries,}
    \item{calls}{a list of calls that created the model estimates
          being summarised.}

}
\examples{
#### Basic workflow

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

# This produces output in tab-delimited format:
write.mtable(mtable123)

\dontrun{
# This produces output in tab-delimited format:
file123 <- "mtable123.txt"
write.mtable(mtable123,file=file123)
file.show(file123)
# The contents of this file can be pasted into Word
# and converted into a Word table.
}
\dontshow{toLatex(mtable123)}
\dontrun{texfile123 <- "mtable123.tex"
write.mtable(mtable123,format="LaTeX",file=texfile123)
file.show(texfile123)}

#### Examples with UC Berkeley data

berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)

berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

mtable(berk0,summary.stats=c("Deviance","N"))
mtable(berk1,summary.stats=c("Deviance","N"))

mtable(berk0,berk1,berk2,summary.stats=c("Deviance","N"))

mtable(berk0,berk1,berk2,
          coef.style="horizontal",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="stat",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci.se",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci.se.horizontal",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci.p.horizontal",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci.horizontal",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="all",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="all.nostar",
          summary.stats=c("Deviance","AIC","N"))

mtable(by(berkeley,berkeley$Dept,
  function(x)glm(cbind(Admitted,Rejected)~Gender,
        data=x,family="binomial")),
      summary.stats=c("Likelihood-ratio","N"))


mtable(By(~Gender,
  glm(cbind(Admitted,Rejected)~Dept,
        family="binomial"),
        data=berkeley),
      summary.stats=c("Likelihood-ratio","N"))

berkfull <- glm(cbind(Admitted,Rejected)~Dept/Gender - 1,
                      data=berkeley,family="binomial")
relabel(mtable(berkfull),Dept="Department",gsub=TRUE)

#### Array-like semantics

mtable123 <- mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2,
    summary.stats=c("sigma","R-squared","F","p","N"))

dim(mtable123)
dimnames(mtable123)
mtable123[c("dpi","ddpi"),
          c("Model 2","Model 3")]

#### Concatention
mt01 <- mtable(lm0,lm1,summary.stats=c("R-squared","N"))
mt12 <- mtable(lm1,lm2,summary.stats=c("R-squared","F","N"))
c(mt01,mt12) # not that this makes sense, but ...
c("Group 1"=mt01,
  "Group 2"=mt12)
}
\keyword{misc}
