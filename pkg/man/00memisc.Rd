\name{Memisc}
\docType{package}
\alias{memisc-package}
\alias{memisc}
\title{Introduction to the `memisc' Package}
\description{This package collects an assortment of tools that are intended to make
work with \code{R} easier for the author of this package
and are submitted to the public in the hope that they will be also be useful to others.

The tools in this package can be grouped into five major categories:
\itemize{
  \item Data preparation and management
  \item Data analysis
  \item Presentation of analysis results
  \item Simulation
  \item Programming
}
}
\section{Data preparation and management}{
\strong{Survey Items}

  \code{memisc} provides facilities to work with what users from other
  packages like SPSS, SAS, or Stata know as `variable labels', `value labels'
  and `user-defined missing values'. In the context of this package these
  aspects of the data are represented by the \code{"description"},
  \code{"labels"}, and \code{"missing.values"} attributes of a
  data vector. 
  These facilities are useful, for example, if you work with
  survey data that contain coded items like vote intention that
  may have the following structure:

  Question: ``If there was a parliamentary election next tuesday, which party would you vote for?''
  \tabular{rl}{
    1 \tab Conservative Party \cr
    2 \tab Labour Party \cr
    3 \tab Liberal Democrat Party \cr
    4 \tab Scottish Nation Party \cr
    5 \tab Plaid Cymru \cr
    6 \tab Green Party \cr
    7 \tab British National Party \cr
    8 \tab Other party \cr
    96 \tab Not allowed to vote \cr
    97 \tab Would not vote \cr
    98 \tab Would vote, do not know yet for which party \cr
    99 \tab No answer 
  }
  A statistical package like SPSS allows to
  attach labels like `Conservative Party', `Labour Party', etc.
  to the codes 1,2,3, etc. and to mark
  mark the codes 96, 97, 98, 99
  as `missing' and thus to exclude these variables from statistical
  analyses. \code{memisc} provides similar facilities.
  Labels can be attached to codes by calls like \code{\link{labels}(x) <- something}
  and expendanded by calls like \code{\link{labels}(x) <- \link{labels}(x) + something},
  codes can be marked as `missing' by
  calls like \code{\link{missing.values}(x) <- something} and
  \code{\link{missing.values}(x) <- \link{missing.values}(x) + something}.

  \code{memisc} defines a class called "data.set", which is similar to the class "data.frame".
  The main difference is that it is especially geared toward containing survey item data.
  Transformations of and within "data.set" objects retain the information about
  value labels, missing values etc. Using \code{as.data.frame} sets the data up for
  \emph{R}'s statistical functions, but doing this explicitely is seldom necessary.
  See \code{\link{data.set}}.

  \strong{More Convenient Import of External Data}

  Survey data sets are often relative large and contain up to a few thousand variables.
  For specific analyses one needs however only a relatively small subset of these variables.
  Although modern computers have enough RAM to load such data sets completely into an R session,
  this is not very efficient having to drop most of the variables after loading. Also, loading
  such a large data set completely can be time-consuming, because R has to allocate space for
  each of the many variables. Loading just the subset of variables really needed for an analysis
  is more efficient and convenient - it tends to be much quicker. Thus this package provides
  facilities to load such subsets of variables, without the need to load a complete data set.
  Further, the loading of data from SPSS files is organized in such a way that all informations
  about variable labels, value labels, and user-defined missing values are retained.
  This is made possible by the definition of \code{\link{importer}} objects, for which
  a \code{\link{subset}} method exists. \code{\link{importer}} objects contain only
  the information about the variables in the external data set but not the data.
  The data itself is loaded into memory when the functions \code{subset} or \code{\link{as.data.set}}
  are used.

  \strong{Recoding}
  
  \code{memisc} also contains facilities for recoding
  survey items. Simple recodings, for example collapsing answer
  categories, can be done using the function \code{\link{recode}}. More
  complex recodings, for example the construction of indices from
  multiple items, and complex case distinctions, can be done
  using the function \code{\link{cases}}. This function may also
  be useful for programming, in so far as it is a generalization of
  \code{\link{ifelse}}.

  \strong{Code Books}

  There is a function \code{\link{codebook}} which produces a code book of an
  external data set or an internal "data.set" object. A codebook contains in a
  conveniently formatted way concise information about every variable in a data set,
  such as which value labels and missing values are defined and some univariate statistics.

  An extended example of all these facilities is contained in the vignette "anes48",
  and in \code{demo(anes48)}
  
  }

\section{Data Analysis}{
\strong{Tables and Data Frames of Descriptive Statistics}

  \code{\link{genTable}} is a generalization of \code{\link{xtabs}}:
  Instead of counts, also descriptive statistics like means or variances
  can be reported conditional on levels of factors. Also conditional
  percentages of a factor can be obtained using this function.

  In addition a \code{\link[=aggregate.formula]{formula}} method for the \code{\link{aggregate}} generic
  function is provided, see. It has the same syntax as \code{genTable}, but
  gives a data frame of descriptive statistics instead of a \code{table}
  object. 

\strong{Per-Subset Analysis}

  \code{\link{By}} is a variant of the
  standard function \code{\link[base]{by}}: Conditioning factors
  are specified by a formula and are
  obtained from the data frame the subsets of which are to be analysed.
  Therefore there is no need to \code{\link{attach}} the data frame
  or to use the dollar operator.
  

\strong{Graphical Model Comparison}

  \code{\link{Termplot}} is a variant and an extension of \code{\link[stats]{termplot}}:
  The plots are similar to those of \code{termplot} but uses \code{\link[lattice]{lattice}}
  graphics. Also \code{Termplot} can be used on more than one model and allows
  to compare the fit of linear or non-linear effect specifications of different
  models.

  Use \code{example(Termplot)} or \code{demo(Termplot)} for an example.
}
\section{Presentation of Results of Statistical Analysis}{
\strong{Publication-Ready Tables of Coefficients}

Journals of the Political and Social Sciences usually require
that estimates of regression models are presented in the following
form:
\preformatted{
    ==================================================
                    Model 1     Model 2     Model 3
    --------------------------------------------------
    Coefficients
    (Intercept)     30.628***    6.360***   28.566***
                    (7.409)     (1.252)     (7.355)
    pop15           -0.471**                -0.461**
                    (0.147)                 (0.145)
    pop75           -1.934                  -1.691
                    (1.041)                 (1.084)
    dpi                          0.001      -0.000
                                (0.001)     (0.001)
    ddpi                         0.529*      0.410*
                                (0.210)     (0.196)
    --------------------------------------------------
    Summaries
    R-squared         0.262       0.162       0.338
    adj. R-squared    0.230       0.126       0.280
    N                50          50          50
    ==================================================
}

Such tables of coefficient estimates can be produced
by \code{\link{mtable}}. To see some of the possibilities of
this function, use \code{example(mtable)}.


\strong{LaTeX Representation of R Objects}

Output produced by \code{\link{mtable}} can be transformed into
LaTeX tables by an appropriate method of the generic function
\code{\link[utils]{toLatex}} which is defined in the package
\code{utils}. In addition, \code{memisc} defines \code{toLatex} methods
for matrices and \code{\link[stats]{ftable}} objects. Note that
results produced by \code{\link{genTable}} can be coerced into
\code{\link[stats]{ftable}} objects. Also, a default method
for the \code{toLatex} function is defined which coerces its
argument to a matrix and applies the matrix method of \code{toLatex}.
}
\section{Simulation}{
The \code{memisc} package defines a function \code{\link{Simulate}},
which can be used to conduct simulation experiments: For a given
number of replications and given sets of parameters (which can
be varied across experimental conditions) data are generated and
can summarized afterwards by other methods.

Use \code{example(Simulate)}, \code{demo(monte.carlo)}, \code{demo(lm.monte.carlo)},
\code{demo(random.walk)}, or \code{demo(schellings)} for examples.
}
\section{Programming}{
\strong{Looping over Variables}

  Sometimes users want to contruct loops that run over variables rather than values.
  For example, if one wants to set the missing values of a battery of items.
  For this purpose, the package contains the function \code{\link{foreach}}.
  To set 8 and 9 as missing values for the items \code{knowledge1},
  \code{knowledge2}, \code{knowledge3}, one can use
  \preformatted{
    foreach(x=c(knowledge1,knowledge2,knowledge3),
        missing.values(x) <- 8:9)
  }

\strong{Changing Names of Objects and Labels of Factors}

  \code{R} already makes it possible to change the names of an object.
  Substituting the \code{\link[base]{names}} or \code{\link[base]{dimnames}}
  can be done with some programming tricks. This package defines
  the function \code{\link{rename}},
  \code{\link{dimrename}}, \code{\link{colrename}}, and \code{\link{rowrename}}
  that implement these tricks in a convenient way, so that programmers
  (like the author of this package) need not reinvent the weel in
  every instance of changing names of an object.

\strong{Dimension-Preserving Versions of \code{lapply} and \code{sapply}}

If a function that is involved in a call to 
\code{\link[base]{sapply}} returns a result an array or a matrix, the
dimensional information gets lost. Also, if a list object to which
\code{\link[base]{lapply}} or \code{\link[base]{sapply}} are applied
have a dimension attribute, the result looses this information.
The functions \code{\link{Lapply}} and
\code{\link{Sapply}} defined in this package preserve such
dimensional information.

\strong{Combining Vectors and Arrays by Names}

The generic function \code{\link{collect}} collects several objects of the
same mode into one object, using their names, \code{rownames},
\code{colnames} and/or \code{dimnames}. There are methods for
atomic vectors, arrays (including matrices), and data frames.
For example
\preformatted{
a <- c(a=1,b=2)
b <- c(a=10,c=30)
collect(a,b)
}
leads to
\preformatted{
   x  y
a  1 10
b  2 NA
c NA 30
}

\strong{Reordering of Matrices and Arrays}

  The \code{memisc} package includes a \code{\link{reorder}}
  method for arrays and matrices. For example, the matrix
  method by default reorders the rows of a matrix according the results
  of a function.
}

\section{Other Facilities}{
\code{\link{mkHelpDir}} is a modification of \code{\link[utils]{make.packages.html}}
that constructs HTML help pages permanently instead of temporarily.
}

\keyword{misc}