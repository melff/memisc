\name{NEWS}
\title{memisc News}
\encoding{UTF-8}

\section{Version 0.97 }{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{spss.system.file} and \code{spss.portable.file} gain a \code{tolower=} argument 
      that defaults to \code{TRUE}, which 
      allows to change annoying all-upper-case variable names to lower case
      \item New generic function \code{Iconv()} that allows to change the character enconding of variable
      descriptions and value labels. It has methods for \code{"data.set"}, \code{"importer"}, \code{"item"},
      \code{"annotation"}, and \code{"value.label"} objects.
      \item There is now a method of \code{as.character()} for \code{"codebook"} objects and a convenience
      function \code{Write()} with methods for \code{"codebook"} and \code{"description"} to make it more
      convenient to direct the output of \code{codebook()} and \code{description()} into text files.
      \item A method for \code{"merMod"} objects of the \code{getSummary()} generic function. 
      \code{mtable()} now should be able (again) to handle estimation results produced by \code{lmer()} and \code{glmer()} from 
      package 'lme4'.
      \item \code{recode()} handles character vectors in a more convenient way: They are converted
    into factors with sorted unique values (after recoding) as levels.
    }
  }
  \subsection{USER-VISIBLE CHANGES}{
   \itemize{
     \item \code{getSummary.expCoef} is renamed into \code{getSummary_expCoef}.
   }
  }
  \subsection{DEFUNCT}{
    \itemize{
      \item S3 method \code{aggregate.formula} has been removed from the package to avoid
      clash with method of the same name in the \code{base} package. The function \code{Aggregate}
      can be used instead.
      \item Removed \code{include}, \code{uninclude}, and \code{detach.sources} as these are flagged
      as modifying the global namespace.
    }
  }
}