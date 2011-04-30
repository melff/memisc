\name{Simulate}
\alias{Simulate}
\alias{interrupt}
\title{Support for Simulation Experiments}
\description{
  \code{Simulate} is a function to simplify simulation studies.
  It can be used to conduct Monte Carlo studies of statistical estimators,
  discrete event, and agent based simulations.
  }
\usage{
Simulate(step,
    conditions = NULL,
    start = NULL,
    cleanup = NULL,
    \dots,
    nsim = 1,
    seed = NULL,
    trace=0,
    keep.data=TRUE,
    keep.states=FALSE,
    keep.seed = !is.null(seed),
    restore.seed = !is.null(seed),
    bucket = default_bucket
    )

# signal an interrupt condition
interrupt(msg=NULL)
}
\arguments{
  \item{step}{an expression that produces simulation results for each replication;
    can be a function call, or a braced expression that ``returns''
    a value like a function body.
    }
  \item{conditions}{an optional
    data frame or object coerceable into a data frame.
    Each row of this data frame defines an experimental condition.
    }
  \item{start}{either NULL or an expression that computes starting values
    for \code{step}.
  }
  \item{cleanup}{either NULL or an expression does some cleaning up after
    the exectution of all \code{step}s.
  }
  \item{\dots}{other substitutions for \code{step},
    held fixed in the
    simulation experiment}
  \item{nsim}{an integer value; the number of replication in each experimental setting. If
    \code{nsim} is infinite or NA, \code{step} is replicated (in each setting)
    until either a user interrupt is signalled (\kbd{CTRL-C} is pressed) or
    \code{interrupt} is called.
    }
  \item{seed}{either NULL or an integer value suitable for \code{set.seed}.
    Note that the random state before the call to \code{Simulate} is restored.
    }
  \item{trace}{an integer value determining the amount of information
    output during the simulation process. If \code{trace} equals zero
    nothing is reported during the simulation run.
    Otherwise, the replication number is output for each
    multiple of \code{trace}.
    }
  \item{keep.data}{logical value; if TRUE, return values of the expression
    in \code{step} are collected into a data fame.}
  \item{keep.states}{logical value; if TRUE, a list of all variables
    defined in \code{step} (after execution of \code{cleanup} if present) is returned.}
  \item{keep.seed}{logical value; if TRUE, the state of the random number generator
    is saved in an attribute "seed" of the return value of \code{Simulate}.
  }
  \item{restore.seed}{logical value; if TRUE, the state of the random number generator
    is restored after conducting the simulations.
  }
  \item{bucket}{a function that returns a \code{\link{bucket}} object, in which
    simulation results are collected.}
  \item{msg}{a character string, the message shown if an interrupt condition is
    signalled.}
}
\details{
  \code{Simulate} calls or evaluates its first argument, \code{step},
  or, if a \code{conditions} argument is given, \code{nsim} times
  for each row of the \code{conditions} data frame.

  Before repeatingly evaluating \code{step}, the expression \code{start}, if present,
  is evaluated, which may be used to create starting values for
  a simulatation of to setup up the scenery for an agent-based simulation.
  \emph{After} repeatingly evaluating \code{step}, the expression \code{cleanup},
  if present, is evaluated.

  If \code{restore.seed} is given, the state of the random generator
  is saved before conducting the simulation and restored afterwards. Therefore
  \code{step}, \code{start}, or \code{cleanup} may call \code{set.seed}
  without affecting the generation of random numbers after a call to
  \code{Simulate}.

  \code{interrupt} raises an interrupt condition, which acts like
  a user interrupt.

  Note that if an interrupt condition is signalled during a (replicated)
  evaluation of \code{step} the results of previous replications are
  still saved and \code{Simulate} jumps to the next condition of
  the simulation experiment (if there is any). That is, if a simulation
  is interrupted by the user because it takes too long, the results so far
  produced by the simulation are not lost.

  On the other hand, \code{interrupt} can be used to determine at run-time
  how often \code{step} is evaluated.

}
\value{
    A data frame that contains experimental conditions and simulation results.
}
\examples{
Normal.example <- function(mean=0,sd=1,n=10){
  x <- rnorm(n=n,mean=mean,sd=sd)
  c(
    Mean=mean(x),
    Median=median(x),
    Var=var(x)
  )
}

Normal.simres <- Simulate(
    Normal.example(mean,sd,n),
    expand.grid(
          mean=0,
          sd=c(1,10),
          n=c(10,100)
          ),
    nsim=200,
    trace=50)

genTable(sd(Median)~sd+n,data=Normal.simres)

expr.simres <- Simulate(
      median(rnorm(n,mean,sd)),
      expand.grid(
          n=c(10,100),
          mean=c(0,1),
          sd=c(1,10)
      ),
    nsim=200,
    trace=50
    )

genTable(c(mean(result),sd(result))~sd+n+mean,data=expr.simres)


\dontrun{
## This takes a little bit longer
lm.example <- function(a=0,b=1,n=101,xrange=c(-1,1),serr=1){
  x <- seq(from=xrange[1],to=xrange[2],length=n)
  y <- a + b*x + rnorm(n,sd=serr)
  lm.res <- lm(y~x)
  coef <- lm.res$coef
  names(coef) <- c("a","b")
  coef
}

lm.simres <- Simulate(
      lm.example(n=n,serr=serr),
      expand.grid(
      serr=c(0.1,1,10),
      n=c(11,101,501)
      ),
      nsim=200,
      trace=50
    )
genTable(c(sd(a),sd(b))~serr+n,data=lm.simres)
}




}
\keyword{misc}
