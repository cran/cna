\name{condTbl}
\alias{condTbl}
\alias{msc}
\alias{asf}
\alias{csf}
\alias{print.condTbl}
\alias{as.condTbl}

\title{
Extract conditions and solutions from a cna object.
}

\description{
\code{cna}, \code{asf} and \code{csf} extract minimally sufficient conditions, atomic solution formulas and complex solution formulas from a \code{cna} object, 
along with their consistencies and coverages.

\code{as.condTbl} reshapes the output produced by a call to \code{\link{condition}} in such a way to make it identical to that returned by \code{msc} and \code{asf}.
}

\usage{
msc(x)
asf(x)
csf(x, asfx = asf(x))

\method{print}{condTbl}(x, digits = 3, ...)

as.condTbl(condlst, ...)
}

\arguments{
  \item{x}{
In \code{msc}, \code{asf} and \code{csf} : A \code{\link{cna}}-object
}
  \item{asfx}{
A \dQuote{condTbl} object resulting from calling \code{asf}. 
The determination of the complex solution formulas requires the atomic solution formulas, 
execution of \code{csf} can thus be made slightly more if the asf have been extracted from the cna before.
}
  \item{digits}{
Number of digits to print in consistencies and coverages.
}
  \item{condlst}{
A list of objects, each of them of class \dQuote{cond}.
As returned by \code{condition}.
}
  \item{\dots}{
Passed to \code{\link{print.data.frame}}.
}
}


\value{
\code{cna}, \code{asf} and \code{as.condTbl} return a data.frame with additional class attribute \dQuote{condTbl}.
}

\seealso{
A general overview of the facilities in the \code{cna} package is given in \link{cna-package}.
}
