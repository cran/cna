\name{cna-package}
\alias{cna-package}

\docType{package}
\title{
Coincidence Analysis (CNA)
}

\description{
This package is an implementation of the Coincidence Analysis (CNA) introduced in Baumgartner (2009a, 2009b).
In what follows, we give a general overview of the facilities in the package.
}

\details{
Starting with an input table of several binary factors, the analysis generally consists in the following main steps:

\itemize{
\item{Build a truth table: \code{\link{truthTab}},}
\item{run the CNA to find sufficient conditions and solution formulas: \code{\link{cna}},}
\item{inspect the resulting solutions using the facilities described in \code{\link{condition}} and \code{\link{condTbl}}.}
}

Note that it is also possible to examine specific conditions directly in your truth table with \code{\link{condition}}.
}

\author{
Mathias Ambuehl
\email{mathias.ambuehl@consultag.ch}
}

\references{
Baumgartner, Michael (2009a), \dQuote{Inferring Causal Complexity}, \emph{Sociological Methods & Research}, 38, 71?101.\cr
Baumgartner, Michael (2009b), \dQuote{Uncovering Deterministic Causal Structures: A Boolean Approach}, \emph{Synthese}, 170, 71?96.
}

\examples{
### extable is a truth table on which coincidence analysis will be applied
extable <- data.frame ( A=c(1,1,1,1,0,0,0,0), B=c(1,1,0,0,1,1,0,0),
C=c(1,1,1,1,1,1,0,0), D=c(1,0,1,0,1,0,1,0),
E=c(1,1,1,1,1,0,1,0))

### cna is the main fonction of this package
cna(extable)

### The final result is:
###                     condition consistency coverage
### (A + B <-> C) & (A + D <-> E)       1.000    1.000
}
