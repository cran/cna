\name{cna}
\alias{cna}
\alias{print.cna}

\title{
Coincidence Analysis (CNA)
}

\description{
\code{cna} performs a Coincidence Analysis (CNA). 
See Baumgartner (2009a, 2009b) for a detailed desription and discussion of the methodology.
}

\usage{
cna(x, ordering = NULL, strict = FALSE, con = 1, cov = 1, 
    maxstep = 5, suff.only = TRUE)

\method{print}{cna}(x, what = "mac", digits = 3, nsolutions = 5, 
      row.names = FALSE, ...)
}

\arguments{
  \item{x}{
A truth table or data frame. 
If not already a truth table \code{\link{truthTab}} is initially applied to \code{x}.
}
  \item{ordering}{
\code{ordering} defines a causal ordering of the factors in \code{x} as a list of character vectors containing the factor names.
For example, \code{ordering = list(c("A", "B"), "C")} postulates that \code{A} and \code{B} are potential causes of \code{C}, but no vice versa.
\code{NULL} means that every causal relationship between any two fctors is possible.
}
  \item{strict}{
Logial. If \code{TRUE} factors on the same level of the causal ordering are potential causes of each other.
}
  \item{con}{
Minimum consistency of a sufficient condition.
}
  \item{cov}{
Minimum coverage of a necessary condition.
}
  \item{maxstep}{
Maximum number of steps in the algorithm for finding atomic solution formulas. See details below.
}
  \item{suff.only}{
Logical. If \code{TRUE} the function only finds minimally neccessary conditions and does not search for atomic and complex solution formulas.
}
  \item{what}{
A character string specifying what to print: 
If \code{what} contains a \dQuote{\code{t}}, the truth table is printed, if it contains an \dQuote{\code{m}}, the msc are printed, 
if it contains an \dQuote{\code{a}}, the asf are printed, and if it contains a \dQuote{\code{c}}, the csf are printed.
\code{what="all"} can be specified to print the full output (being equivalent to \code{what="tmac"}).
}
  \item{digits}{
Number of digits to print in consistencies and coverages.
}
  \item{nsolutions}{
Maximum number of conditions to print. 
Applies sepeartely to minimally sufficient conditions, atomic solution formulas (both for each outcome seperately) and complex solution formulas.
}
  \item{row.names, \dots}{
Are passed to \code{\link{print.data.frame}}.
}
}

\details{
Separately for each potential outcome factor, \code{cna} first finds all minimally necessary conditions (msc) according to \code{con}, 
and then searches for disjunctions of these msc that are minimally necessary according to \code{cov}, the atomic solution formulas (asf).
The search for atomic solution formulas is effected as follows: 
In a first step, the msc themselves are considered, in a second step disjunctions of two msc, then of three, etc.
The argument \code{maxstep} defines the number of such steps to be carried out, i.e. disjunctions of up to \code{maxstep} msc are examined as potential asf.
}

\value{
\code{cna} returns an object of class \dQuote{cna}, a list containing the relevant results of the analysis.
}

\references{
See the references listed in \link{cna-package}.
}

\seealso{
A general overview of the facilities in the \code{cna} package is given in \link{cna-package}.
}

