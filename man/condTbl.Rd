\name{condTbl}
\alias{condTbl}
\alias{print.condTbl}
\alias{as.condTbl}
\alias{msc}
\alias{asf}
\alias{csf}


\title{
Extract conditions and solutions from an object of class \dQuote{cna}
}

\description{
\code{condTbl} is a set of three functions that can be used to extract specific elements from an object of class \dQuote{cna}: \code{msc} extracts all minimally sufficient conditions, \code{asf} all atomic solution formulas, and \code{csf} all complex solution formulas. Moreover, \code{msc}, \code{asf}, and \code{csf} recover corresponding consistency and coverage scores.

\code{as.condTbl} reshapes the output produced by \code{\link{condition}}
in such a way as to make it identical to the output returned by \code{msc} and \code{asf}.
}

\usage{
msc(x)
asf(x)
csf(x, asfx = asf(x))

\method{print}{condTbl}(x, digits = 3, ...)

as.condTbl(condlst, ...)
}

\arguments{
  \item{x}{An object of class \dQuote{cna}.}
  \item{asfx}{An object of class \dQuote{condTbl} resulting from \code{asf}.}
  \item{digits}{Number of digits to print in consistency and coverage scores.}
  \item{condlst}{A list of objects, each of them of class \dQuote{cond}, as
        returned by \code{condition}.}
  \item{\dots}{Passed to \code{\link{print.data.frame}}.}
}

\details{Depending on the processed data frame or truth table, the solutions output by \code{\link{cna}} are sometimes ambiguous, that is, it can happen that many atomic and complex solutions fit the data equally well. To facilitate the inspection of the \code{\link{cna}} output, however, the latter standardly returns only 5 minimally sufficient conditions and 5 atomic and complex solution formulas for each outcome factor. \code{msc} can be used to extract \emph{all} minimally sufficient conditions from an object of class \dQuote{cna}, \code{asf} to extract \emph{all} atomic solution formulas, and \code{csf} to extract \emph{all} complex solution formulas. The outputs of \code{msc} and \code{asf} can be further processed by the \code{\link{condition}} function.

The argument \code{digits} applies to the \code{print} function. It determines how many digits of consistency and coverage scores are printed in the outputs of \code{msc}, \code{asf}, and \code{csf}. The default value is 3.






}

\value{
\code{msc}, \code{asf}, \code{csf}, and \code{as.condTbl} return objects of class \dQuote{condTbl}, which feature the following components:
\tabular{rl}{
\code{outcome}: \tab the outcome factors\cr
\code{condition}:\tab the relevant conditions or solutions\cr
\code{consistency}:\tab the consistency scores \cr
   \code{coverage}: \tab the coverage scores  \cr
    \code{row.names}: \tab the row names
  }
}


\references{
Lam, Wai Fung, and Elinor Ostrom. 2010.
\dQuote{Analyzing the Dynamic Complexity of Development Interventions: Lessons
from an Irrigation Experiment in Nepal.}
\emph{Policy Sciences} 43 (2):1-25.

}


\seealso{\code{\link{cna}}, \code{\link{truthTab}}, \code{\link{condition}}, \code{\link{d.irrigate}}}

\examples{
# Lam and Ostrom (2010) on the impact of development interventions on water adequacy in Nepal
#--------------------------------------------------------------------------------------------
# Load dataset. 
data(d.irrigate)

# CNA with causal ordering that corresponds to the ordering in Lam & Ostrom (2010); coverage 
# cut-off at 0.9 (consistency cut-off at 1).
cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9)

# The previous function call yields a total of 12 complex solution formulas, only
# 5 of which are printed in the default output. 
# Here is how to extract all 12 complex solution formulas along with corresponding consistency
# and coverage scores.
cna.irrigate <- cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9)
csf(cna.irrigate)

# Extract all atomic solution formulas along with corresponding consistency and coverage
# scores.
asf(cna.irrigate)

# Extract all minimally sufficient conditions along with corresponding consistency and 
# coverage scores.
msc(cna.irrigate)

# Extract only the conditions or solutions as character vectors.
csf(cna.irrigate)$condition
asf(cna.irrigate)$condition
msc(cna.irrigate)$condition

# A CNA of d.irrigate without a presupposed ordering is even more ambiguous.
cna2.irrigate <- cna(d.irrigate, cov = 0.9)

# To speed up the construction of complex solution formulas, first extract atomic solutions
# and then feed these asf into csf.
cna2.irrigate.asf <- asf(cna2.irrigate) 
csf(cna2.irrigate, cna2.irrigate.asf)

# Return consistency and coverage scores with 5 digits.
print(cna2.irrigate.asf, digits = 5)

# Another example to the same effect.
print(csf(cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9)), digits = 5)

# Feed the outputs of msc and asf into the condition function to further inspect the properties
# of minimally sufficient conditions and atomic solution formulas.
condition(msc(cna.irrigate)$condition, d.irrigate)
condition(asf(cna.irrigate)$condition, d.irrigate)

# Reshape the output of the condition function in such a way as to make it identical to the 
# output returned by msc and asf.
as.condTbl(condition(msc(cna.irrigate)$condition, d.irrigate))
as.condTbl(condition(asf(cna.irrigate)$condition, d.irrigate))
}
