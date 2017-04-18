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
Given an object \code{x} resulting from a \code{\link{cna}}, \code{msc(x)} extracts all the minimally sufficient conditions, \code{asf(x)} all atomic solution formulas, and \code{csf(x)} all complex solution formulas. The consistency and coverage scores are thereby recovered, as well as complexity measures and, in case of csf, coherence scores (cf. \code{\link{coherence}}). All three functions return a data frame with the additional class attribute \code{condTbl}.

\code{as.condTbl} reshapes the output produced by \code{\link{condition}} in such a way as to make it identical to the output returned by \code{msc}, \code{asf} and \code{csf}.
}

\usage{
msc(x)
asf(x)
csf(x, n = 20, tt = x$truthTab, asfx = asf(x))

\method{print}{condTbl}(x, digits = 3, ...)

as.condTbl(condlst, ...)
}

\arguments{
  \item{x}{An object of class \dQuote{cna}.}
  \item{n}{The maximal number of complex solutions that are calculated.}
  \item{tt}{A \code{truthTab}.}
  \item{asfx}{An object of class \dQuote{condTbl} resulting from \code{asf}.}
  \item{digits}{Number of digits to print in consistency, coverage and coherence scores.}
  \item{condlst}{A list of objects, each of them of class \dQuote{cond}, as
        returned by \code{condition}.}
  \item{\dots}{Passed to \code{\link{print.data.frame}}.}
}

\details{Depending on the processed data, the solutions output by \code{\link{cna}} are sometimes ambiguous, that is, it can happen that many atomic and complex solutions fit the data equally well. To facilitate the inspection of the \code{\link{cna}} output, however, the latter standardly returns only 5 minimally sufficient conditions and 5 atomic and complex solution formulas for each outcome. \code{msc} can be used to extract \emph{all} minimally sufficient conditions from an object of class \dQuote{cna}, \code{asf} to extract \emph{all} atomic solution formulas, and \code{csf} to extract \emph{all} complex solution formulas. Moreover, \code{msc}, \code{asf}, and \code{csf} recover corresponding consistency and coverage scores as well as complexity measures and, in case of csf, coherence scores (cf. \code{\link{coherence}}).
The outputs of \code{msc}, \code{asf}, and \code{csf} can be further processed by the \code{\link{condition}} function.

The argument \code{digits} applies to the \code{print} function. It determines how many digits of consistency, coverage, and coherence scores are printed. The default value is 3.

The function \code{as.condTbl} takes a list of objects of class \dQuote{cond} that are returned by the \code{\link{condition}} function as input, and reshapes these objects in such a way as to make them identical to the output returned by \code{msc}, \code{asf}, and \code{csf}.
}

\value{
\code{msc}, \code{asf}, \code{csf}, and \code{as.condTbl} return objects of class \dQuote{condTbl}, which feature the following components:
\tabular{rl}{
\code{outcome}: \tab the outcome factors\cr
\code{condition}:\tab the relevant conditions or solutions\cr
\code{consistency}:\tab the consistency scores \cr
\code{coverage}: \tab the coverage scores  \cr
\code{complexity, coherence}: \tab the complexity and coherence scores
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
# Crisp-set data from Lam and Ostrom (2010) on the impact of development interventions
# ------------------------------------------------------------------------------------
# Load dataset. 
data(d.irrigate)

# CNA with causal ordering that corresponds to the ordering in Lam & Ostrom (2010); coverage 
# cut-off at 0.9 (consistency cut-off at 1).
cna.irrigate <- cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9,
                    maxstep = c(4, 4, 12))
cna.irrigate

# The previous function call yields a total of 12 complex solution formulas, only
# 5 of which are returned in the default output. 
# Here is how to extract all 12 complex solution formulas along with corresponding consistency,
# coverage, complexity, and coherence scores.
csf(cna.irrigate)

# Extract all atomic solution formulas along with corresponding consistency and coverage
# scores.
asf(cna.irrigate)

# Extract all minimally sufficient conditions along with corresponding consistency and 
# coverage scores.
msc(cna.irrigate)

# Extract only the conditions (solutions).
csf(cna.irrigate)$condition
asf(cna.irrigate)$condition
msc(cna.irrigate)$condition

# A CNA of d.irrigate without a presupposed ordering is even more ambiguous.
cna2.irrigate <- cna(d.irrigate, cov = 0.9)

# To speed up the construction of complex solution formulas, first extract atomic solutions
# and then feed these asf into csf.
cna2.irrigate.asf <- asf(cna2.irrigate) 
csf(cna2.irrigate, asfx = cna2.irrigate.asf)
csf(cna2.irrigate, asfx = cna2.irrigate.asf, n = 75)

# Return consistency and coverage scores with 5 digits.
print(cna2.irrigate.asf, digits = 5)

# Another example to the same effect.
\donttest{
print(csf(cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), 
              maxstep = c(4, 4, 12), cov = 0.9)), digits = 5)
}              

# Feed the outputs of msc, asf, and csf into the condition function to further inspect the 
# properties of minimally sufficient conditions and atomic and complex solution formulas.
condition(msc(cna.irrigate)$condition, d.irrigate)
condition(asf(cna.irrigate)$condition, d.irrigate)
condition(csf(cna.irrigate)$condition, d.irrigate)

# Reshape the output of the condition function in such a way as to make it identical to the
# output returned by msc, asf, and csf.
as.condTbl(condition(msc(cna.irrigate)$condition, d.irrigate))
as.condTbl(condition(asf(cna.irrigate)$condition, d.irrigate))
as.condTbl(condition(csf(cna.irrigate)$condition, d.irrigate))
}
