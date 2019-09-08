\name{condTbl}
\alias{condTbl}
\alias{print.condTbl}
\alias{as.condTbl}
\alias{condTbl}
\alias{msc}
\alias{asf}
\alias{csf}


\title{
Extract conditions and solutions from an object of class \dQuote{cna}
}

\description{
Given a solution object \code{x} produced by \code{\link{cna}}, \code{msc(x)} extracts all minimally sufficient conditions, \code{asf(x)} all atomic solution formulas, and \code{csf(x, n)} extracts at least \code{n} complex solution formulas. All solution attributes (\code{details}) that are saved in \code{x} are recovered as well. The three functions return a data frame with the additional class attribute \code{condTbl}.

\code{as.condTbl} reshapes the output produced by \code{\link{condition}} in such a way as to make it identical to the output returned by \code{msc}, \code{asf}, and \code{csf}.

\code{condTbl} executes \code{\link{condition}} and returns a concise summary table featuring consistencies and coverages.
}

\usage{
msc(x, details = x$details)
asf(x, details = x$details, warn_details = TRUE)
csf(x, n = 20, tt = x$truthTab, details = x$details, 
    asfx = asf(x, details, warn_details = FALSE))

\method{print}{condTbl}(x, digits = 3, quote = FALSE, row.names = TRUE, ...)

condTbl(...)
as.condTbl(x, ...)
}

\arguments{
  \item{x}{Object of class \dQuote{cna}. In \code{as.condTbl}, \code{x} is a list of evaluated conditions as returned by \code{condition}.}
  \item{details}{Either \code{TRUE/FALSE} or a character vector specifying which solution attributes to print (see \code{\link{cna}}). Note that \code{msc} and \code{asf} can only display attributes that are saved in \code{x}, i.e. those that have been requested in the \code{details} argument within the call of \code{\link{cna}}.}
  \item{warn_details}{Logical; if \code{TRUE}, a warning is issued when some attribute requested in \code{details} is not available in \code{x} (parameter for internal use).}
  \item{n}{The minimal number of \code{csf} to be calculated.}
  \item{tt}{A \code{truthTab}.}
  \item{asfx}{Object of class \dQuote{condTbl} resulting from \code{asf}.}
  \item{digits}{Number of digits to print in consistency, coverage, exhaustiveness, faithfulness, and coherence scores.}
  \item{quote, row.names}{As in \code{\link{print.data.frame}}.}
  \item{\dots}{All arguments in \code{condTbl} are passed on to \code{\link{condition}}.}
}

\details{Depending on the processed data, the solutions output by \code{\link{cna}} are often ambiguous, to the effect that many atomic and complex solutions fit the data equally well. To facilitate the inspection of the \code{\link{cna}} output, however, the latter standardly returns only 5 minimally sufficient conditions and 5 atomic and complex solution formulas for each outcome. \code{msc} can be used to extract \emph{all} minimally sufficient conditions from an object \code{x} of class \dQuote{cna}, \code{asf} to extract \emph{all} atomic solution formulas, and \code{csf} to extract at least \code{n} complex solution formulas from \code{x}. All solution attributes (\code{details}) that are saved in \code{x} are recovered as well.
The outputs of \code{msc}, \code{asf}, and \code{csf} can be further processed by the \code{\link{condition}} function.

The argument \code{digits} applies to the \code{print} function. It determines how many digits of consistency, coverage, exhaustiveness, faithfulness, and coherence scores are printed. The default value is 3.

The function \code{as.condTbl} takes a list of objects of class \dQuote{cond} that are returned by the \code{\link{condition}} function as input, and reshapes these objects in such a way as to make them identical to the output returned by \code{msc}, \code{asf}, and \code{csf}.

\code{condTbl(...)} is identical with \code{as.condTbl(condition(...))}.
}

\value{
\code{msc}, \code{asf}, \code{csf}, and \code{as.condTbl} return objects of class \dQuote{condTbl}, a \code{data.frame} which features the following components:
\tabular{rl}{
\code{outcome}: \tab the outcomes\cr
\code{condition}:\tab the relevant conditions or solutions\cr
\code{consistency}:\tab the consistency scores \cr
\code{coverage}: \tab the coverage scores  \cr
\code{complexity}: \tab the complexity scores\cr
\code{inus}: \tab whether the solutions are inus\cr
\code{exhaustiveness}: \tab the exhaustiveness scores\cr
\code{faithfulness}: \tab the faithfulness scores\cr
\code{coherence}: \tab the coherence scores\cr
\code{redundant}: \tab whether the csf contain redundant proper parts
}
The latter five measures are optional and will be appended to the table according to the setting of the argument \code{details}.
}


\references{
Lam, Wai Fung, and Elinor Ostrom. 2010.
\dQuote{Analyzing the Dynamic Complexity of Development Interventions: Lessons
from an Irrigation Experiment in Nepal.}
\emph{Policy Sciences} 43 (2):1-25.
}


\seealso{\code{\link{cna}}, \code{\link{truthTab}}, \code{\link{condition}}, \code{\link{minimalizeCsf}}, \code{\link{d.irrigate}}}

\examples{
# Crisp-set data from Lam and Ostrom (2010) on the impact of development interventions
# ------------------------------------------------------------------------------------
# CNA with causal ordering that corresponds to the ordering in Lam & Ostrom (2010); coverage 
# cut-off at 0.9 (consistency cut-off at 1).
cna.irrigate <- cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = .9,
                    maxstep = c(4, 4, 12), details = TRUE)
cna.irrigate

# The previous function call yields a total of 12 complex solution formulas, only
# 5 of which are returned in the default output. 
# Here is how to extract all 12 complex solution formulas along with all 
# solution attributes.
csf(cna.irrigate)
# With only the standard attributes plus exhaustiveness and faithfulness.
csf(cna.irrigate, details = c("e", "f"))

# Extract all atomic solution formulas.
asf(cna.irrigate)

# Extract all minimally sufficient conditions.
msc(cna.irrigate)

# Extract only the conditions (solutions).
csf(cna.irrigate)$condition
asf(cna.irrigate)$condition
msc(cna.irrigate)$condition

# A CNA of d.irrigate without a presupposed ordering is even more ambiguous.
cna2.irrigate <- cna(d.irrigate, cov = .9, maxstep = c(4,4,12), details = TRUE)

# To speed up the construction of complex solution formulas, first extract atomic solutions
# and then pass these asf to csf.
cna2.irrigate.asf <- asf(cna2.irrigate)
# By default, at least 20 csf are generated.
\donttest{csf(cna2.irrigate, asfx = cna2.irrigate.asf, details = FALSE)}
# Generate the first 191 csf.
\donttest{csf(cna2.irrigate, asfx = cna2.irrigate.asf, 191, details = FALSE)}
# Also extract exhaustiveness scores.
\donttest{csf(cna2.irrigate, asfx = cna2.irrigate.asf, 191, details = "e")}
# Generate all 684 csf.
\donttest{csf(cna2.irrigate, asfx = cna2.irrigate.asf, 684)}

# Return solution attributes with 5 digits.
print(cna2.irrigate.asf, digits = 5)

# Another example to the same effect.
\donttest{print(csf(cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), 
              maxstep = c(4, 4, 12), cov = 0.9)), digits = 5)}              

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

condTbl(csf(cna.irrigate)$condition, d.irrigate) # Same as preceding line
}
