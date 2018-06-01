\name{minimalizeCsf}
\alias{minimalizeCsf}
\alias{minimalizeCsf.default}
\alias{minimalizeCsf.cna}
\alias{print.minimalizeCsf}

\title{
Reduce csf by eliminating redundant parts
}

\description{
\code{minimalizeCsf} reduces \code{csf} by recursively testing their component \code{asf} for redundancy and eliminating the redundant ones.
}

\usage{
minimalizeCsf(x, ...)

\method{minimalizeCsf}{default}(x, data, verbose = FALSE, ...)
\method{minimalizeCsf}{cna}(x, n = 20, verbose = FALSE, ...)
\method{print}{minimalizeCsf}(x, subset = 1:5, ...)
}

\arguments{
  \item{x}{In the default method, \code{x} is a character vector specifying \code{csf}. The \code{cna} method uses the strings representing the \code{csf} contained in an output object of \code{cna}.}
  \item{data}{A \code{\link{truthTab}}, \code{data.frame} or \code{matrix} with the data.}
  \item{verbose}{Logical, if \code{TRUE} additional messages on the number of csf that are found to be reducible are printed.}
  \item{n}{Minimal number of \code{csf} to use.}
  \item{subset}{Integer vector specifying the numbers of \code{csf} to be displayed.}
  \item{\dots}{Further arguments passed to the methods.}
}

\details{The core criterion that Boolean dependency structures must satisfy in order to be causally interpretable is \emph{redundancy-freeness}. In atomic solution formulas (asf), both sufficient and necessary conditions are completely free of redundant elements. However, when asf are conjunctively combined to complex solution formulas (csf), new redundancies may arise. A csf may contain redundant parts. To illustrate, assume that a csf is composed of three asf: asf1 * asf2 * asf3. It can happen that asf1 * asf2 * asf3 is logically equivalent to a proper part of itself, say, to asf1 * asf2. In that case, asf3 is redundant in asf1 * asf2 * asf3 and must not be causally interpreted. 

\code{minimalizeCsf} recursively tests the \code{asf} contained in a \code{csf} for redundancy and eliminates the redundant ones. More specifically, it takes a character vector \code{x} specifying csf as input and builds all redundancy-free csf that can be inferred from \code{x}. The function is especially useful in combination with \code{cna}, which builds csf by conjunctively concatenating asf. One of the \code{cna} solution attributes, which is accessed via \code{details = TRUE} or \code{details = "r"}, is called "redundant". If a csf output by \code{cna} has the attribute \code{redundant = TRUE}, that csf has at least one redundant part. The \code{cna} function, however, does not identify those redundant parts. For this purpose, the \code{cna} object must be passed on to \code{minimalizeCsf}.
}

\value{
\code{minimalizeCsf} returns an object of class "minimalizeCsf", essentially a \code{data.frame}.
}

\section{Contributors}{
Falk, Christoph: identification of the problem of structural redundancies
}

\seealso{
\code{\link{csf}}, \code{\link{cna}}.
}

\examples{
myData <- allCombs(rep(2, 6))-1
minimalizeCsf("(f + a*D <-> C)*(C + A*B <-> D)*(c + a*E <-> F)", 
              myData)
minimalizeCsf("(f + a*D <-> C)*(C + A*B <-> D)*(c + a*E <-> F)", 
              myData, verbose = TRUE) # Same result, but with some messages.

# cna method.
dat1 <- selectCases("(C + A*B <-> D)*(c + a*E <-> F)", myData)
ana1 <- cna(dat1, details = c("r"))
csf(ana1)
# The attribute "redundant" taking the value TRUE in ana1 shows that this csf contains 
# at least one redundant element. Only the application of minimalizeCsf() identifies 
# the redundant element.
minimalizeCsf(ana1)
\donttest{
# Real data entailing a large number of csf with many redundancies.
tt.js <- fstt(d.jobsecurity)
cna.js <- cna(tt.js, con = .8, cov = .8)
minim100 <- minimalizeCsf(cna.js, n = 100) # may take a couple of seconds...
minim100  # By default the first 5 solutions are displayed. 
}
# With mv data.
tt.pban <- mvtt(d.pban)
cna.pban <- cna(tt.pban, con = .75, cov = .75)
csf.pban <- csf(cna.pban, 100)
sol.pban <- csf.pban$condition

minim.pban <- minimalizeCsf(sol.pban, tt.pban)
minim.pban

# Alternatively, a more direct replication of the above using the method of 
# minimalizeCsf for class cna.
minim.pban <- minimalizeCsf(cna.pban, n = 100)
print(minim.pban, 1:50) # print the first 50 redundancy-free csf
}
