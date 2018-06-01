\name{coherence}
\alias{coherence}

\title{
Calculate the coherence of complex solution formulas
}

\description{
Calculates the coherence measure of complex solution formulas.
}

\usage{
coherence(cond, tt, type)
}

\arguments{
  \item{cond}{Character vector specifying an asf or csf.}
  \item{tt}{A \code{truthTab} or data frame.}
  \item{type}{A character vector specifying the type of \code{tt}: \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set). Defaults to the type of \code{tt}, if \code{tt} is a \code{truthTab} or to \code{"cs"} otherwise.}
}

\details{
Coherence is a measure for model fit that is custom-built for complex solution formulas (csf). It measures the degree to which the atomic solution formulas (asf) combined in a csf cohere, i.e. are instantiated together in \code{tt} rather than independently of one another. More concretely, coherence is the ratio of the number of cases satisfying all asf contained in a csf to the number of cases satisfying at least one asf in the csf. For example, if the csf contains the three asf asf1, asf2, asf3, coherence amounts to | asf1 * asf2 * asf3 | / | asf1 + asf2 + asf3 |, where |...| expresses the cardinality of the set of cases instantiating the corresponding expression. For asf, \code{coherence} returns 1. For boolean conditions (see \code{\link{condition}}), the coherence measure is not defined and \code{coherence} hence retuns \code{NA}. For multiple csf that do not have a factor in common, \code{coherence} returns the minimum of the separate coherence scores.
}

\value{
Numeric vector of coherence values.
}

\seealso{\code{\link{cna}}, \code{\link{condition}}, \code{\link{selectCases}}, \code{\link{allCombs}}, \code{\link{condTbl}}}

\examples{
# Perfect coherence.
dat1 <- allCombs(rep(2, 6))-1 
dat2 <- selectCases("(A*b <-> C)*(C+D <-> E)", dat1)
coherence("(A*b <-> C)*(C + D <-> E)", dat2)
csf(cna(dat2, details = "c"))

# Non-perfect coherence.
dat3 <- allCombs(rep(2, 8)) -1
dat4 <- selectCases("(a*B <-> C)*(C + D<->E)*(F*g <-> H)", dat3)
dat5 <- rbind(tt2df(dat4), c(0,1,0,1,1,1,0,1))
coherence("(a*B <-> C)*(C + D <-> E)*(F*g <-> H)", dat5)
csf(cna(dat5, con=.88, details = "c")) 
}

