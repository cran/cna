\name{coherence}
\alias{coherence}

\title{
Calculate the coherence of complex solution formulas
}

\description{
Calculates the coherence measure of complex solution formulas (csf).
}

\usage{
coherence(cond, tt, type)
}

\arguments{
  \item{cond}{Character vector specifying an asf or csf.}
  \item{tt}{Data frame or \code{truthTab}.}
  \item{type}{Character vector specifying the type of \code{tt}: \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set). Defaults to the type of \code{tt}, if \code{tt} is a \code{truthTab} or to \code{"cs"} otherwise.}
}

\details{
Coherence is a measure for model fit that is custom-built for complex solution formulas (csf). It measures the degree to which the atomic solution formulas (asf) combined in a csf cohere, i.e. are instantiated together in \code{tt} rather than independently of one another. More concretely, coherence is the ratio of the number of cases satisfying all asf contained in a csf to the number of cases satisfying at least one asf in the csf. For example, if the csf contains the three asf asf1, asf2, asf3, coherence amounts to | asf1 * asf2 * asf3 | / | asf1 + asf2 + asf3 |, where |...| expresses the cardinality of the set of cases instantiating the corresponding expression. For asf, \code{coherence} returns 1. For boolean conditions (see \code{\link{condition}}), the coherence measure is not defined and \code{coherence} hence retuns \code{NA}. For multiple csf that do not have a factor in common, \code{coherence} returns the minimum of the separate coherence scores.
}

\value{
Numeric vector of coherence values.
}

\seealso{\code{\link{cna}}, \code{\link{condition}}, \code{\link{selectCases}}, \code{\link{allCombs}}, \code{\link{full.tt}}, \code{\link{condTbl}}}

\examples{
# Perfect coherence.
dat1 <- selectCases("(A*b <-> C)*(C+D <-> E)")
coherence("(A*b <-> C)*(C + D <-> E)", dat1)
csf(cna(dat1, details = "c"))

# Non-perfect coherence.
dat2 <- selectCases("(a*B <-> C)*(C + D<->E)*(F*g <-> H)")
dat3 <- rbind(tt2df(dat2), c(0,1,0,1,1,1,0,1))
coherence("(a*B <-> C)*(C + D <-> E)*(F*g <-> H)", dat3)
csf(cna(dat3, con=.88, details = "c")) 
}

