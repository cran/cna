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
Coherence is a measure for model fit that is custom-built for complex solution formulas. It measures the degree to which the asf combined in a csf cohere, i.e. are instantiated together in \code{tt} rather than independently of one another. More concretely, coherence is the ratio of the number of cases satisfying at least one asf contained in a csf to the number of cases satisfying all asf in the csf. For example, if the csf contains the three asf asf1, asf2, asf3, coherence amounts to n(asf1 + asf2 + asf3) / n(asf1 * asf2 * asf3), where n(condition) expresses the number of cases instantiating the condition. For asf, \code{coherence} returns "NA". For multiple csf that do not have a factor in common, \code{coherence} returns the minimum of the separate coherence scores.
}

\value{
Numeric vector of coherence values.
}

\seealso{\code{\link{condition}}, \code{\link{selectCases}}, \code{\link{allCombs}}, \code{\link{condTbl}}}

\examples{
dat1 <- allCombs(rep(2, 6))-1
dat2 <- selectCases("A*b + c*D <-> E", dat1) 
coherence("A*b + c*D <-> E", dat1)
csf(cna(dat2))

dat3 <- selectCases("(A*b <-> C)*(c+D <-> E)", dat1)
coherence("(A*b <-> C)*(c + D <-> E)", dat3)
csf(cna(dat3))

dat4 <- selectCases("(A*B <-> C)*(D+E <-> F)", dat1)
coherence("(A*B <-> C)*(D + E <-> F)", dat4)
csf(cna(dat4))

dat5 <- allCombs(rep(2, 8))-1
dat6 <- selectCases("(a*B <-> C)*(c + D<->E)*(F*g <-> H)", dat5)
dat7 <- rbind(tt2df(dat6), c(0,1,1,1,0,0,0,1))
coherence("(a*B <-> C)*(c + D <-> E)*(F*g <-> H)", dat7)
csf(cna(dat7, con=.8, cov=.9)) 
}

