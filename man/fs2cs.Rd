\name{fs2cs}
\alias{fs2cs}

\title{
Convert fs data to cs data
}

\description{
Convert fuzzy-set (fs) data to crisp-set (cs) data. 
Works for both a \code{data.frame} and a \code{\link{configTable}} input.
}

\usage{
fs2cs(x, cutoff = 0.5, border = "up", verbose = FALSE)
}

\arguments{
  \item{x}{A data frame or \code{\link{configTable}} of type fs.}
  \item{cutoff}{Minimum membership score required for a factor to count as instantiated in the data and to be integrated in the analysis. Value in the unit interval [0,1]. The default cutoff is 0.5.}
  \item{border}{Character string specifying whether factors with membership scores equal to \code{cutoff} are rounded up (\code{"up"}) or rounded down (\code{"down"}).}
  \item{verbose}{
Logical; if \code{verbose=TRUE} and a factor becomes constant, this is reported in a console message.}
}

\details{
If the input is a data frame, the output will be, too; and correspondingly for a \code{configTable} input.
Case frequencies in an input \code{configTable} are accounted for.
}

\value{\code{\link{data.frame}} or \code{\link{configTable}} of type cs, depending on input.}

\seealso{
\code{\link{configTable}}
}

\examples{

csJob <- configTable(d.jobsecurity)
fs2cs(csJob)

fs2cs(d.jobsecurity) # data.frame

dJob1 <- d.jobsecurity[1, ]
fs2cs(dJob1)         # L=0.57 --> L=1
fs2cs(dJob1, cutoff = 0.6) #  --> L=0
fs2cs(dJob1, cutoff = 0.57) #  --> L=1 (since border="up" by default)
fs2cs(dJob1, cutoff = 0.57, 
      border = "down")     #  --> L=0 
}

