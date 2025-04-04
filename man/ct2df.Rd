\name{ct2df}
\alias{ct2df}
\alias{as.data.frame.configTable}

\title{
Transform a configuration table into a data frame
}

\description{
\code{ct2df} transforms a configuration table into a data frame. 
This is the converse function of \if{html}{}\if{latex}{\cr}\code{\link{configTable}}.

The method \code{as.data.frame} for class \dQuote{configTable} does a similar job, but ignores case frequencies.
}

\usage{
ct2df(ct)

\method{as.data.frame}{configTable}(x, ..., warn = TRUE)
}

\arguments{
  \item{ct,x}{A \code{\link{configTable}}.}
  \item{\dots}{Currently not used.}
  \item{warn}{Logical; if \code{TRUE} and case frequencies in input are not all equal to 1, a warning is issued.}
}

\details{
The function \code{ct2df} transforms a  \code{\link{configTable}} into a data frame by rendering rows corresponding to several cases in the \code{configTable}  as multiple rows in the resulting data frame.
In contrast, \code{as.data.frame(x)} simply drops the case frequencies without accounting for multiple identical cases and turns the \code{configTable} into a data frame.
} 

\value{
A \code{\link{data.frame}}.
}

\seealso{\code{\link{configTable}}, \code{\link{data.frame}}
}

\examples{
ct.educate <- configTable(d.educate[1:2])
ct.educate
ct2df(ct.educate) # the resulting data frame has 8 rows
as.data.frame(ct.educate) # the resulting data frame has 4 rows

dat1 <- some(configTable(allCombs(c(2, 2, 2, 2, 2)) - 1), n = 200, replace = TRUE)
dat2 <- selectCases("(A*b + a*B <-> C)*(C*d + c*D <-> E)", dat1)
dat2 
ct2df(dat2)
as.data.frame(dat2)

dat3 <- data.frame(
  A = c(1,1,1,1,1,1,0,0,0,0,0),
  B = c(1,1,1,0,0,0,1,1,1,0,0),
  C = c(1,1,1,1,1,1,1,1,1,0,0),
  D = c(1,0,0,1,0,0,1,1,0,1,0),
  E = c(1,1,0,1,1,0,1,0,1,1,0)
  )
ct.dat3 <- configTable(dat3, frequency = c(4,3,5,7,4,6,10,2,4,3,12))
ct2df(ct.dat3)
as.data.frame(ct.dat3)
}

