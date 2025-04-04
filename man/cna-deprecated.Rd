\name{cna-deprecated}
\alias{cna-deprecated}
\alias{csct}
\alias{mvct}
\alias{fsct}
\alias{cscna}
\alias{mvcna}
\alias{fscna}
\alias{cscond}
\alias{mvcond}
\alias{fscond}

\title{Deprecated functions in the cna package}

\usage{
csct(...)
mvct(...)
fsct(...)

cscna(...)
mvcna(...)
fscna(...)

cscond(...)
mvcond(...)
fscond(...)
}

\description{
These functions are provided for compatibility with older versions of the \pkg{cna} package only, and may be removed eventually. They have become obsolete since the introduction of the default setting \code{type = "auto"} in package version 3.2.0.
}

\arguments{
  \item{\dots}{
    In \code{cscna}, \code{mvcna}, \code{fscna}: any formal argument of \code{cna} except \code{type}. In \code{csct}, \code{mvct}, \code{fsct}: any formal argument of \code{configTable} except \code{type}. In \code{cscond}, \code{mvcond}, \code{fscond}: any formal argument of \code{condition} except \code{type}.   
  }
}

\details{
\code{csct(x, ...)}, \code{mvct(x, ...)}, and \code{fsct(x, ...)} are shorthands for \code{configTable(x, type = "cs", ...)}, \cr\code{configTable(x, type = "mv", ...)} and \code{configTable(x, type = "fs", ...)}, respectively.

\code{cscna(x, ...)}, \code{mvcna(x, ...)}, and \code{fscna(x, ...)} are shorthands for \code{cna(x, type = "cs", ...)}, \code{cna(x, type = "mv", ...)} and \code{cna(x, type = "fs", ...)}, respectively.

\code{cscond(x, ct, ...)}, \code{mvcond(x, ct, ...)}, and \code{fscond(x, ct, ...)} are shorthands for \code{condition(x, ct, type = "cs", ...)}, \cr\code{condition(x, ct, type = "mv", ...)} and \code{condition(x, ct, type = "fs", ...)}, respectively.
}

\seealso{\code{\link{configTable}}, \code{\link{cna}}, \code{\link[cna]{condition}}}

\keyword{internal}

