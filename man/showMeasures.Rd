\name{showMeasures}
\alias{showMeasures}
\alias{showConCovMeasures}
\alias{showDetailMeasures}

\title{Show names and aliases of the con/cov measures and of the solution attributes}

\description{
\code{showMeasures} prints an overview of the available measures for sufficiency and necessity evaluation as well as of the additional solution attributes (details) to the console; \code{showConCovMeasures} and \code{showDetailMeasures} print the two parts separately.
}
\usage{
showMeasures(conCov = TRUE, details = TRUE)
showConCovMeasures()
showDetailMeasures() 
}

\arguments{
  \item{conCov}{Logical; if \code{TRUE} (the default), con/cov measures are printed to the console.}
  \item{details}{Logical; if \code{TRUE} (the default), additional solution attributes (details) are printed to the console. If both \code{conCov} and \code{details} are \code{FALSE}, nothing is printed.}
}

\details{
As of version 4.0 of the \pkg{cna} package, four measures are available for sufficiency evaluation and four for necessity evaluation. For model building, \code{\link{cna}} uses one measure of each kind (cf. its \code{measures} argument). Their theoretical background is discussed in the package vignette (\code{vignette("cna")}) and in De Souter and Baumgartner (2025). The measures have different names and aliases.\cr \code{showConCovMeasures} prints an overview of these measures to the console.
The names or aliases corresponding to each measure can be passed to the \code{measures} argument of \code{\link{cna}} and of \code{\link{condition}}, to the \code{details} argument of \code{\link{cna}}, \code{\link{msc}}, \code{\link{asf}}, and \code{\link{csf}} (cf. \code{\link{condTbl}}), and to the \code{what} argument of \code{\link{detailMeasures}}.

Similarly, the function \code{showDetailMeasures} provides an overview of the eight additional solution attributes (details) that can be used in the \code{details} argument of the functions \code{cna}, \code{\link{msc}}, \code{\link{asf}}, and \code{\link{csf}} and in the \code{what} argument of the \code{detailMeasures} function: \code{inus}, \code{cyclic}, \code{fm_empirical}, \code{fm_structural}, \code{exhaustiveness}, \code{faithfulness}, \code{coherence}, and \code{redundant}.
For more information about these attributes, see sections 5.2 to 5.5 of the package vignette (\code{vignette("cna")}). 

The function \code{showMeasures} combines the output of \code{showConCovMeasures} and \if{html}{}\if{latex}{\cr} \code{showDetailMeasures}. 
}

\seealso{\code{\link{cna}}, \code{\link{detailMeasures}}, \code{\link{condition}}, \code{\link{condTbl}}, \code{\link{func_minim}}}

\references{
De Souter, Luna and Michael Baumgartner. 2025. \dQuote{New sufficiency and necessity measures for model building with Coincidence Analysis.} \emph{Zenodo}. \doi{10.5281/zenodo.13619580}
}

\examples{
showConCovMeasures()
showDetailMeasures()
showMeasures()
}
