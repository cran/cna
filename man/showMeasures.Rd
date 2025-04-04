
\name{showMeasures}
\alias{showMeasures}
\alias{showConCovMeasures}
\alias{showDetailMeasures}

\title{Show names and abbreviations of con/cov measures and details}

\description{
\code{showMeasures()} prints an overview of the available measures for sufficiency and necessity evaluation to the console.
}
\usage{
showMeasures(conCov = TRUE, details = TRUE)
showConCovMeasures()
showDetailMeasures()
}

\arguments{
  \item{conCov}{Logical; if \code{TRUE}, con/cov measures are printed to the console.}
  \item{details}{Logical; if \code{TRUE}, additional solution attributes (details) are printed to the console.}
}

\details{
As of version 4.0 of the \pkg{cna} package, not only standard consistency and coverage are available for sufficiency and necessity evaluation, but the \code{\link{cna}} function can build models using three additional measures for sufficiency and three for necessity evaluation. Their theoretical background is discussed in the package vignette (\code{vignette("cna")}) and in De Souter and Baumgartner (2025). The measures have different names and aliases. The function \code{showConCovMeasures()} provides easy access to an overview of these measures in the console. 
%It outputs the following list:
%
%\enumerate{
%\item  standard consistency\cr
%      aliases: "scon", "s-con"
%\item  standard coverage\cr
%      aliases: "scov", "s-cov"
% \item  contrapositive consistency\cr
%      aliases: "ccon", "c-con"
%\item   contrapositive coverage\cr
%      aliases: "ccov", "c-cov"
% \item  prevalence-adjusted consistency\cr
%      aliases: "PAcon", "PA-con"
%\item   antecedent-adjusted coverage\cr
%      aliases: "AAcov", "AA-cov"
% \item  antecedent-adjusted contrapositive consistency\cr
%      aliases: "AACcon", "AAC-con"
%\item   prevalence-adjusted contrapositive coverage\cr
%      aliases: "PACcov", "PAC-cov"
%}
The names or aliases corresponding to each measure can be passed to the \code{measures} and \code{details} arguments of the functions \code{cna} and \code{\link{condition}}, to the \code{details} argument of the functions \code{msc}, \code{asf}, and \code{csf} (cf. \code{\link{condTbl}}), and to the \code{what} argument of the \code{\link{detailMeasures}} function.






Similarly, the function \code{showDetailMeasures()} provides an overview of the additional solution attributes (details) that can be used in the \code{details} argument of the functions \code{cna}, \code{msc}, \code{asf}, and \code{csf} and the \code{what} argument of the \code{detailMeasures} function.  %It lists the following attributes:
% \code{inus}, \code{cyclic}, \code{exhaustiveness}, \code{faithfulness}, \code{coherence}, and \code{redundant}. 
 For more information about these attributes, see the package vignette (\code{vignette("cna")}).
 
The function \code{showMeasures()} combines the output of \code{showConCovMeasures()} and \if{html}{}\if{latex}{\cr} \code{showDetailMeasures()}. 


}


\seealso{\code{\link{cna}}, \code{\link{detailMeasures}}, \code{\link{condition}}, \code{\link{condTbl}}}

\references{
De Souter, Luna and Michael Baumgartner. 2025. \dQuote{New sufficiency and necessity measures for model building with Coincidence Analysis.} \emph{Zenodo}. https://doi.org/10.5281/zenodo.13619580
}

\examples{
showConCovMeasures()
showDetailMeasures()
showMeasures()
}

