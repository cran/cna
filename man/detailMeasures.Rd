\name{detailMeasures}
\alias{detailMeasures}

\title{
Calculate summary measures for msc, asf, and csf
}
\description{
\code{detailMeasures} can calculate all available measures for sufficiency and necessity evaluation (e.g. prevalence-adjusted consistency and antecedent-adjusted coverage), independently of whether they are used for model building, as well as additional solution attributes (e.g. exhaustiveness or faithfulness).
}

\usage{
detailMeasures(cond, x, 
               what = c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence"),
               cycle.type = c("factor", "value"), ...)
}

\arguments{
  \item{cond}{
Character vector specifying a set of minimally sufficient conditions (msc) or solution formulas (asf or csf) in the standard format 
(cf. \code{\link{condition}}/\code{\link{condList}}). Blanks are allowed.
}
  \item{x}{
Data frame, \code{\link{configTable}}, or matrix.
}

  \item{what}{
Character vector specifying the evaluation measures and additional solution attributes to be computed. Possible elements are all the measures in  \code{\link{showMeasures}}. Can also be \code{TRUE}/\code{FALSE}. If \code{FALSE}, no additional measures are returned; if \code{TRUE}, all measures in \code{\link{showDetailMeasures}} are computed.
}

\item{cycle.type}{Character string specifying what type of cycles to be detected: \code{"factor"} (the default) or \code{"value"} (cf. \code{\link{cyclic}}).
}
  \item{\dots}{Pass more arguments to \code{.det()} methods.
}
}

\details{
The \code{\link{cna}} function can build its models using one out of four measures for sufficiency evaluation and one out of four measures for necessity evaluation (cf. section 3.2 of the \pkg{cna} package vignette, call \code{vignette("cna")}, or De Souter & Baumgartner 2025). The measures that are not used for model building may still be useful for cross-validation or selecting among the resulting models. The \code{detailMeasures} function can calculate all these measures, independently of whether they are used for model building. The measures can be passed to the \code{detailMeasures} function by their names or aliases in \code{\link{showConCovMeasures}}.

In addition, \code{detailMeasures} computes \code{exhaustiveness}, \code{faithfulness}, and \code{coherence}, which are three measures for overall data fit (cf. sections 5.2 and 5.3 of \code{vignette("cna")}). It identifies models with \code{cyclic} substructures, and, if the CNA algorithm is modified through \code{cna}'s \code{control} argument, \code{detailMeasures} can determine whether models have \code{redundant} parts and whether they have \code{inus} form. These additional solution attributes are passed to the \code{detailMeasures} function by their names in \code{\link{showDetailMeasures}}.

Note: First, \code{coherence} and \code{redundant} are only meaningful for complex solution formulas (csf). Second, \code{redundant} and \code{inus} are interdependent as follows: if \code{redundant} is \code{TRUE} for a csf, then \code{inus} is \code{FALSE} for that csf (see example below).

}

\value{
A \code{\link{data.frame}}.
}

\seealso{
\code{\link{cna}}, \code{\link{msc}}, \code{\link{asf}}, \code{\link{csf}}, \code{\link{configTable}}, \code{\link{condition}}, \code{\link{cyclic}}, \code{\link{showMeasures}}
}


\references{
De Souter, Luna and Michael Baumgartner. 2025. \dQuote{New sufficiency and necessity measures for model building with Coincidence Analysis.} \emph{Zenodo}. https://doi.org/10.5281/zenodo.13619580
}


\examples{
cond <- csf(cna(d.women))$condition
detailMeasures(cond, d.women)
detailMeasures(cond, d.women, what = c("ex", "fa", "PAcon", "PACcov", "AACcon", 
                                       "AAcov"))

# Mixing msc, asf and csf.
detailMeasures(c("ES*ws*WNP -> QU", "QU*LP + WM*LP <-> WNP", 
                 "(ES + WM <-> QU)*(WS + ES*WM + QU*LP + WM*LP <-> WNP)"),
              d.women)

# In the following example, the csf is not inus, although all its component asfs are:
cond <- c("(f+a*D <-> C)", "(C+A*B <-> D)", "(c+a*E <-> F)", 
          "(f+a*D <-> C)*(C+A*B <-> D)*(c+a*E <-> F)")
ct <- full.ct(cond)
detailMeasures(cond, ct)
# The reason is that one of the asfs is redundant:
redundant(cond[4])
}

