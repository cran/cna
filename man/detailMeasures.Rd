\name{detailMeasures}
\alias{detailMeasures}

\title{
Calculate evaluation measures and solution attributes for msc, asf, and csf
}
\description{
\code{detailMeasures} can calculate all available measures for sufficiency and necessity evaluation (e.g. prevalence-adjusted consistency and antecedent-adjusted coverage), independently of whether they are used for model building, as well as additional solution attributes (e.g. exhaustiveness or faithfulness).
}

\usage{
detailMeasures(
  cond, x, what = TRUE,
  inus.def = "implication", inus.func.minim = FALSE,
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
Character vector specifying the evaluation measures and additional solution attributes to be computed. Possible elements are all the measures in \code{\link{showMeasures}}. Can also be \code{TRUE}/\code{FALSE}. If \code{FALSE}, no additional measures are returned; \code{TRUE} resolves to all solution attributes listed in \code{\link{showDetailMeasures}} except \code{redundant}, which must be requested explicitly.\cr The attributes that apply to csf only (\code{coherence}, \code{fm_structural}, \code{fm_empirical}) are computed only for csf.
}

  \item{inus.def, inus.func.minim}{Passed to \code{\link{is.inus}}: \code{inus.def} selects the definition of partial structural redundancy, \code{inus.func.minim} determines whether functional minimality (in the sense of \code{\link{fm_structural}}) is required for elements of \code{cond} to pass as having INUS form.
}

  \item{cycle.type}{Character string specifying what type of cycles to be detected: \code{"factor"} (the default) or \code{"value"} (cf. \code{\link{cyclic}}).
}
  \item{\dots}{Further arguments passed to the \code{.det} methods, e.g. \code{type} when \code{x} is a data frame (cf. \code{\link{configTable}}).
}
}

\details{
The \code{\link{cna}} function can build its models using one out of four measures for sufficiency evaluation and one out of four measures for necessity evaluation (cf. section 3.2 of the \pkg{cna} package vignette, call \code{vignette("cna")}, or De Souter & Baumgartner 2025). The measures that are not used for model building may still be useful for cross-validation or selecting among the resulting models. The \code{detailMeasures} function can calculate all these measures, independently of whether they are used for model building. The measures can be passed to the \code{detailMeasures} function by their names or aliases in \code{\link{showConCovMeasures}}.

In addition, \code{detailMeasures} computes \code{exhaustiveness}, \code{faithfulness}, and \code{coherence}, which are three measures for overall data fit (cf. sections 5.2 and 5.3 of \code{vignette("cna")}). It identifies formulas with \code{cyclic} substructures and determines whether they have \code{inus} form or \code{redundant} parts. How much information these two attributes carry depends on how the models were built: csf returned by \code{cna} at its default \code{inus.only = TRUE} are free of structural redundancies, so \code{redundant} is \code{FALSE} and \code{inus} \code{TRUE} for all of them; models built with \code{inus.only = FALSE} or supplied by the user need not be either. These additional solution attributes are passed to the \code{detailMeasures} function by their names in \code{\link{showDetailMeasures}}.

\bold{Notes:} First, \code{coherence}, \code{fm_structural}, \code{fm_empirical} and \code{redundant} apply to complex solution formulas (csf) only; when they are requested for asf, placeholder values are returned (\code{coherence} is set to 1, \code{redundant} to \code{FALSE}, and the two \code{fm*} measures to \code{NA}). \code{inus}, \code{cyclic}, \code{exhaustiveness} and \code{faithfulness}, by contrast, are computed and meaningful for msc, asf, and csf alike. Second, \code{redundant} and \code{inus} are interdependent as follows: if \code{redundant} is \code{TRUE} for a csf, then \code{inus} is \code{FALSE} for that csf (see example below). Third, whether the csf-only attributes are computed or returned as placeholders depends on the class of \code{cond}: conditions from a solution table (cf. \code{\link{cna-solutions}}) carry the class \code{stdAtomic} or \code{stdComplex}, but if that class is dropped---for instance by applying \code{c()} to the vector---the type is re-derived from the syntax, and a csf that consists of a single asf is treated as an asf.

}

\value{
An object of class \dQuote{\code{\link{condTbl}}}, a \code{data.frame} with the columns \code{outcome} and \code{condition} followed by the requested measures and solution attributes.
}

\seealso{
\code{\link{cna}}, \code{\link{msc}}, \code{\link{asf}}, \code{\link{csf}}, \code{\link{configTable}}, \code{\link{condition}}, \code{\link{cyclic}}, \code{\link{showMeasures}}, \code{\link{showConCovMeasures}}, \code{\link{showDetailMeasures}}, \code{\link{func_minim}}, \code{\link{redundant}}
} 


\references{
De Souter, Luna and Michael Baumgartner. 2025. \dQuote{New sufficiency and necessity measures for model building with Coincidence Analysis.} \emph{Zenodo}. \doi{10.5281/zenodo.13619580}
}


\examples{
cond <- csf(cna(d.women))$condition
detailMeasures(cond, d.women)
detailMeasures(cond, d.women, what = c("ex", "fa", "PAcon", "PACcov", "AACcon", 
                                       "AAcov"))

# The functional-minimality measures are computed for csf only; for the asf in
# the first row, the placeholder value NA is returned.
detailMeasures(c("ES*ws*WNP -> QU",
                 "(ES + WM <-> QU)*(WS + ES*WM + QU*LP + WM*LP <-> WNP)"),
              d.women, what = c("fm_empirical", "fm_structural"))

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

# INUS form can additionally be required to include functional minimality
# (inus.func.minim, cf. is.inus and func_minim). The second csf below contains
# a factor that is functionally redundant in the sense of fm_structural().
aircon <- selectCases("(A + B <-> E1)*(A*b + A*C <-> E2)")
cond2 <- csf(cna(aircon))$condition
detailMeasures(cond2, aircon, what = "inus")
detailMeasures(cond2, aircon, what = "inus", inus.func.minim = TRUE)
}

