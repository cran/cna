\name{cna-solutions}
\alias{cna-solutions}
\alias{msc}
\alias{asf}
\alias{csf}


\title{
Extract solutions from an object of class \dQuote{cna}
}

\description{
Given a solution object \code{x} produced by \code{\link{cna}}, \code{msc(x)} extracts all minimally sufficient conditions, \code{asf(x)} all atomic solution formulas, and \code{csf(x, n.init)} builds approximately \code{n.init} complex solution formulas. All solution attributes (\code{details}) available in \code{\link{showMeasures}} can be computed. The three functions return a data frame with the additional class attribute \dQuote{\code{condTbl}}.
}

\usage{
msc(x, details = x$details, cases = FALSE)
asf(x, details = x$details)
csf(x, n.init = 1000, details = x$details, asfx = NULL, 
    inus.only = x$control$inus.only, inus.def = x$control$inus.def,  
    minimalizeCsf = inus.only, 
    acyclic.only = x$acyclic.only, cycle.type = x$cycle.type, verbose = FALSE)
}

\arguments{
  \item{x}{Object of class \dQuote{cna}. %In \code{as.condTbl}, \code{x} is a list of evaluated conditions as returned by \code{condition}. In \code{print}, \code{x} is an object returned by \code{msc}, \code{asf}, or \code{csf}.
  }
  
  \item{details}{A character vector specifying the evaluation measures and additional solution attributes to be computed. Possible elements are all the measures in \cr \code{\link{showMeasures}()}. Defaults to the \code{details} specification stored in \code{x}. Can also be \code{TRUE}/\code{FALSE}. If \code{FALSE}, no additional attributes are returned; \code{TRUE} resolves to \code{c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence")} (cf. \code{\link{detailMeasures}}).}  
  
  \item{cases}{Logical; if \code{TRUE}, an additional column listing the cases where the msc is instantiated in combination with the outcome is added to the output.}
  
  \item{n.init}{Integer capping the amount of initial asf combinations. Default at 1000. Serves to control the computational complexity of the csf building process. %After combining the asf to a preliminary set of csf, the number of these csf is reduced to \code{n.init} in order to limit the computational burden.
  }
  \item{asfx}{Object of class \dQuote{condTbl} produced by the \code{asf} function. Internal parameter not meant to be set by users.}
  
  \item{inus.only}{Logical; if \code{TRUE}, csf are freed of structural redundancies and only csf not featuring partial structural redundancies are retained  (cf. \code{vignette("cna")}; cf. also \code{\link{is.inus}} and \code{\link{cnaControl}}). }
  
  \item{inus.def}{Character string specifying the definition of partial structural redundancy to be applied. Possible values are "implication" or "equivalence". The strings can be abbreviated. Cf. also \code{\link{is.inus}} and \code{\link{cnaControl}}. }
  
  \item{minimalizeCsf}{Logical; if \code{TRUE}, csf are freed of structural redundancies (cf. \code{vignette("cna")}).}
  
  \item{acyclic.only}{Logical; if \code{TRUE}, csf featuring a cyclic substructure are not returned. \code{FALSE} by default.}
  \item{cycle.type}{Character string specifying what type of cycles to be detected: \code{"factor"} (the default) or \code{"value"} (cf. \code{\link{cyclic}}).}
  
  \item{verbose}{Logical; if \code{TRUE}, some details on the csf building process are printed. \code{FALSE} by default.}
}

\details{
Depending on the processed data, the solutions (models) output by \code{\link{cna}} are often ambiguous, to the effect that many atomic and complex solutions fit the data equally well. To facilitate the inspection of the \code{cna} output, however, \code{cna} standardly returns only 5 minimally sufficient conditions (msc) and 5 atomic solution formulas (asf) for each outcome as well as 5 complex solution formulas (csf). \code{msc} can be used to extract \emph{all} msc from an object \code{x} of class \dQuote{cna}, \code{asf} to extract \emph{all} asf, and \code{csf} to build approximately \code{n.init} csf from the asf stored in \code{x}. All solution attributes (\code{details}) that are saved in \code{x} are recovered as well. Moreover, all evaluation measures and solution attributes available in \code{\link{showMeasures}}---irrespective of whether they are saved in \code{x}---can be computed by specifying them in the \code{details} argument.
The outputs of \code{msc}, \code{asf}, and \code{csf} can be further processed by the \code{\link[cna]{condition}} function.

While \code{msc} and \code{asf} merely extract information stored in \code{x}, \code{csf} builds csf from the inventory of asf recovered at the end of the third stage of the \code{cna} algorithm (cf. \code{vignette("cna")}, section 4). That is, the \code{csf} function implements the fourth stage of that algorithm. It proceeds in a stepwise manner as follows.
\enumerate{
\item \code{n.init} possible conjunctions featuring one asf of every outcome are built. 
\item  If \code{inus.only = TRUE} or \code{minimalizeCsf = TRUE}, the solutions resulting from step 1 are freed of structural redundancies (cf. Baumgartner and Falk 2023). 
\item If \code{inus.only = TRUE}, tautologous and contradictory solutions as well as solutions with partial structural redundancies (as defined in \code{inus.def}) and constant factors are eliminated. 
[If \code{inus.only = FALSE} and \code{minimalizeCsf = TRUE}, only structural redundancies are eliminated, meaning only step 2, but not step 3, is executed.]
\item If \code{acyclic.only = TRUE}, solutions with cyclic substructures are eliminated. 
\item Solutions that are a submodel of another solution are removed.
\item For those solutions that were modified in the previous steps, the scores on the selected evaluation \code{measures} are re-calculated and solutions that no longer reach \code{con} or \code{cov} are eliminated (cf. \code{\link{cna}}). 
\item The remaining solutions are returned as csf, ordered by complexity and the product of their scores on the evaluation \code{measures}.
}
}

\value{
\code{msc}, \code{asf} and \code{csf} return objects of class \dQuote{\code{\link{condTbl}}}, an object similar to a \code{data.frame}, which features the following components:
\tabular{rl}{
\code{outcome}: \tab the outcomes\cr
\code{condition}:\tab the relevant conditions or solutions\cr
\code{con}:\tab the scores on the sufficiency measure (e.g. consistency) \cr
\code{cov}: \tab the scores on the necessity measure (e.g. coverage) \cr
\code{complexity}: \tab the complexity scores (number of factor value appearances to the left of \dQuote{\code{<->}})\cr
\code{...}: \tab scores on additional evaluation measures and solution attributes as specified in\cr\tab \code{details}
}
}



\references{
Lam, Wai Fung, and Elinor Ostrom. 2010.
\dQuote{Analyzing the Dynamic Complexity of Development Interventions: Lessons
from an Irrigation Experiment in Nepal.}
\emph{Policy Sciences} 43 (2):1-25.
}


\seealso{\code{\link{cna}}, \code{\link{configTable}}, \code{\link[cna]{condition}}, \code{\link{condTbl}},  \code{\link{cnaControl}}, \code{\link{is.inus}}, \code{\link{detailMeasures}}, \code{\link{showMeasures}}, \code{\link{cyclic}}, \code{\link{d.irrigate}}}

\examples{
# Crisp-set data from Lam and Ostrom (2010) on the impact of development interventions
# ------------------------------------------------------------------------------------
# CNA with causal ordering that corresponds to the ordering in Lam & Ostrom (2010); coverage 
# cut-off at 0.9 (consistency cut-off at 1).
cna.irrigate <- cna(d.irrigate, ordering = "A, R, F, L, C < W", cov = .9,
                    maxstep = c(4, 4, 12), details = TRUE)
cna.irrigate

# The previous function call yields a total of 12 complex solution formulas, only
# 5 of which are returned in the default output. 
# Here is how to extract all 12 complex solution formulas along with all 
# solution attributes.
csf(cna.irrigate)
# With only the used evaluation measures and complexity plus exhaustiveness and faithfulness.
csf(cna.irrigate, details = c("e", "f"))
# Calculate additional evaluation measures from showCovCovMeasures().
csf(cna.irrigate, details = c("e", "f", "PAcon", "PACcov", "AACcon", "AAcov"))

# Extract all atomic solution formulas.
asf(cna.irrigate, details = c("e", "f"))

# Extract all minimally sufficient conditions.
msc(cna.irrigate) # capped at 20 rows
print(msc(cna.irrigate), n = Inf) # prints all rows
# Add cases featuring the minimally sufficient conditions combined 
# with the outcome.
(msc.table <- msc(cna.irrigate, cases = TRUE))
# Render as data frame.
as.data.frame(msc.table)

# Extract only the conditions (solutions).
csf(cna.irrigate)$condition
asf(cna.irrigate)$condition
msc(cna.irrigate)$condition

# A CNA of d.irrigate without outcome specification and ordering is even more
# ambiguous.
cna2.irrigate <- cna(d.irrigate, cov = .9, maxstep = c(4,4,12), 
                     details = c("e", "f", "ccon", "ccov"))

# Reduce the initial asf combinations to 50.
csf(cna2.irrigate, n.init = 50)
# Print the first 20 csf.
csf(cna2.irrigate, n.init = 50)[1:20, ]

# Print details about the csf building process.
csf(cna.irrigate, verbose = TRUE)

# Return evaluation measures and solution attributes with 5 digits.
print(asf(cna2.irrigate), digits = 5)
 

# Further examples
# ----------------
# An example generating structural redundancies.
target <- "(A*B + C <-> D)*(c + a <-> E)"
dat1 <- selectCases(target)
ana1 <- cna(dat1, maxstep = c(3, 4, 10))
# Run csf with elimination of structural redundancies.
csf(ana1, verbose = TRUE)
# Run csf without elimination of structural redundancies. 
csf(ana1, verbose = TRUE, inus.only = FALSE)

# An example generating partial structural redundancies.
dat2 <- data.frame(
  A=c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0, 1),
  B=c(0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1),
  C=c(1,1,0,0,0,1,0,0,1,1,0,1,1,0,1,1,0,1,1,1,0,1,0,1,0,1,0),
  D=c(0,1,1,1,0,1,1,1,0,0,0,1,0,1,0,0,0,1,0,0,0,1,1,0,0,1,0),
  E=c(1,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,0,1,1,1,1,0,0,0,0,1,1),
  F=c(1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0),
  G=c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1))
ana2 <- cna(dat2, con = .8, cov = .8, maxstep = c(3, 3, 10))
# Run csf without elimination of partial structural redundancies.
csf(ana2, inus.only = FALSE, verbose = TRUE)
# Run csf with elimination of partial structural redundancies.
csf(ana2, verbose = TRUE)
# Prior to version 3.6.0, the "equivalence" definition of partial structural
# redundancy was used by default (see ?is.inus() for details). Now, the 
# "implication" definition is used. To replicate old behavior
# set inus.def to "equivalence".
csf(ana2, verbose = TRUE, inus.def = "equivalence")
# The two definitions only come apart in case of cyclic structures.
# Build only acyclic models.
csf(ana2, verbose = TRUE, acyclic.only = TRUE)
# Add further details.
csf(ana2, verbose = TRUE, acyclic.only = TRUE, details = c("PAcon", "PACcov"))

}
