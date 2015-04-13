\name{cna-package}
\alias{cna-package}


\docType{package}

\title{
cna: A Package for Coincidence Analysis
}

\description{
The \pkg{cna} package implements the method of \dfn{Coincidence Analysis} (CNA)
as developed and introduced in Baumgartner (2009a, 2009b, 2013). CNA is a
configurational comparative method related to Qualitative Comparative Analysis (QCA) (Ragin 1987, 2008). Like QCA, CNA processes configurational data, i.e. data consisting of observed cases featuring different factor configurations, it focuses on causal complexity rather than on net effects, it searches for rigorously minimized sufficient and necessary conditions of causally modeled outcomes, and it implements the same regularity theoretic notion of causation as QCA, i.e. the notion e.g. developed by Mackie (1974). Contrary to QCA, however, CNA can treat any number of factors in an analyzed factor set as outcomes, and it does not eliminate redundancies from sufficient and necessary conditions by means of Quine-McCluskey optimization (Quine 1959, McCluskey 1965), but by means of an optimization algorithm that is custom-built for causal modeling. As a direct consequence of these differences, CNA can identify common-cause and causal-chain structures. Moreover, the algorithm does not need to be told which factors are outcomes and which ones are potential causes; it can infer that from the data. Finally, limited diversity does not force CNA to resort to counterfactual additions to the data. As of the current program version, however, the \pkg{cna} package still requires a data input comprising Boolean variables only.

}

\details{
    \tabular{ll}{
Package: \tab cna\cr
Type:    \tab Package\cr
Version: \tab 1.0-3\cr
Date:    \tab 2015-04-13\cr
License: \tab GPL (>= 2)\cr
    }
}

\author{

\bold{Authors}:\cr
Mathias Ambuehl\cr
\email{mathias.ambuehl@consultag.ch}

Michael Baumgartner\cr
Department of Philosophy\cr
University of Geneva\cr
\email{michael.baumgartner@unige.ch}

Ruedi Epple\cr
Department of Social Sciences\cr
University of Fribourg\cr
\email{ruedi.epple@unifr.ch}

Alexis Kauffmann\cr
Department of Philosophy\cr
University of Geneva\cr
\email{alexis.kauffmann@unige.ch}

Alrik Thiem\cr
Department of Philosophy\cr
University of Geneva\cr
\email{alrik.thiem@unige.ch}

\bold{Maintainer}:\cr
Alexis Kauffmann
}

\references{
Baumgartner, Michael. 2009a. \dQuote{Inferring Causal Complexity.}
\emph{Sociological Methods & Research} 38(1):71-101.

Baumgartner, Michael. 2009b. \dQuote{Uncovering Deterministic Causal Structures:
A Boolean Approach.} \emph{Synthese} 170(1):71-96.

Baumgartner, Michael. 2013. \dQuote{Detecting Causal Chains in Small-n Data.}
\emph{Field Methods} 25 (1):3-24.

Baumgartner, Michael, and Ruedi Epple. 2014. \dQuote{A Coincidence Analysis of a
Causal Chain: The Swiss Minaret Vote.}
\emph{Sociological Methods & Research} 43(2):280-312.

Mackie, John L. 1974. \emph{The Cement of the Universe: A Study of Causation}. Oxford: Oxford University Press.

McCluskey, Edward J. 1965. \emph{Introduction to the Theory of Switching Circuits}. Princeton: Princeton University Press.

Quine, Willard v. O. 1959. \dQuote{On Cores and Prime Implicants of Truth Functions.} \emph{The American
Mathematical Monthly} 66:755-760.

Ragin, Charles C. 1987. \emph{The Comparative Method}. Berkeley: University of California Press.

Ragin, Charles C. 2008. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. Chicago:
University of Chicago Press.
}

\keyword{package}