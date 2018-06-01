\name{cna-package}
\alias{cna-package}


\docType{package}

\title{
cna: A Package for Causal Modeling with Coincidence Analysis
}

\description{
The \pkg{cna} package implements the method of \dfn{Coincidence Analysis} (CNA), which was first introduced
in Baumgartner (2009a, 2009b, 2013). CNA is a
configurational comparative method related to Qualitative Comparative Analysis (QCA) (Ragin 1987, 2008). Like QCA, CNA processes configurational data, i.e. data consisting of observed cases featuring different factor configurations, it searches for redundancy-free sufficient and necessary conditions of causally modeled outcomes, it places a Boolean ordering over causally relevant factors (instead of quantifying effect sizes in the vein of regression analytic methods), and it draws on the same regularity theoretic notion of causation as QCA, i.e. the notion first introduced by Mackie (1974). Contrary to QCA, however, CNA is custom-built to treat multiple factors as outcomes, and it does not generate causal models from the top down by first building maximal Boolean dependency structures and then gradually eliminating redundant elements (using e.g. Quine-McCluskey optimization; cf. Quine 1959, McCluskey 1965); rather, CNA builds causal models from the bottom up by gradually combining single factors to complex dependency structures until the requested thresholds of model fit are met, such that the resulting models are automatically redundancy-free. As a consequence of these differences, CNA can identify common-cause and causal-chain structures and it can avoid the task of redundancy elimination (which creates various problems for QCA). Moreover, the algorithm does not require an input identifying the endogenous factors; it can infer that from the data. Finally, data fragmentation (limited diversity) does not force CNA to resort to counterfactual reasoning. 

The new functionalities provided by this version of the \pkg{cna} package include various model fit parameters measuring, for instance, the degree of correspondance between the configurations that are, in principle, compatible with causal models and the configurations actually contained in the data. Moreover, multi-outcome models are now tested for structural redundancies and a new function has been added that eliminates such redundancies. Also, algorithmic efficiency has been significantly increased. Finally, the package now comes with an extensive vignette that presents the theoretical background of CNA and introduces to causal modeling with \pkg{cna} on the basis of many concrete examples.



}

\details{
    \tabular{ll}{
Package: \tab cna\cr
Type:    \tab Package\cr
Version: \tab 2.1.0\cr
Date:    \tab 2018-06-01\cr
License: \tab GPL (>= 2)\cr
    }
}

\author{
\bold{Authors}:\cr
Mathias Ambuehl\cr
\email{mathias.ambuehl@consultag.ch}

Michael Baumgartner\cr
Department of Philosophy\cr
University of Bergen\cr
\email{michael.baumgartner@uib.no}


\bold{Maintainer}:\cr
Mathias Ambuehl
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

Baumgartner, Michael and Alrik Thiem. 2015. \dQuote{Identifying Complex Causal Dependencies in Configurational Data with Coincidence Analysis}, \emph{The R Journal} 7:176-184.

Mackie, John L. 1974. \emph{The Cement of the Universe: A Study of Causation}. Oxford: Oxford University Press.

McCluskey, Edward J. 1965. \emph{Introduction to the Theory of Switching Circuits}. Princeton: Princeton University Press.

Quine, Willard v. O. 1959. \dQuote{On Cores and Prime Implicants of Truth Functions.} \emph{The American
Mathematical Monthly} 66:755-760.

Ragin, Charles C. 1987. \emph{The Comparative Method}. Berkeley: University of California Press.

Ragin, Charles C. 2008. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. Chicago:
University of Chicago Press.
}

\keyword{package}
