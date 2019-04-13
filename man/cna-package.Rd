\name{cna-package}
\alias{cna-package}


\docType{package}

\title{
cna: A Package for Causal Modeling with Coincidence Analysis
}

\description{
\dfn{Coincidence Analysis} (CNA) is a configurational comparative method of causal data analysis that was first introduced for crisp-set (i.e. binary) data in Baumgartner (2009a, 2009b, 2013) and generalized for multi-value and fuzzy-set data in Baumgartner and Ambuehl (2018). The \pkg{cna} package reflects and implements the method's latest stage of development. 

CNA is related to Qualitative Comparative Analysis (QCA) (Ragin 1987, 2008). Like QCA, CNA processes configurational data, i.e. data consisting of observed cases featuring different factor configurations, it searches for redundancy-free sufficient and necessary conditions of causally modeled outcomes, it places a Boolean ordering on causally relevant factor values (instead of e.g. quantifying net effects and effect sizes in the vein of regression analysis), and it draws on the same regularity theoretic notion of causation as QCA, i.e. the notion first introduced by Mackie (1974). Contrary to QCA, however, CNA is custom-built to treat multiple factors as outcomes, and it does not generate causal models from the top down by first building maximal Boolean dependency structures and then gradually eliminating redundant elements (using e.g. Quine-McCluskey optimization; cf. McCluskey 1965); rather, CNA builds causal models from the bottom up by gradually combining single factor values to complex dependency structures until the requested thresholds of model fit are met, such that the resulting models are automatically redundancy-free. As a consequence of these differences, CNA can identify common-cause and causal-chain structures and it can avoid the task of redundancy elimination (which creates various problems for QCA). Moreover, the algorithm does not require an input identifying the endogenous factors; it can infer that from the data. Finally, data fragmentation (limited diversity) does not force CNA to resort to counterfactual reasoning. 

The new functionalities provided by version 2.2 of the \pkg{cna} package include functions for evaluating and benchmarking CNA's output. The functions randomAsf() and randomCsf() draw data-generating structures with one and multiple outcomes, respectively, as a basis for inverse searches. By means of is.submodel(), the CNA output can be scanned for correctness-preserving models. Moreover, is.inus() determines whether a solution is an INUS model, minimalize() brings non-INUS models into INUS form, and cyclic() checks whether causal models contain cyclic substructures, i.e. feedback loops. The package vignette, which presents the theoretical background of CNA and introduces to causal modeling with \pkg{cna}, has been updated accordingly. In particular, a section on correctness benchmarking has been added. 



}

\details{
    \tabular{ll}{
Package: \tab cna\cr
Type:    \tab Package\cr
Version: \tab 2.2.0\cr
Date:    \tab 2019-04-13\cr
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

Baumgartner, Michael and Mathias Ambuehl. 2018. \dQuote{Causal Modeling with Multi-Value and Fuzzy-Set Coincidence Analysis.} \emph{Political Science Research and Methods} doi:10.1017/psrm.2018.45. 

Baumgartner, Michael and Alrik Thiem. 2015. \dQuote{Identifying Complex Causal Dependencies in Configurational Data with Coincidence Analysis}, \emph{The R Journal} 7:176-184.

Mackie, John L. 1974. \emph{The Cement of the Universe: A Study of Causation}. Oxford: Oxford University Press.

McCluskey, Edward J. 1965. \emph{Introduction to the Theory of Switching Circuits}. Princeton: Princeton University Press.

Ragin, Charles C. 1987. \emph{The Comparative Method}. Berkeley: University of California Press.

Ragin, Charles C. 2008. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. Chicago:
University of Chicago Press.
}

\keyword{package}
