\name{cna}
\alias{cna}

\title{Perform Coincidence Analysis}

\description{
The \code{cna} function performs Coincidence Analysis to identify atomic solution formulas (asf) consisting of minimally necessary disjunctions of minimally sufficient conditions of all outcomes in the data
and combines the recovered asf to complex solution formulas (csf) representing multi-outcome structures, e.g. common-cause and/or causal chain structures. 
}

\usage{
cna(x, outcome = TRUE, con = 1, cov = 1, maxstep = c(3, 4, 10), 
    measures = c("standard consistency", "standard coverage"), 
    ordering = NULL, strict = FALSE, exclude = character(0), notcols = NULL, 
    what = if (suff.only) "m" else "ac", details = FALSE, 
    suff.only = FALSE, acyclic.only = FALSE, cycle.type = c("factor", "value"), 
    verbose = FALSE, control = NULL, ...)
}

\arguments{
  \item{x}{Data frame or  \code{\link{configTable}}.}
  \item{outcome}{Character vector specifying one or several factor values that are to be considered as potential outcome(s). For crisp- and fuzzy-set data, factor values are expressed by upper and lower cases, for multi-value data, they are expressed by the "factor=value" notation.  
  Defaults to \code{outcome = TRUE}, which means that values of all factors in \code{x} are considered as potential outcomes.}
\item{con}{Numeric scalar between 0 and 1 to set the threshold for the sufficiency measure selected in \code{measures[1]}, e.g. the consistency threshold. Every minimally sufficient condition (msc), atomic solution formula (asf), and complex solution formula (csf) must satisfy \code{con}. %(See also the argument \code{con.msc} below).
}
  \item{cov}{Numeric scalar between 0 and 1 to set the threshold for the necessity measure selected in \code{measures[2]}, e.g. the coverage threshold. Every asf and csf must satisfy \code{cov}.}
  \item{maxstep}{Vector of three integers; the first specifies the maximum number of conjuncts in each disjunct of an asf, the second specifies the maximum number of disjuncts in an asf, the third specifies the maximum \emph{complexity} of an asf. The complexity of an asf is 
  the total number of exogenous factor value appearances in the asf. Default: \code{c(3,4,10)}.}
  
  \item{measures}{Character vector of length 2. \code{measures[1]} specifies the measure to be used for sufficiency evaluation, \code{measures[2]} the measure to be used for necessity evaluation. Any measure from \code{\link{showConCovMeasures}} can be chosen. For more, see the \pkg{cna} package vignette, section 3.2. %The default is \code{measures = c("standard consistency", "standard coverage")}.
  }
  
  \item{ordering}{Character string or list of character vectors specifying the causal ordering of
        the factors in \code{x}. For instance, \code{ordering = "A,B > C"} determines that factors A and B are causally upstream of C.} 
        
  \item{strict}{Logical; if \code{TRUE}, factors on the same level of the causal
        ordering are \emph{not} potential causes of each other; if \code{FALSE} (default), factors on the same level \emph{are} potential causes of each other.}
        
  \item{exclude}{Character vector specifying factor values to be excluded as possible causes of certain outcomes. For instance, \code{exclude = "A,c->B"} determines that A and c are not considered as potential causes of B.}
  
  \item{notcols}{Character vector of factors to be negated in \code{x}. If \code{notcols = "all"}, all factors in \code{x} are negated.}
  
  \item{what}{Character string specifying what to print; \code{"t"} for the configuration table, \code{"m"} for msc, \code{"a"} for asf, \code{"c"} for csf, and \code{"all"} for all. Defaults to \code{"ac"} if \code{suff.only = FALSE}, and to \code{"m"} otherwise.}
  
  \item{details}{A character vector specifying the evaluation measures and additional solution attributes to be computed. Possible elements are all the measures in  \code{\link{showMeasures}}. Can also be \code{TRUE}/\code{FALSE}. If \code{FALSE} (default), no additional measures are returned; \code{TRUE} resolves to \code{c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence")}. See also \code{\link{detailMeasures}}.}
  
  \item{suff.only}{Logical; if \code{TRUE}, the function only searches for msc and not for asf and csf.}
  
  \item{acyclic.only}{Logical; if \code{TRUE}, csf featuring a cyclic substructure are not returned. \code{FALSE} by default.}
  
  \item{cycle.type}{Character string specifying what type of cycles to be detected: \code{"factor"} (the default) or \code{"value"}. Cf. \code{\link{cyclic}}.}
  
  \item{verbose}{Logical; if \code{TRUE}, some details on the csf building process are printed during the execution of the \code{cna} function. \code{FALSE} by default.}
  
  \item{control}{Argument for fine-tuning and modifying the CNA algorithm (in ways that are not relevant for the ordinary user). See \code{\link{cnaControl}} for more details.
  The default \code{NULL} is equivalent to \code{cnaControl(con.msc=con, type=<type>)}, where \code{<type>} is the type of the data \code{x}.}
  \item{\dots}{
  Arguments for fine-tuning; passed to \code{cnaControl}.}
}

\details{
The \strong{first input \code{x}} of the \code{cna} function is a data frame or a configuration table. The data can be crisp-set (cs), fuzzy-set (fs), or multi-value (mv). Factors in cs data can only take values from \{0,1\}, factors in fs data can take on any (continuous) values from the unit interval [0,1], while factors in mv data can take on any of an open (but finite) number of non-negative integers as values. To ensure that no misinterpretations of returned asf and csf can occur, users are advised to exclusively use upper case letters as factor (column) names. Column names may contain numbers, but the first sign in a column name must be a letter. Only ASCII signs should be used for column and row names.


A data frame or configuration table \code{x} is the sole mandatory input of the \code{cna} function. In particular, \code{cna} does not need an input specifying which factor(s) in \code{x} are endogenous, it tries to infer that from the data. But if it is known prior to the analysis what factors have values that can figure as outcomes, an \strong{outcome specification} can be passed to \code{cna} via the argument \code{outcome}, which takes as input a character vector identifying one or several factor values as potential outcome(s). For cs and fs data, outcomes are expressed by upper and lower cases (e.g. \code{outcome = c("A", "b")}). If factor names have multiple letters, any upper case letter is interpreted as 1, and the absence of upper case letters as 0 (i.e. \code{outcome = c("coLd", "shiver")} is interpreted as \code{COLD=1} and \code{SHIVER=0}). For mv data, factor values are assigned by the ``factor=value'' notation (e.g. \code{outcome = c("A=1","B=3")}). Defaults to \code{outcome = TRUE}, which means that all factor values in \code{x} are potential outcomes.

When the data \code{x} contain multiple potential outcomes, it may moreover be known, prior to the analysis, that these outcomes have a certain \strong{causal ordering}, meaning that some of them are causally upstream of the others. Such information can be passed to \code{cna} by means of the argument \code{ordering}, which takes either a character string or a list of character vectors as value. For example, \code{ordering = "A, B < C"} or, equivalently, \code{ordering = list(c("A",} \code{ "B"), "C")} determines that factor C is causally located \emph{downstream} of factors A and B, meaning that \emph{no values} of C are potential causes of values of A and B. In consequence, \code{cna} only checks whether values of A and B can be modeled as causes of values of C; the test for a causal dependency in the other direction is skipped.
An \code{ordering} does not need to explicitly mention all factors in \code{x}. If only a subset of the factors are included in the \code{ordering}, the non-included factors are entailed to be upstream of the included ones. Hence, \code{ordering = "C"} means that C is located downstream of all other factors in \code{x}.

The argument \bold{\code{strict}} determines whether the elements of one level in an ordering can be causally related or not. For example, if \code{ordering = "A, B < C"} and \code{strict = TRUE}, then the values of A and B---which are on the same level of the ordering---are excluded to be causally related and \code{cna} skips corresponding tests. By contrast, if \code{ordering = "A, B < C"} and \code{strict = FALSE}, then \code{cna} also searches for dependencies among the values of A and B. The default is \code{strict} \code{ = FALSE}. 

An \code{ordering} excludes \emph{all} values of a factor as potential causes of an outcome. But a user may only be in a position to exclude \emph{some} (not all) values as potential causes. Such information can be passed to \code{cna} through the argument \strong{\code{exclude}}, which can be assigned a vector of character strings featuring the factor values to be excluded as causes to the left of the "\code{->}" sign and the corresponding outcomes on the right. For example, \code{exclude = "A=1,C=3 -> B=1"} determines that the value 1 of factor A and the value 3 of factor C are excluded as  causes of the value 1 of factor B. Factor values can be excluded as potential causes of multiple outcomes as follows: \code{exclude = c("A,c -> B", "b,H -> D")}. For cs and fs data, upper case letters are interpreted as 1, lower case letters as 0. If factor names have multiple letters, any upper case letter is interpreted as 1, and the absence of upper case letters as 0. For mv data, the "factor=value" notation is required. 
To exclude \emph{all} values of a factor as potential causes of an outcome or to exclude a factor value as potential cause of \emph{all} values of some endogenous factor, a "\code{*}" can be appended to the corresponding factor name; for example: \code{exclude = "A* -> B"} or \code{exclude = "A=1,C=3 -> B*"}.
The \code{exclude} argument can be used both independently of and in conjunction with \code{outcome} and  \code{ordering}, but if assignments to \code{outcome} and \code{ordering} contradict assignments to \code{exclude}, the latter are ignored. If \code{exclude} is assigned values of factors that do not appear in the data \code{x}, an error is returned.

If no outcomes are specified and no causal ordering is provided, all factor values in \code{x} are treated as potential outcomes; more specifically, in case of cs and fs data, \code{cna} tests for all factors whether their presence (i.e. them taking the value 1) can be modeled as an outcome, and in case of  mv data, \code{cna} tests for all factors whether any of their possible values can be modeled as an outcome. That is done by searching for redundancy-free Boolean functions (in disjunctive normal form) that account for the behavior of an outcome in accordance with \code{exclude}.


The core Boolean dependence relations exploited for that purpose are sufficiency and necessity. To assess whether the (typically noisy) data warrant inferences to sufficiency and necessity, \code{cna} draws on \bold{evaluation measures for sufficiency and necessity}, which can be selected via the argument \code{measures}, expecting a character vector of length 2. The first element, \code{measures[1]}, specifies the measure to be used for sufficiency evaluation, and \code{measures[2]} specifies the measure to be used for necessity evaluation. All eight available evaluation measures can be printed to the console through \code{\link{showConCovMeasures}}. Four of them are sufficiency measures---variants of consistency (Ragin 2006)---, and four are necessity measures---variants of coverage (Ragin 2006). They implement different approaches for assessing whether the evidence in the data justifies an inference to sufficiency or necessity, respectively (cf. De Souter 2024; De Souter & Baumgartner 2025). The default is \code{measures = c("standard consistency", "standard coverage")}. More details are provided in section 3.2 of the \pkg{cna} package vignette (call \code{vignette("cna")}).

Against that background, \code{cna} first identifies, for each potential outcome in \code{x}, all minimally sufficient conditions (msc) that meet the threshold given to the selected sufficiency measure in the argument \code{con}. Then, these msc are disjunctively combined to minimally necessary conditions that meet the threshold for the selected necessity measure given to the argument \code{cov}, such that the whole disjunction meets \code{con}. The default value for \code{con} and \code{cov} is 1. The expressions resulting from this procedure are the atomic solution formulas (asf) for every factor value that can be modeled as an outcome. Asf represent causal structures with one outcome. To model structures with more than one outcome, the recovered asf are conjunctively combined to complex solution formulas (csf). To build its models, \code{cna} uses a \strong{bottom-up search algorithm}, which we do not reiterate here (see Baumgartner and Ambuehl 2020 or the section 4 of \code{vignette("cna")}).

%\describe{
%\item{First stage}{On the basis of \code{outcome} and \code{ordering}, the algorithm builds a set of potential outcomes \strong{O} from the factors in \code{x}; and on the basis of \code{ordering} and \code{exclude}, it assigns a set of potential causes to each potential outcome. At the default values of \code{outcome}, \code{ordering}, and \code{exclude}, all factor values in \code{x} are treated as potential outcomes and  as potential causes.}

%\item{Second stage}{The algorithm checks whether single factor values, e.g. A, b, C, (where "A" stands for "A=1" and "b" for "B=0") or D=3, E=2, etc., (whose membership scores, in case of fs data, meet 0.5 in at least one case) are sufficient for a potential outcome in \strong{O} (where a factor value counts as sufficient iff it meets the threshold given by \code{con} to ). Next, conjuncts of two factor values, e.g. A*b, A*C, D=3*E=2 etc., (whose membership scores, in case of fs data, meet \code{cutoff} in at least one case) are tested for sufficiency. Then, conjuncts of three factors, and so on. Whenever a conjunction (or a single factor value) is found to be sufficient, all supersets of that conjunction contain redundancies and are, thus, not considered for the further analysis. The result is a set of msc for every potential outcome in \strong{O}. To recover certain target structures from noisy data, it may be useful to allow \code{cna} to also consider sufficient conditions for further analysis that are not minimal. This can be accomplished by setting \code{only.minimal.msc} to \code{FALSE}. A concrete example illustrating the utility of \code{only.minimal.msc} is provided in the \dQuote{Examples} section below. (The ordinary user is advised not to change the default value of this argument.)} 

%\item{Third stage}{Minimally necessary disjunctions are built for each potential outcome in \strong{O} by first testing whether single msc are necessary, then disjunctions of two msc, then of three, etc. (where a disjunction of msc counts as necessary iff it meets the threshold given by \code{cov}). Whenever a disjunction of msc (or a single msc) is found to be necessary, all supersets of that disjunction contain redundancies and are, thus, excluded from the further analysis. Finally, all and only those disjunctions of msc that meet both \code{cov} and \code{con} are issued as redundancy-free \strong{atomic solution formulas} (asf). To recover certain target structures from noisy data, it may be useful to allow \code{cna} to also consider necessary conditions for further analysis that are not minimal. This can be accomplished by setting \code{only.minimal.asf} to \code{FALSE}, in which case \emph{all} disjunctions of msc reaching the con and cov thresholds will be returned. (The ordinary user is advised not to change the default value of this argument.)  

As the combinatorial search space of this algorithm is often too large to be exhaustively scanned in reasonable time, the argument \bold{\code{maxstep}} allows for setting an upper bound for the complexity of the generated asf. \code{maxstep} takes a vector of three integers \code{c(i, j, k)} as input, entailing that the generated asf have maximally \code{j} disjuncts with maximally \code{i} conjuncts each and a total of maximally \code{k} factor value appearances (\code{k} is the maximal complexity). The default is \code{maxstep = c(3, 4, 10)}.

Note that when the data \code{x} feature noise, the default \code{con} and \code{cov} thresholds of 1 will often not yield any asf. In such cases, \code{con} and \code{cov} may be set to values below 1. \code{con} and \code{cov} should neither be set too high, in order to avoid overfitting, nor too low, in order to avoid underfitting. The \strong{overfitting danger} is severe in causal modeling with CNA (and configurational causal modeling more generally). For a discussion of this problem see Parkkinen and Baumgartner (2023), who also introduce a procedure for robustness assessment that explores all threshold settings in a given interval---in an attempt to reduce both over- and underfitting. See also the R package \CRANpkg{frscore}. 



If \code{verbose} is set to its non-default value \code{TRUE}, some information about the progression of the algorithm is returned to the console during the execution of the \code{cna} function. The execution can easily be interrupted by ESC at all stages.

The \bold{default output} of \code{cna} first lists the provided ordering (if any), second, the pre-identified outcomes (if any), third, the implemented sufficiency and necessity measures, fourth, the recovered asf, and fifth, the csf. Asf and csf are ordered by complexity and the product of their \code{con} and \code{cov} scores.
For asf and csf, three attributes are standardly computed: \code{con}, \code{cov}, and \code{complexity}. The first two correspond to a solution's scores on the selected sufficiency and necessity \code{measures}, and the \code{complexity} score amounts to the number of factor value appearances on the left-hand sides of ``\code{->}'' or ``\code{<->}'' in asf and csf.

Apart from the evaluation \code{measures} used for model building through the \code{measures} argument, \code{cna} can also return the solution scores on all other available evaluation measures. This is accomplished by giving the \code{details} argument a character vector containing the names or aliases of the evaluation measures to be computed. For example, if \code{details = c("ccon", "ccov", "PAcon", "AAcov")}, the output of \code{cna} contains additional columns presenting the scores of the solutions on the requested measures.


In addition to measures evaluating the evidence for sufficiency and necessity, \code{cna} can calculate a number of  \bold{further solution attributes}: \code{exhaustiveness}, \code{faithfulness}, \code{coherence}, and \code{cyclic} all of which are recovered by requesting them through the \code{details} argument. Explanations of these attributes can be found in sections 5.2 to 5.4 of \code{vignette("cna")}.

The argument \code{notcols} is used to calculate asf and csf
for \bold{negative outcomes} in data of \code{type} cs and fs (in mv data \code{notcols} has no meaningful interpretation and, correspondingly, issues an error message). If \code{notcols = "all"}, all factors in \code{x} are negated,
i.e. their membership scores i are replaced by 1-i. If \code{notcols} is given a character vector 
of factors in \code{x}, only the factors in that vector are negated. For example, \code{notcols = c("A", "B")}
determines that only factors A and B are negated. The default is no negations, i.e. \code{notcols = NULL}.

\bold{\code{suff.only}} is applicable whenever a complete \code{cna} analysis cannot be performed for reasons of computational complexity. In such a case, \code{suff.only = TRUE} forces \code{cna} to stop the analysis after the identification of msc, which will normally yield results even in cases when a complete analysis does not terminate. In that manner, it is possible to shed at least some light on the dependencies among the factors in \code{x}, in spite of an incomputable solution space.

The argument \bold{\code{control}} provides a number of options to fine-tune and modify the CNA algorithm and the output of \code{cna}. It expects a list generated by the function \code{cnaControl} as input, for example, \code{control = cnaControl(inus.only = FALSE, inus.def = c("equivalence"), con.msc = 0.8)}.\if{html}{}\if{latex}{\cr} The available fine-tuning parameters are documented here: \code{\link{cnaControl}}. All of the arguments in \code{cnaControl} can also be passed to the \code{cna} function directly via \code{\dots}. They all have default values yielding the standard behavior of \code{cna}, which do not have to be changed by the ordinary CNA user.

The argument \code{what} regulates what items of the output of \code{cna} are printed. It has no effect on the computations that are performed when executing \code{cna}; it only determines how the result is printed. See \code{\link{print.cna}} for more information on \code{what}.
}

\value{
\code{cna} returns an object of class \dQuote{cna}, which amounts to a list with the following elements:

\tabular{rl}{
\code{call}: \tab the executed function call\cr
\code{x}:\tab the processed data frame or configuration table, as input to \code{cna}\cr
\code{ordering} \tab the ordering imposed on the factors in the configuration table (if not \code{NULL})\cr 
\code{configTable}: \tab a \dQuote{configTable} containing the the input data\cr
\code{solution}: \tab the solution object, which itself is composed of lists exhibiting msc and asf 
for all\cr \tab  outcome factors\cr
\code{measures}: \tab the evaluation \code{con}- and \code{cov}-measures used for model-building\cr
\code{what}:\tab the values given to the \code{what} argument\cr
\code{...}:\tab plus additional list elements conveying more details on the function call and the\cr\tab performed coincidence analysis.}
}

\note{In the first example described below (in \emph{Examples}), the two resulting complex solution formulas represent a common cause structure and a causal chain, respectively. The common cause structure is graphically depicted in figure (a) below, the causal chain in figure (b).

\if{html}{\figure{structures.png}{Causal Structures}}
\if{latex}{\figure{structures.pdf}{options: width=9cm}}
}



\references{
Aleman, Jose. 2009. \dQuote{The Politics of Tripartite Cooperation in New Democracies: A Multi-level Analysis.} \emph{International Political Science Review} 30 (2):141-162.

Basurto, Xavier. 2013. \dQuote{Linking Multi-Level Governance to Local Common-Pool 
Resource Theory using Fuzzy-Set Qualitative Comparative Analysis: Insights from 
Twenty Years of Biodiversity Conservation in Costa Rica.} \emph{Global Environmental Change} 23(3):573-87.

Baumgartner, Michael. 2009. \dQuote{Inferring Causal Complexity.}
\emph{Sociological Methods & Research} 38(1):71-101.

Baumgartner, Michael and Mathias Ambuehl. 2020. \dQuote{Causal Modeling with Multi-Value and Fuzzy-Set Coincidence Analysis.} \emph{Political Science Research and Methods}. 8:526--542.

Baumgartner, Michael and Christoph Falk. 2023. \dQuote{Boolean Difference-Making: A Modern Regularity Theory of Causation.} \emph{The British Journal for the Philosophy of Science}, 74(1), 171-197. doi:10.1093/bjps/axz047.

De Souter, Luna. 2024.  \dQuote{Evaluating Boolean Relationships in Configurational ComparativeMethods.} \emph{Journal of Causal Inference} 12(1). doi:10.1515/jci-2023-0014.

De Souter, Luna and Michael Baumgartner. 2025. \dQuote{New sufficiency and necessity measures for model building with Coincidence Analysis.} \emph{Zenodo}. https://doi.org/10.5281/zenodo.13619580

Hartmann, Christof, and Joerg Kemmerzell. 2010. \dQuote{Understanding Variations 
in Party Bans in Africa.} \emph{Democratization} 17(4):642-65.

Krook, Mona Lena. 2010.
\dQuote{Women's Representation in Parliament: A Qualitative Comparative Analysis.}
\emph{Political Studies} 58(5):886-908.

Mackie, John L. 1974. \emph{The Cement of the Universe: A Study of Causation.} Oxford: Oxford University Press.

Parkkinen, Veli-Pekka and Michael Baumgartner. 2023. \dQuote{Robustness and Model Selection in Configurational Causal Modeling.} \emph{Sociological Methods & Research},  52(1), 176-208.

Ragin, Charles C. 2006. \dQuote{Set Relations in Social Research: Evaluating Their Consistency and Coverage.} \emph{Political Analysis} 14(3):291-310.

Wollebaek, Dag. 2010.
\dQuote{Volatility and Growth in Populations of Rural Associations.}
\emph{Rural Sociology} 75:144-166.
}

\seealso{\code{\link[cna]{print.cna}}, \code{\link{configTable}}, \code{\link[cna]{condition}}, \code{\link{cyclic}}, \code{\link{condTbl}}, \code{\link{selectCases}}, \code{\link{makeFuzzy}}, \code{\link[cna]{some}},\cr   
\code{\link{randomConds}}, \code{\link{is.submodel}},  \code{\link{is.inus}}, \code{\link{showMeasures}}, \code{\link{redundant}}, \code{\link{full.ct}}, \code{\link{d.educate}},
\code{\link{d.women}},\cr  \code{\link{d.pban}},\code{\link{d.autonomy}}, \code{\link{d.highdim}}}

\examples{
# Ideal crisp-set data from Baumgartner (2009) on education levels in western democracies
# ----------------------------------------------------------------------------------------
# Exhaustive CNA without constraints on the search space; print atomic and complex 
# solution formulas (default output).
cna.educate <- cna(d.educate)
cna.educate
# The two resulting complex solution formulas represent a common cause structure 
# and a causal chain, respectively. The common cause structure is graphically depicted 
# in (Note, figure (a)), the causal chain in (Note, figure (b)).

# Build solutions with other than standard evaluation measures.
cna(d.educate, measures = c("ccon", "ccov"))
cna(d.educate, measures = c("PAcon", "PACcov"))

# CNA with negations of the factors E and L.
cna(d.educate, notcols = c("E","L"))
# The same by use of the outcome argument.
cna(d.educate, outcome = c("e","l"))

# CNA with negations of all factors.
cna(d.educate, notcols = "all")

# Print msc, asf, and csf with additional evaluation measures and solution attributes.
cna(d.educate, what = "mac", details = c("ccon","ccov","PAcon","PACcov","exhaustive"))
cna(d.educate, what = "mac", details = c("e","f","AACcon","AAcov"))
cna(d.educate, what = "mac", details = TRUE)

# Print solutions without spaces before and after "+".
options(spaces = c("<->", "->" ))
cna(d.educate, details = c("e", "f"))

# Print solutions with spaces before and after "*".
options(spaces = c("<->", "->", "*" ))
cna(d.educate, details = c("e", "f", "PAcon", "PACcov"))

# Restore the default of the option "spaces".
options(spaces = c("<->", "->", "+"))


# Crisp-set data from Krook (2010) on representation of women in western-democratic
# parliaments
# -----------------------------------------------------------------------------------
# This example shows that CNA can distinguish exogenous and endogenous factors in the 
# data. Without being told which factor is the outcome, CNA reproduces the original 
# QCA of Krook (2010).
ana1 <- cna(d.women, measures = c("PAcon", "PACcov"), details = c("e", "f"))
ana1

# The two resulting asf only reach an exhaustiveness score of 0.438, meaning that
# not all configurations that are compatible with the asf are contained in the data
# "d.women". Here is how to extract the configurations that are compatible with 
# the first asf but are not contained in "d.women".
\donttest{library(dplyr)
setdiff(ct2df(selectCases(asf(ana1)$condition[1], full.ct(d.women))),
        d.women)
}

# Highly ambiguous crisp-set data from Wollebaek (2010) on very high volatility of 
# grassroots associations in Norway
# --------------------------------------------------------------------------------
# csCNA with ordering from Wollebaek (2010) [Beware: due to massive ambiguities,  
# this analysis will take about 20 seconds to compute.]
\donttest{cna(d.volatile, ordering = "VO2", maxstep = c(6, 6, 16))}
              
# Using suff.only, CNA can be forced to abandon the analysis after minimization of 
# sufficient conditions. [This analysis terminates quickly.]
cna(d.volatile, ordering = "VO2", maxstep = c(6, 6, 16), suff.only = TRUE)

# Similarly, by using the default maxstep, CNA can be forced to only search for asf 
# and csf with reduced complexity.
\donttest{cna(d.volatile, ordering = "VO2")}

# ordering = "VO2" only excludes that the values of VO2 are causes of the values
# of the other factors in d.volatile, but cna() still tries to model other factor 
# values as outcomes. The following call determines that only VO2 is a possible 
# outcome. (This call terminates quickly.)
cna(d.volatile, outcome = "VO2")

# We can even increase maxstep.
\donttest{cna(d.volatile, outcome = "VO2", maxstep=c(4,4,16))}

# If it is known that, say, el and od cannot be causes of VO2, we can exclude this.
cna(d.volatile, outcome = "VO2", maxstep=c(4,4,16), exclude = "el, od -> VO2")

# The verbose argument returns information during the execution of cna().
\donttest{cna(d.volatile, ordering = "VO2", verbose = TRUE)}


# Multi-value data from Hartmann & Kemmerzell (2010) on party bans in Africa
# ---------------------------------------------------------------------------
# mvCNA with an outcome specification taken from Hartmann & Kemmerzell 
# (2010); standard coverage threshold at 0.95 (standard consistency threshold at 1),
# maxstep at c(6, 6, 10).
cna.pban <- cna(d.pban, outcome = "PB=1", cov = .95, maxstep = c(6, 6, 10), 
                  what = "all")
cna.pban

# The previous function call yields a total of 14 asf and csf, only 5 of which are 
# printed in the default output. Here is how to extract all 14 asf and csf.
asf(cna.pban)
csf(cna.pban)

# [Note that all of these 14 causal models reach better consistency and 
# coverage scores than the one model Hartmann & Kemmerzell (2010) present in their  
# paper, which they generated using the TOSMANA software, version 1.3. 
# T=0 + T=1 + C=2 + T=1*V=0 + T=2*V=0 <-> PB=1]
condTbl("T=0 + T=1 + C=2 + T=1*V=0 + T=2*V=0 <-> PB = 1", d.pban)

# Extract all minimally sufficient conditions with further details.
msc(cna.pban, details = c("ccon", "ccov", "PAcon", "PACcov"))

# Alternatively, all msc, asf, and csf can be recovered by means of the nsolutions
# argument of the print function, which also allows for adding details.
print(cna.pban, nsolutions = "all", details = c("AACcon", "AAcov", "ex", "fa"))

# Print the configuration table with the "cases" column.
print(cna.pban, what = "t", show.cases = TRUE)

# Build solution formulas with maximally 4 disjuncts.
\donttest{cna(d.pban, outcome = "PB=1", cov = .95, maxstep = c(4, 4, 10))
  
# Use non-standard evaluation measures for solution building.
cna(d.pban, outcome = "PB=1", cov = .95, measures = c("PAcon", "PACcov"))

# Only print 2 digits of standard consistency and coverage scores.
print(cna.pban, digits = 2)

# Build all but print only two msc for each factor and two asf and csf.
print(cna(d.pban, outcome = "PB=1", cov = .95,
          maxstep = c(6, 6, 10), what = "all"), nsolutions = 2)

# Lowering the thresholds on standard consistency and coverage yields further 
# models with excellent fit scores; print only asf.
cna(d.pban, outcome = "PB=1", con = .93, what = "a", maxstep = c(6, 6, 10))

# Lowering both standard consistency and coverage. 
cna(d.pban, outcome = "PB=1", con = .9, cov =.9, maxstep = c(6, 6, 10))

# Lowering both standard consistency and coverage and excluding F=0 as potential 
# cause of PB=1.
cna(d.pban, outcome = "PB=1", con = .9, cov =.9, maxstep = c(6, 6, 10), 
    exclude = "F=0 -> PB=1")
      
# Specifying an outcome is unnecessary for d.pban. PB=1 is the only 
# factor value in those data that could possibly be an outcome.
cna(d.pban, con=.9, cov = .9, maxstep = c(6, 6, 10))
}

# Fuzzy-set data from Basurto (2013) on autonomy of biodiversity institutions in Costa Rica
# ---------------------------------------------------------------------------------------
# Basurto investigates two outcomes: emergence of local autonomy and endurance thereof. The 
# data for the first outcome are contained in rows 1-14 of d.autonomy, the data for the second
# outcome in rows 15-30. For each outcome, the author distinguishes between local ("EM",  
# "SP", "CO"), national ("CI", "PO") and international ("RE", "CN", "DE") conditions. Here,   
# we first apply fsCNA to replicate the analysis for the local conditions of the endurance of 
# local autonomy.
dat1 <- d.autonomy[15:30, c("AU","EM","SP","CO")]
cna(dat1, ordering = "AU", strict = TRUE, con = .9, cov = .9)

# The CNA model has significantly better consistency (and equal coverage) scores than the 
# model presented by Basurto (p. 580): SP*EM + CO <-> AU, which he generated using the 
# fs/QCA software.
condition("SP*EM + CO <-> AU", dat1) # both EM and CO are redundant to account for AU

# If we allow for dependencies among the conditions by setting strict = FALSE, CNA reveals 
# that SP is a common cause of both AU and EM.
cna(dat1, ordering = "AU", strict = FALSE, con = .9, cov = .9)

# Here are two analyses at different con/cov thresholds for the international conditions
# of autonomy endurance.
dat2 <- d.autonomy[15:30, c("AU","RE", "CN", "DE")]
cna(dat2, ordering = "AU", con = .9,  cov = .85)
cna(dat2, ordering = "AU", con = .85, cov = .9, details = TRUE)

# Here are two analyses of the whole dataset using different evaluation measures.
# They show that across the whole period 1986-2006, the best causal model of local
# autonomy (AU) renders that outcome dependent only on local direct spending (SP).
\donttest{cna(d.autonomy, outcome = "AU", con = .85, cov = .9, 
    maxstep = c(5, 5, 11), details = TRUE)
cna(d.autonomy, outcome = "AU", measures = c("AACcon","AAcov"), con = .85, cov = .9, 
    maxstep = c(5, 5, 11), details = TRUE)}
      
      
# High-dimensional data
# ---------------------
# Here's an analysis of the data d.highdim with 50 factors, massive 
# fragmentation, and 20\% noise. (Takes about 15 seconds to compute.)
head(d.highdim)
\donttest{cna(d.highdim,  outcome = c("V13", "V11"), con = .8, cov = .8)}

# By lowering maxstep, computation time can be reduced to less than 1 second
# (at the cost of an incomplete solution).
cna(d.highdim,  outcome = c("V13", "V11"), con = .8, cov = .8,
    maxstep = c(2,3,10))      


# Highly ambiguous artificial data to illustrate exhaustiveness and acyclic.only
# ------------------------------------------------------------------------------
mycond <- "(D + C*f <-> A)*(C*d + c*D <-> B)*(B*d + D*f <-> C)*(c*B + B*f <-> E)"
dat1 <- selectCases(mycond)
\donttest{ana1 <- cna(dat1, details = c("e","cy"))
# There exist almost 2M csf. This is how to build the first 927 of them, with 
# additional messages about the csf building process.
first.csf <- csf(ana1, verbose = TRUE)
first.csf
# Most of these csf are compatible with more configurations than are contained in 
# dat1. Only 141 csf in first.csf are perfectly exhaustive (i.e. all compatible 
# configurations are contained in dat1).
subset(first.csf, exhaustiveness == 1)

# All of the csf in first.csf contain cyclic substructures.
subset(first.csf, cyclic == TRUE)

# Here's how to build acyclic csf.
ana2 <- cna(dat1, details = c("e","cy"), acyclic.only = TRUE)
csf(ana2, verbose = TRUE)}

}

