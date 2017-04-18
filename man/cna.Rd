\name{cna}
\alias{cna}
\alias{cscna}
\alias{mvcna}
\alias{fscna}
\alias{print.cna}


\title{Perform Coincidence Analysis}

\description{
The \code{cna} function performs Coincidence Analysis to identify atomic solution formulas (asf) consisting of minimally necessary
disjunctions of minimally sufficient conditions of all outcomes in the data
and combines the recovered asf to complex solution formulas (csf) representing multi-outcome structures, e.g. common-cause and/or
causal-chain structures.
}

\usage{
cna(x, type, ordering = NULL, strict = FALSE, con = 1, cov = 1, con.msc = con,
    notcols = NULL, rm.const.factors = TRUE, rm.dup.factors = TRUE,  
    maxstep = c(3, 3, 9), only.minimal.msc = TRUE, maxSol = 1e6, 
    suff.only = FALSE, what = "mac", 
    cutoff = 0.5, border = c("down", "up", "drop"))
cscna(...)
mvcna(...)
fscna(...)

\method{print}{cna}(x, what = x$what, digits = 3, nsolutions = 5,
      show.cases = NULL, ...)
}

\arguments{
  \item{x}{A data frame or an object of class \dQuote{truthTab} (as output by \code{\link{truthTab}}).}
  \item{type}{A character vector specifying the type of \code{x}: \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set).}
  \item{ordering}{A list of character vectors specifying the causal ordering of
        the factors in \code{x}.}
  \item{strict}{Logical; if \code{TRUE}, factors on the same level of the causal
        ordering are \emph{not} potential causes of each other; if \code{FALSE}, factors on the same level \emph{are} potential causes of each other.}
  \item{con}{Numeric scalar between 0 and 1 to set the minimum consistency threshold every minimally sufficient condition (msc), atomic solution formula (asf), and complex solution formula (csf) must satisfy. (See also the argument \code{con.msc} below).}
  \item{cov}{Numeric scalar between 0 and 1 to set the minimum coverage threshold every asf and csf must satisfy.}
  \item{con.msc}{Numeric scalar between 0 and 1 to set the minimum consistency threshold every msc must satisfy. Allows for imposing a consistency threshold on msc that differs from the value \code{con} imposes on asf and csf. Defaults to \code{con}.}
  \item{maxstep}{Vector of three integers; the first specifies the maximum number of conjuncts in each disjunct of an asf, the second specifies the maximum number of disjuncts in an asf, the third specifies the maximum \emph{complexity} of an asf. The complexity of an asf is an integer defined to be the sum of the number of conjuncts in all of its disjuncts, i.e. the total number of exogenous factors in the asf.}
  \item{only.minimal.msc}{Logical; if \code{TRUE} (the default), only minimal conjunctions are retained as msc. If \code{FALSE}, sufficient conditions are not required to be minimal, in which case the number of msc will usually be much greater.}
  \item{maxSol}{Maximum number of asf calculated.}
  \item{suff.only}{Logical; if \code{TRUE}, the function only searches for msc and does not search for asf and csf.}
  \item{notcols}{A character vector of factors to be negated in \code{x}. If \code{notcols = "all"}, all factors in \code{x} are negated.}
  \item{rm.const.factors, rm.dup.factors}{Logical; if \code{TRUE} (default), factors with constant values are removed and all but the first of a set of duplicated factors are removed. These parameters are passed to \code{\link{truthTab}}.}
  \item{what}{A character vector specifying what to print; \code{"t"} for the truth table, \code{"m"} for msc, \code{"a"} for asf, \code{"c"} for csf, and \code{"all"} for all.}
  \item{cutoff}{Minimum membership score required for a factor to count as instantiated in the data and to be integrated in the analysis. Value in the unit interval (0,1). The default cutoff is 0.5. Only meaningful if \code{type="fs"}.}
  \item{border}{A character vector specifying whether factors with membership scores equal to \code{cutoff} are rounded up (\code{"up"}), rounded down (\code{"down"}) or dropped from the analysis (\code{"drop"}). Only meaningful if \code{type="fs"}. }
  \item{digits}{Number of digits to print in consistency and coverage scores.}
  \item{nsolutions}{Maximum number of msc, asf, and csf to print. Alternatively, \code{nsolutions="all"} will print all solutions.}
  \item{show.cases}{Logical; if \code{TRUE}, the truthTab's attribute \dQuote{cases}
        is printed. See \code{\link{print.truthTab}}}
  \item{\dots}{
        In \code{cscna}, \code{mvcna}, \code{fscna}: any formal argument of \code{cna} except \code{type}.
        In \code{print.cna}: arguments passed to other \code{print}-methods.}
}

\details{
The first input \code{x} of the \code{cna} function is a data frame or an object of class \dQuote{truthTab} as issued by \code{\link{truthTab}}. To ensure that no misinterpretations of issued asf and csf can occur, users are advised to use only upper case letters as factor (column) names. Column names may contain numbers, but the first sign in a column name must be a letter.

\code{cna} must be told what type of data \code{x} contains, unless \code{x} is a \code{truthTab}. In the latter case, the type of \code{x} is already defined. Data that feature factors taking values 1 or 0 only are called \emph{crisp-set}, in which case the \code{type} argument takes its default value \code{"cs"}. If the data contain at least one factor that takes more than two values, e.g. \{1,2,3\}, the data count as \emph{multi-value}, which is indicated by \code{type = "mv"}. Data featuring at least one factor taking real values from the interval [0,1] count as \emph{fuzzy-set}, which is specified by \code{type = "fs"}. Note that data comprising both multi-value and fuzzy-set factors cannot be meaningfully modeled causally. Such data must be properly calibrated prior to processing it with \code{cna}. To abbreviate the specification of the data type using the \code{type} argument, the functions \code{cscna(x, ...)}, \code{mvcna(x, ...)}, and \code{fscna(x, ...)} are available as shorthands for \code{cna(x, type = "cs", ...)}, \code{cna(x, type = "mv", ...)}, and \code{cna(x, type = "fs", ...)}, respectively.

A data frame or truth table \code{x} with a corresponding type specification is the only mandatory input of the \code{cna} function. If no causal ordering is provided (see below), \code{cna} tests for all factors in \code{x} whether they can be modeled as outcomes. This is done by, first, searching all minimally sufficient conditions (msc) that meet the threshold given by \code{con.msc} (resp. \code{con}, if \code{con.msc = con}) for each factor in \code{x}.
Then, \code{cna} disjunctively combines these msc to minimally
necessary conditions that meet the threshold given by \code{cov} such that the whole disjunction meets the threshold given by \code{con}. The resulting
expressions are the atomic solution formulas (asf) for every factor that can be modeled as outcome. The default value for \code{con.msc}, \code{con}, and \code{cov} is 1. 

[Consistency and coverage measures have originally been introduced by Ragin (2006) for QCA. Informally put, consistency reproduces the degree to which the behavior of an outcome obeys a corresponding sufficiency or necessity relationship or a whole causal model, whereas coverage reproduces the degree to which a sufficiency or necessity relationship or a whole model accounts for the behavior of the corresponding outcome. As the implication operator underlying the notions of sufficiency and necessity is defined differently in classical and in fuzzy logic, the two measures are defined differently for crisp-set and multi-value data, on the one hand, and fuzzy-set data, on the other. For details cf. Ragin (2006).]

\code{cna} builds msc and asf \emph{from the bottom up}. That is, in a first step, \code{cna}
checks whether single factors A, B, C, etc., whose membership scores meet \code{cutoff} in at least one case, are sufficient for an outcome (where a factor counts as sufficient iff it meets the threshold given by \code{con.msc}). Next, conjuncts of two factors A*B, A*C, B*C etc., whose membership scores meet \code{cutoff} in at least one case, are tested for sufficiency. Then, conjuncts of three factors, and so on. Whenever a conjunction of factors (or a single factor) is found to be sufficient, all supersets of that conjunction contain redundancies and are, thus, not considered for the further analysis. The result of that first phase is a set of msc for every outcome. To recover certain target structures in cases of noisy data, it may be useful to allow \code{cna} to also consider sufficient conditions for further analysis that are not strictly speaking minimal. This can be accomplished by setting \code{only.minimal.msc} to \code{FALSE}. A concrete example illustrating the purpose of \code{only.minimal.msc} is provided in the example section below.

In the next phase, minimally necessary disjunctions are built for each outcome by first testing whether single msc are necessary, then disjunctions of two msc, then of three, etc (where a disjunction of msc counts as necessary iff it meets the threshold given by \code{cov}). Whenever a disjunction of msc (or a single msc) is found to be necessary, all supersets of that disjunction contain redundancies and are, thus, excluded from the further analysis. Finally, all and only those disjunctions of msc that meet both \code{cov} and \code{con} are issued as redundancy-free asf.

As the combinatorial search space for asf is potentially too large to be exhaustively scanned in reasonable time, the argument \code{maxstep} allows for setting an upper bound for the complexity of the generated asf. \code{maxstep} takes a vector of three integers \code{c(i,j,k)} as input, entailing that the generated asf have maximally \code{j} disjuncts with maximally \code{i} conjuncts each and a total of maximally \code{k} factors (\code{k} is the maximal complexity). The default is \code{maxstep = c(3,3,9)}. 

Note that the default \code{con} and \code{cov} thresholds of 1 will often not yield any asf because real-life data tend to feature noise due to uncontrolled background influences. In such cases, users should gradually lower \code{con} and \code{cov} (e.g. in steps of 0.05) until \code{cna} finds solution formulas---for the aim of a CNA is to find solutions with the highest possible consistency and coverage scores. \code{con} and \code{cov} should only be lowered below 0.75 with great caution. If thresholds of 0.75 do not result in solutions, the corresponding data feature such a high degree of noise that there is a severe risk of causal fallacies.  

If \code{cna} finds asf, it combines them to complex solution formulas (csf). Asf with identical outcomes are not combined, for they do not represent a complex causal structure but model ambiguities with respect to one outcome. Asf with different outcomes can be concatenated to csf using two different signs: "*" and ",". If asf1 and asf2 have at least one factor in common, they are combined to "asf1 * asf2"; if they have no common factor, they are combined to "asf1, asf2". That is, csf with "*" as main operator represent \emph{cohering} complex causal structures and the degree of coherence in the analyzed data is issued as coherence score (cf. \code{\link{coherence}}). Csf with "," as main operator represent \emph{non-cohering} structures. For instance, the two asf (D + U <-> L) and (G + L <-> E) can be combined to the cohering csf "(D + U <-> L) * (G + L <-> E)", which represents a causal chain from D + U via L to E, whereas (D + U <-> L) and (G + F <-> E) yield the non-cohering csf "(D + U <-> L), (G + F <-> E)".

\code{cna} does not need to be told which factor(s) are endogenous, it can infer that from the data. Still, when prior causal knowledge about an investigated process is available, \code{cna}
can be prohibited from treating certain factors as potential causes of other factors by
means of the argument \code{ordering}. If specified, that argument defines a causal
ordering for the factors in \code{x}. For example,
\code{ordering = list(c("A",} \code{ "B"), "C")} determines that C is causally
located \emph{after} A and B, meaning that C is \emph{not}
a potential cause of A and B. In consequence, \code{cna} only checks
whether A and B can be modeled as causes of C; the test for a
causal dependency in the other direction is skipped. If the argument \code{ordering}
is not specified or if it is given the \code{NULL} value (which is the argument's default value),
\code{cna} searches for dependencies between all factors in \code{x}. An \code{ordering} does not need to explicitly mention all factors in an analyzed data frame. If only a subset of the factors are included in the \code{ordering}, the non-included factors are entailed to be causally before the included ones. Hence, \code{ordering = list("C")}, for instance, means that C is causally located after all other factors in the data, meaning that C is the ultimate outcome of the structure under scrutiny. 

The argument \code{strict} determines whether the elements of one level in an
ordering can be causally related or not. For example, if
\code{ordering = list(c("A", "B"), "C")} and \code{strict = TRUE}, then A and B---which are on the same level of the ordering---are excluded to be causally related
and \code{cna} skips corresponding tests. By contrast, if
\code{ordering = list(c("A", "B"), "C")} and \code{strict = FALSE}, then \code{cna}
also searches for dependencies among A and B. The default is \code{strict} \code{ = FALSE}. If the user knows prior to the analysis that the data contains exactly one endogenous factor E and that the remaining exogenous factors are mutually causally independent, the appropriate function call should feature \code{cna(..., ordering = list("E"), strict = TRUE,...)}. 

The argument \code{notcols} is used to calculate asf and csf
for negated factors (negative outcomes) in data of \code{type} \code{"cs"} and \code{"fs"} (in data of \code{type} \code{"mv"} \code{notcols} has no meaningful interpretation and, correspondingly, issues an error message). If \code{notcols = "all"}, all factors in \code{x} are negated,
i.e. their membership scores i are replaced by 1-i. If \code{notcols} is given a character vector 
of factors in \code{x}, only the factors in that vector are negated. For example, \code{notcols = c("A", "B")}
determines that only factors A and B are negated. The default is no negations, i.e. \code{notcols = NULL}.

\code{suff.only} is applicable in cases of very ambiguous solutions. It may happen
that \code{x} can be modeled in terms of so many asf and csf that \code{cna}
does not terminate in reasonable time. In such a case, \code{suff.only = TRUE} forces \code{cna} to stop the analysis after the identification of msc, which will normally yield results even in cases of extreme solution ambiguities. In that manner, it is possible to shed at least some light on the dependencies among the factors in \code{x}, in spite of an incomputable solution space.

\code{rm.const.factors} and \code{rm.dup.factors} are used to determine the handling of constant factors, i.e. factors with constant values in all cases (rows) listed in \code{x}, and of duplicated factors, i.e. factors that take identical value distributions in all cases in \code{x}. If \code{rm.const.factors = TRUE}, which is the default value, constant factors are removed from the data prior to the analysis, and if \code{rm.dup.factors = TRUE} (the default) all but the first of a set of duplicated factors are removed. From the perspective of configurational causal modeling, factors with constant values in all cases can neither be modeled as causes nor as outcomes; therefore, they can be removed prior to the analysis. Factors that take identical values in all cases cannot be distinguished configurationally, meaning they are one and the same factor as far as configurational causal modeling is concerned. Therefore, only one factor of a set of duplicated factors is standardly retained by \code{cna}.

The argument \code{what} can be specified both for the \code{cna} and the \code{print}
function. It regulates what elements of the output of \code{cna} are printed. If
\code{what} is given the value \dQuote{\code{t}}, the truth table is printed; if
it is given an \dQuote{\code{m}}, the msc are printed; if it is given an \dQuote{\code{a}},
the asf are printed; if it is given a \dQuote{\code{c}}, the csf are printed.
\code{what = "all"} or \code{what = "tmac"} determine that the full output is
printed.  Note that \code{what} has no effect on the computations that will be performed when executing \code{cna}; it only determines how the result will be printed.

The default output of \code{cna} is \code{what = "mac"}. It first returns the implemented ordering. Second, it lists all recovered msc for all potential outcomes in \code{x} along with their consistency and coverage scores as well as a measure for their complexity. Third, the asf and, fourth, the csf are reported. In addition to consistency, coverage and complexity, csf are returned with coherence scores (cf. \code{\link{coherence}}). If csf are the same as asf, this is indicated by "Same as asf".

\code{cna} only includes factor configurations in the analysis that are actually instantiated in the data. The argument \code{cutoff} determines the minimum membership score required for a factor or a combination of factors to count as instantiated. It takes values in the unit interval (0,1) with a default of 0.5. \code{border} specifies whether factor combinations with membership scores equal to \code{cutoff} are rounded up (\code{border = "up"}), rounded down (\code{border = "down"}), which is the default, or dropped from the analysis (\code{border = "drop"}).

The arguments \code{digits}, \code{nsolutions}, and \code{show.cases} apply to the \code{print} function, which takes an object of class \dQuote{cna} as first input. \code{digits} determines how many digits of consistency, coverage, and coherence scores
are printed, while \code{nsolutions} fixes the number of conditions and solutions
to print. \code{nsolutions} applies separately to minimally sufficient conditions,
atomic solution formulas, and complex solution formulas. \code{nsolutions = "all"} recovers all minimally sufficient conditions, atomic and complex solution formulas. \code{show.cases} is applicable if the \code{what} argument is given the value \dQuote{\code{t}}. In that case, \code{show.cases = TRUE} yields a truth table featuring a \dQuote{cases} column, which assigns cases to configurations.
}

\value{
\code{cna} returns an object of class \dQuote{cna}, which amounts to a list with the following components:

\tabular{rl}{
\code{call}: \tab the executed function call\cr
\code{x}:\tab the processed data frame or truth table\cr
\code{ordering}:\tab the implemented ordering\cr
\code{truthTab}: \tab the object of class "truthTab", as input to \code{cna}\cr
\code{truthTab_out}: \tab the object of class "truthTab", after modification according to \code{notcols}\cr
\code{solution}: \tab the solution object, which itself is composed of lists exhibiting msc, asf, and csf for\cr\tab all factors in \code{x}\cr
\code{what}:\tab the values given to the \code{what} argument
  }
}

\note{In the first example described below (in \emph{Examples}), the two resulting complex solution formulas represent a common cause structure and a causal chain, respectively. The common cause structure is graphically depicted in figure (a) below, the causal chain in figure (b).

\if{html}{\figure{structures3.png}{Causal Structures}}
\if{latex}{\figure{structures3.png}{options: width=13.5cm}}
}


\section{Contributors}{
Epple, Ruedi: development, testing\cr
Thiem, Alrik: testing
}



\references{
Basurto, Xavier. 2013. \dQuote{Linking Multi-Level Governance to Local Common-Pool 
Resource Theory using Fuzzy-Set Qualitative Comparative Analysis: Insights from 
Twenty Years of Biodiversity Conservation in Costa Rica.} \emph{Global Environmental Change} 23(3):573-87.

Baumgartner, Michael. 2009a. \dQuote{Inferring Causal Complexity.}
\emph{Sociological Methods & Research} 38(1):71-101.

Baumgartner, Michael. 2009b. \dQuote{Uncovering Deterministic Causal Structures:
A Boolean Approach.} \emph{Synthese} 170(1):71-96.

Hartmann, Christof, and Joerg Kemmerzell. 2010. \dQuote{Understanding Variations 
in Party Bans in Africa.} \emph{Democratization} 17(4):642-65.
DOI: 10.1080/13510347.2010.491189.

Krook, Mona Lena. 2010.
\dQuote{Women's Representation in Parliament: A Qualitative Comparative Analysis.}
\emph{Political Studies} 58(5):886-908.

Ragin, Charles C. 2006. \dQuote{Set Relations in Social Research: Evaluating Their Consistency and Coverage}. \emph{Political Analysis} 14(3):291-310.

Wollebaek, Dag. 2010.
\dQuote{Volatility and Growth in Populations of Rural Associations.}
\emph{Rural Sociology} 75:144-166.
}

\seealso{\code{\link{truthTab}}, \code{\link{condition}}, \code{\link{condTbl}}, \code{\link{selectCases}}, \code{\link{makeFuzzy}}, \code{\link{some}}, \code{\link{coherence}}, \code{\link{d.educate}}, \code{\link{d.women}}, \code{\link{d.pban}}, \code{\link{d.autonomy}}}

\examples{
# Ideal crisp-set data from Baumgartner (2009a) on education levels in western democracies
#---------------------------------------------------------------------------------------
# Load dataset.
data(d.educate)

# Exhaustive CNA without constraints on the search space; print complete solution without
# the truth table.
cna.educate <- cna(d.educate)
cna.educate

# The two resulting complex solution formulas represent a common cause structure 
# and a causal chain, respectively. The common cause structure is graphically depicted 
# in (Note, figure (a)), the causal chain in (Note, figure (b)).

# Print only complex solution formulas.
print(cna.educate, what = "c")

# Print only atomic solution formulas.
print(cna.educate, what = "a")

# Print only minimally sufficient conditions.
print(cna.educate, what = "m")

# Print only the truth table.
print(cna.educate, what = "t")

# CNA with negations of the factors E and L.
cna(d.educate, notcols = c("E","L"))

# CNA with negations of all factors.
cna(d.educate, notcols = "all")


# Crisp-set data from Krook (2010) on representation of women in western-democratic parliaments
# -------------------------------------------------------------------------------------------
# Load dataset. 
data(d.women)

# This example shows that CNA can infer which factors are causes and which ones
# are effects from the data. Without being told which factor is the outcome, 
# CNA reproduces the original QCA of Krook (2010).
\donttest{cna(d.women, maxstep = c(3, 4, 9))}


# Highly ambiguous crisp-set data from Wollebaek (2010) on very high volatility of 
# grassroots associations in Norway
#---------------------------------------------------------------------------------
# Load dataset. 
data(d.volatile)

# csCNA with ordering from Wollebaek (2010) [Beware: due to massive ambiguities, this analysis
# will take about 20 seconds to compute.]
\donttest{cna(d.volatile, ordering = list("VO2"), maxstep = c(6, 6, 16))}
              
# Using suff.only, CNA can be forced to abandon the analysis after minimization of sufficient 
# conditions. [This analysis terminates quickly.]
cna(d.volatile, ordering = list("VO2"), maxstep = c(6, 6, 16), suff.only = TRUE)

# Similarly, by using the default maxstep, CNA can be forced to only search for asf and csf
# with reduced complexity. [This analysis also terminates quickly.]
cna(d.volatile, ordering = list("VO2"))


# Multi-value data from Hartmann & Kemmerzell (2010) on party bans in Africa
# ---------------------------------------------------------------------------
# Load dataset. 
data(d.pban)

# mvCNA with causal ordering that corresponds to the ordering in Hartmann & Kemmerzell 
# (2010); coverage cutoff at 0.95 (consistency cutoff at 1), maxstep at (6, 6, 10).
cna.pban <- mvcna(d.pban, ordering = list(c("C","F","T","V"),"PB"), cov = 0.95,
                  maxstep = c(6, 6, 10))
cna.pban

# The previous function call yields a total of 14 asf and csf, only 5 of which are 
# printed in the default output. Here is how to extract all 14 asf and csf.
asf(cna.pban)
csf(cna.pban)

# [Note that all of these 14 causal models reach considerably better consistency and 
# coverage scores than the one model Hartmann & Kemmerzell (2010) present in their paper, 
# which they generated using the TOSMANA software, version 1.3: 
# T=0 + T=1 + C=2 + T=1*V=0 + T=2*V=0 <-> PB=1
mvcond("T=0 + T=1 + C=2 + T=1*V=0 + T=2*V=0 <-> PB = 1", d.pban)

# That is, not only does TOSMANA fail to recover model ambiguities in this case, it 
# also issues a model whose fit is significantly below the models this data set would 
# warrant.] 

# Extract all minimally sufficient conditions.
msc(cna.pban)

# Alternatively, all msc, asf, and csf can be recovered by means of the nsolutions
# argument of the print function.
print(cna.pban, nsolutions = "all")

# Print the truth table with the "cases" column.
print(cna.pban, what = "t", show.cases = TRUE)

\donttest{
# Build solution formulas with maximally 4 disjuncts.
mvcna(d.pban, ordering = list(c("C","F","T","V"),"PB"), cov = 0.95, maxstep = c(4, 4, 10))

# Only print 2 digits of consistency and coverage scores.
print(cna.pban, digits = 2)

# Build all but print only two msc for each factor and two asf and csf.
print(mvcna(d.pban, ordering = list(c("C","F","T","V"),"PB"), cov = 0.95,
      maxstep = c(6, 6, 10)), nsolutions = 2)

# Lowering the consistency instead of the coverage threshold yields further models with
# excellent fit scores; print only asf.
mvcna(d.pban, ordering = list(c("C","F","T","V"),"PB"), con = .93, what = "a",
      maxstep = c(6, 6, 10))

# Importing an ordering from prior causal knowledge is unnecessary for d.pban. PB  
# is the only factor in that data that could possibly be an outcome.
mvcna(d.pban, cov = 0.95, maxstep = c(6, 6, 10))
}

# Fuzzy-set data from Basurto (2013) on autonomy of biodiversity institutions in Costa Rica
# ---------------------------------------------------------------------------------------
# Load dataset. 
data(d.autonomy)

# Basurto investigates two outcomes: emergence of local autonomy and endurance thereof. The 
# data for the first outcome is contained in rows 1-14 of d.autonomy, the data for the second
# outcome in rows 15-30. For each outcome, the author distinguishes between local ("EM",  
# "SP", "CO"),  national ("CI", "PO") and international ("RE", "CN", "DE") conditions. Here,   
# we first apply fsCNA to replicate the analysis for the local conditions of the endurance of 
# local autonomy.
dat1 <- d.autonomy[15:30, c("AU","EM","SP","CO")]
fscna(dat1, ordering = list("AU"), strict = TRUE, con=.9, cov=.9)

# The fsCNA model has significantly better consistency (and equal coverage) scores than the 
# model presented by Basurto (p. 580): SP*EM + CO <-> AU, which he generated using the 
# fs/QCA software.
fscond("SP*EM + CO <-> AU", dat1) # both EM and CO are redundant to account for AU

# If we allow for dependencies among the conditions by setting strict = FALSE, CNA reveals 
# that SP is a common cause of both AU and EM:
fscna(dat1, ordering = list("AU"), strict = FALSE, con=.9, cov=.9)

# Here is the analysis for the international conditions of autonomy endurance, which
# yields the same model presented by Basurto (plus one model Basurto does not mention):
dat2 <- d.autonomy[15:30, c("AU","RE", "CN", "DE")]
fscna(dat2, ordering = list("AU"), con=.9, con.msc=.85, cov=.85)

# But there are other models that fare equally well.
fscna(dat2, ordering = list("AU"), con=.85, cov=.9)

# Finally, here is an analysis of the whole data set, showing that across the whole period 
# 1986-2006, the best causal model of local autonomy (AU) renders that outcome dependent
# only on local direct spending (SP):
\donttest{
fscna(d.autonomy, ordering = list("AU"), strict = TRUE, con=.85, cov=.9, 
      maxstep = c(5, 5, 11))
}

# Inverse search trials to assess the correctness of cna
# ------------------------------------------------------
# 1. ideal mv data, i.e. perfect consistencies and coverages, without data fragmentation.
\donttest{
dat1 <- allCombs(c(4, 4, 4, 4, 4)) 
dat2 <- selectCases("(A=1*B=2 + A=4*B=3 <-> C=1)*(C=4*D=1 + C=2*D=4 <-> E=4)", dat1, 
                    type = "mv")
mvcna(dat2)

# with data fragmentation: only 100 of 472 observable configurations are actually
# observed. [Repeated runs will generate different data frames.]
dat1 <- allCombs(c(4, 4, 4, 4, 4)) 
dat2 <- selectCases("(A=1*B=2 + A=4*B=3 <-> C=1)*(C=4*D=1 + C=2*D=4 <-> E=4)", dat1, 
                    type = "mv")
dat3 <- some(dat2, n= 100, replace = TRUE)
mvcna(dat3)

# 2. fs data with imperfect consistencies (con = 0.8) and coverages (cov = 0.8); about
# 150 cases (depending on the seed). [Repeated runs will generate different data frames.]
dat1 <- allCombs(c(2, 2, 2, 2, 2)) - 1
dat2 <- some(truthTab(dat1), n = 200, replace = TRUE)
dat3 <- makeFuzzy(tt2df(dat2), fuzzvalues = seq(0, 0.45, 0.01))
dat4 <- selectCases1("a*B + c*D + b*d <-> E", con=.8, cov=.8, type = "fs", dat3)
fscna(dat4, ordering = list("E"), strict = TRUE, con=.8, cov=.8)

# data fragmentation: only 80 of about 150 possible cases are actually observed.
# [Repeated runs will generate different data frames.]
dat1 <- allCombs(c(2, 2, 2, 2, 2)) - 1
dat2 <- some(truthTab(dat1), n = 200, replace = TRUE)
dat3 <- makeFuzzy(tt2df(dat2), fuzzvalues = seq(0, 0.45, 0.01))
dat4 <- selectCases1("a*B + c*D + b*d <-> E", con=.8, cov=.8, type = "fs", dat3)
dat5 <- some(dat4, n = 80, replace = TRUE)
fscna(dat5, ordering = list("E"), strict = TRUE, con=.8, cov=.8)
}

# Illustration of only.minimal.msc = FALSE
# ----------------------------------------

# Simulate noisy data on the causal structure "a*B*d + A*c*D <-> E"
set.seed(1324557857)
mydata <- allCombs(rep(2, 5)) - 1
dat <- makeFuzzy(mydata, fuzzvalues = seq(0, 0.5, 0.01))
dat <- tt2df(selectCases1("a*B*d + A*c*D <-> E", con=.8, cov=.8, dat))

# In dat, "a*B*d + A*c*D <-> E" has the following con and cov scores:
as.condTbl(fscond("a*B*d + A*c*D <-> E", dat))

# The standard algorithm of cna will, however, not find this structure with
# con=cov=0.8 because one of the disjuncts (a*B*d) does not meet the con
# threshold:
as.condTbl(fscond(c("a*B*d <-> E", "A*c*D <-> E"), dat))
fscna(dat, ordering=list("E"), strict=TRUE, con=.8, cov=.8)

# With the argument con.msc we can lower the con threshold for msc, but this does not
# recover "a*B*d + A*c*D <-> E" either:
cna2 <- fscna(dat, ordering=list("E"), strict=TRUE, con=.8, cov=.8, con.msc = 0.7)
cna2
msc(cna2)

# The reason is that "a*B -> E" and "c*D -> E" now also meet the con.msc threshold and,
# therefore, neither "a*B*d -> E" nor "A*c*D -> E" are contained in the msc---
# because of violated minimality. In a situation like this, lifting the minimality  
# requirement via only.minimal.msc = FALSE allows cna to find the intended target:
fscna(dat, ordering=list("E"), strict=TRUE, con=.8, cov=.8, con.msc=.7,
      only.minimal.msc = FALSE)
}

