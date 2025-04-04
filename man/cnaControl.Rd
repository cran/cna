
\name{cnaControl}
\alias{cnaControl}

\title{
Fine-tuning and modifying the CNA algorithm
}
\description{
The \code{cnaControl} function provides a number of arguments for fine-tuning and modifying the CNA algorithm as implemented in the \code{\link{cna}} function. The arguments can also be passed directly to the \code{cna} function. All arguments in \code{cnaControl} have default values that should be left unchanged for most CNA applications. 
}

\usage{
cnaControl(inus.only = TRUE, inus.def = c("implication","equivalence"), 
           type = "auto", con.msc = NULL, 
           rm.const.factors = FALSE, rm.dup.factors = FALSE, 
           cutoff = 0.5, border = "up", asf.selection = c("cs", "fs", "none"), 
           only.minimal.msc = TRUE, only.minimal.asf = TRUE, maxSol = 1e+06)
}

\arguments{
  \item{inus.only}{Logical; if \code{TRUE}, only disjunctive normal forms that are free of redundancies are retained as asf (see also \code{\link{is.inus}}). Defaults to \code{TRUE}.}
  
  \item{inus.def}{Character string specifying the definition of partial structural redundancy to be applied. Possible values are "implication" or "equivalence". The strings can be abbreviated.}
  
  \item{type}{Character vector specifying the type of the data analyzed by \code{cna}: \code{"auto"} (automatic detection; default), \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set).}
  
  \item{con.msc}{Numeric scalar between 0 and 1 to set the minimum threshold every msc must satisfy on the sufficiency measure selected in \code{measures[1]}, e.g. consistency (cf. \code{\link{cna}}). Overrides \code{con} for msc and, thereby, allows for imposing a threshold on msc that differs from the threshold \code{con} imposes on asf and csf. Defaults to \code{con}.}
  
  \item{rm.const.factors, rm.dup.factors}{Logical; if \code{TRUE}, factors with constant values are removed and all but the first of a set of duplicated factors are removed. These parameters are passed to \code{\link{configTable}}. \bold{Note:} The default value has changed from \code{TRUE} to \code{FALSE} in the package's version 3.5.4.}
  
  \item{cutoff}{Minimum membership score required for a factor to count as instantiated in the data and to be integrated into the analysis. Value in the unit interval [0,1]. The default cutoff is 0.5. Only meaningful if the data is fuzzy-set (\code{type = "fs"}).}
  
  \item{border}{Character string specifying whether factors with membership scores equal to \code{cutoff} are rounded up (\code{"up"}) or rounded down (\code{"down"}). Only meaningful if \code{type = "fs"}. }
  
  \item{asf.selection}{Character string specifying how to select asf based on outcome variation in configurations incompatible with a model. \code{asf.selection = "cs"} (default): selection based on variation at the 0.5 anchor; \code{asf.selection = "fs"}: selection based on variation in the fuzzy-set value; \code{asf.selection = "none"}: no selection based on outcome variation in incompatible configurations.}
  
  \item{only.minimal.msc}{Logical; if \code{TRUE} (default), only minimal conjunctions are retained as msc. If \code{FALSE}, sufficient conjunctions are not required to be minimal.}
  
  \item{only.minimal.asf}{Logical; if \code{TRUE} (default), only minimal disjunctions are retained as asf. If \code{FALSE}, necessary disjunctions are not required to be minimal. }
  
  \item{maxSol}{Maximum number of asf calculated. The default value should normally not be changed by the user.}
}

\value{
A list of parameter settings.
}

\details{
When the \bold{\code{inus.only}} argument takes its default value \code{TRUE}, the \code{\link{cna}} function only returns solution formulas---asf and csf---that are freed of all types of redundancies: redundancies in sufficient and necessary conditions as well as structural and partial structural redundancies. Moreover, tautologous and contradictory solutions and solutions featuring constant factors are eliminated (cf. \code{\link{is.inus}}). In other words, at \code{inus.only = TRUE}, \code{cna} issues so-called MINUS-formulas only (cf. \code{vignette("cna")} for details). MINUS-formulas are causally interpretable. In some research contexts, however, solution formulas with redundancies might be of interest, for example, when the analyst is not searching for causal models but for models with maximal data fit. In such cases, the \code{inus.only} argument can be set to its non-default value \code{FALSE}.

The notion of a partial structural redundancy (PSR) can be defined in two different ways, which can be selected through the \bold{\code{inus.def}} argument. If \code{inus.def = "implication"} (default), a solution formula is treated as containing a PSR iff it logically implies a proper submodel of itself. If \code{inus.def = "equivalence"}, a PSR obtains iff the solution formula is logically equivalent with a proper submodel of itself. The character string passed to \code{inus.def} can be abbreviated. To reproduce results produced by versions of the \pkg{cna} package prior to 3.6.0, \code{inus.def} may have to be set to \code{"equivalence"}, which was the default in earlier versions.

The argument \bold{\code{type}} allows for manually specifying the type of data passed to the \code{cna} function. The argument has the default value \code{"auto"}, inducing automatic detection of the data type. But the user can still manually set the data type. Data with factors taking values 1 or 0 only are called \emph{crisp-set}, which can be indicated by \code{type = "cs"}. If the data contain at least one factor that takes more than two values, e.g. \{1,2,3\}, the data count as \emph{multi-value}: \code{type = "mv"}. Data featuring at least one factor taking real values from the interval [0,1] count as \emph{fuzzy-set}: \code{type = "fs"}. (Note that mixing multi-value and fuzzy-set factors in one analysis is not supported). One context in which users may want to set the data type manually is when they are interested in receiving models for both the presence and the absence of a crisp-set outcome from just one call of the \code{cna} function. When analyzing cs data \code{x}, \code{cna(x, ordering = "A", type = "mv")} searches for models of A=1 and A=0 at the same time, whereas the default \code{cna(x, ordering = "A")} searches for models of A=1 only.

The \code{cna} function standardly takes one threshold \code{con} for the selected sufficiency measure, e.g. consistency, that is imposed on both minimally sufficient conditions (msc) and solution formulas, asf and csf. But the analyst may want to impose a different \code{con} threshold on msc than on asf and csf. This can be accomplished by setting the argument \bold{\code{con.msc}} to a different value than \code{con}. In that case, \code{cna} first builds msc using \code{con.msc} and then combines these msc to asf and to csf using \code{con} (and \code{cov}). See Examples below for a concrete context, in which this might be useful.

\bold{\code{rm.const.factors}} and \bold{\code{rm.dup.factors}} are used to determine the handling of constant factors, i.e. factors with constant values in all cases (rows) in the data analyzed by \code{cna}, and of duplicated factors, i.e. factors with identical value distributions in all cases in the data. If the arguments are given the value \code{TRUE}, factors with constant values are removed and all but the first of a set of duplicated factors are removed. As of package version 3.5.4, the default is \code{FALSE} for both \code{rm.const.factors} and \code{rm.dup.factors}, which means that constant and duplicated factors are not removed. See \code{\link{configTable}} for more details.

 \code{cna} only includes factor configurations in the analysis that are actually instantiated in the data. The argument \bold{\code{cutoff}} determines the minimum membership score required for a factor or a combination of factors to count as instantiated. It takes values in the unit interval [0,1] with a default of 0.5. \bold{\code{border}} specifies whether configurations with membership scores equal to \code{cutoff} are rounded up (\code{border = "up"}), which is the default, or rounded down (\code{border = "down"}).

If the data analyzed by \code{cna} feature noise, it can happen that all variation of an outcome occurs in noisy configurations in the data. In such cases, there may be asf that meet chosen \code{con} and \code{cov} thresholds (lower than 1) such that the corresponding outcome only varies in configurations that are incompatible with the strict crisp-set or fuzzy-set necessity and sufficiency relations expressed by those very asf. In the default setting \code{"cs"} of the argument \bold{\code{asf.selection}}, an asf is only returned if the outcome takes a value above and below the 0.5 anchor in the configurations compatible with the strict crisp-set necessity and sufficiency relations expressed by that asf. At \code{asf.selection = "fs"}, an asf is only returned if the outcome takes different values in the configurations compatible with the strict fuzzy-set necessity and sufficiency relations expressed by that asf. At \code{asf.selection = "none"}, asf are returned even if outcome variation only occurs in noisy configurations. (For more details, see Examples below.)


To recover certain target structures from noisy data, it may be useful to allow \code{cna} to also consider sufficient conditions for further analysis that are not minimal (i.e. redundancy-free). This can be accomplished by setting \bold{\code{only.minimal.msc}} to its non-default value \code{FALSE}. A concrete example illustrating the utility of \code{only.minimal.msc = FALSE} is provided in the Examples section below. Similarly, to recover certain target structures from noisy data, \code{cna} may need to also consider necessary conditions for further analysis that are not minimal. This is accomplished by setting \bold{\code{only.minimal.asf}} to \code{FALSE}, in which case \emph{all} disjunctions of msc reaching the \code{con} and \code{cov} thresholds will be returned. (The ordinary user is advised not to change the default values of either argument.)

For details on the usage of \code{cnaControl}, see the example below.


}


\seealso{\code{\link{cna}},  \code{\link{is.inus}}, \code{\link{configTable}}, \code{\link{showConCovMeasures}}}


\examples{
# cnaControl() generates a list that can be passed to the control argument of cna().
cna(d.jobsecurity, outcome = "JSR", con = .85, cov = .85, maxstep = c(3,3,9),
    control = cnaControl(inus.only = FALSE, only.minimal.msc = FALSE, con.msc = .78))
# The fine-tuning arguments can also be passed to cna() directly.
cna(d.jobsecurity, outcome = "JSR", con = .85, cov = .85, maxstep = c(3,3,9),
    inus.only = FALSE, only.minimal.msc = FALSE, con.msc = .78)
# Changing the set-inclusion cutoff and border rounding.
cna(d.jobsecurity, outcome = "JSR", con = .85, cov = .85, 
  	control = cnaControl(cutoff= 0.6, border = "down"))
# Modifying the handling of constant factors.
data <- subset(d.highdim, d.highdim$V4==1)
cna(data, outcome = "V11", con=0.75, cov=0.75, maxstep = c(2,3,9), 
		control = cnaControl(rm.const.factors = TRUE)) 
    
    
# Illustration of only.minimal.msc = FALSE
# ----------------------------------------
# Simulate noisy data on the causal structure "a*B*d + A*c*D <-> E"
set.seed(1324557857)
mydata <- allCombs(rep(2, 5)) - 1
dat1 <- makeFuzzy(mydata, fuzzvalues = seq(0, 0.5, 0.01))
dat1 <- ct2df(selectCases1("a*B*d + A*c*D <-> E", con = .8, cov = .8, dat1))

# In dat1, "a*B*d + A*c*D <-> E" has the following con and cov scores.
as.condTbl(condition("a*B*d + A*c*D <-> E", dat1))

# The standard algorithm of CNA will, however, not find this structure with
# con = cov = 0.8 because one of the disjuncts (a*B*d) does not meet the con
# threshold.
as.condTbl(condition(c("a*B*d <-> E", "A*c*D <-> E"), dat1))
cna(dat1, outcome = "E", con = .8, cov = .8)

# With the argument con.msc we can lower the con threshold for msc, but this does not
# recover "a*B*d + A*c*D <-> E" either.
cna2 <- cna(dat1, outcome = "E", con = .8, cov = .8, con.msc = .78)
cna2
msc(cna2)

# The reason is that "A*c -> E" and "c*D -> E" now also meet the con.msc threshold and,
# therefore,  "A*c*D -> E" is not contained in the msc---because of violated minimality.
# In a situation like this, lifting the minimality requirement via 
# only.minimal.msc = FALSE allows CNA to find the intended target.
cna(dat1, outcome = "E", con = .8, cov = .8, control = cnaControl(con.msc = .78,
		only.minimal.msc = FALSE))


# Overriding automatic detection of the data type
# ------------------------------------------------
# The type argument allows for manually setting the data type.
# If "cs" data are treated as "mv" data, cna() automatically builds models for all values
# of outcome factors, i.e. both positive and negated outcomes.
cna(d.educate, control = cnaControl(type = "mv"))
# Treating "cs" data as "fs".
cna(d.women, type = "fs")

# Not all manual settings are admissible. 
try(cna(d.autonomy, outcome = "AU", con = .8, cov = .8, type = "mv" ))


# Illustration of asf.selection
# -----------------------------
# Consider the following data set:
d1 <- data.frame(X1 = c(1, 0, 1),
				X2 = c(0, 1, 0), 
				Y = c(1, 1, 0))
ct1 <- configTable(d1, frequency = c(10, 10, 1))

# Both of the following asf reach con=0.95 and cov=1.
condition(c("X1+X2<->Y", "x1+x2<->Y"), ct1)

# Up to version 3.4.0 of the cna package, these two asf were inferred from 
# ct1 by cna(). But the outcome Y is constant in ct1, except for a variation in 
# the third row, which is incompatible with X1+X2<->Y and x1+x2<->Y. Subject to  
# both of these models, the third row of ct1 is a noisy configuration. Inferring
# difference-making models that are incapable of accounting for the only difference
# in the outcome in the data is inadequate. (Thanks to Luna De Souter for 
# pointing out this problem.) Hence, as of version 3.5.0, asf whose outcome only
# varies in configurations incompatible with the strict crisp-set necessity 
# or sufficiency relations expressed by those asf are not returned anymore.

cna(ct1, outcome = "Y", con = 0.9)

# The old behavior of cna() can be obtained by setting the argument asf.selection
# to its non-default value "none".

cna(ct1, outcome = "Y", con = 0.9, control = cnaControl(asf.selection = "none"))

# Analysis of fuzzy-set data from Aleman (2009).
cna(d.pacts, con = .9, cov = .85)
cna(d.pacts, con = .9, cov = .85, asf.selection = "none")
# In the default setting, cna() does not return any model for d.pacts because
# the outcome takes a value >0.5 in every single case, meaning it does not change
# between presence and absence. No difference-making model should be inferred from
# such data. 
# The implications of asf.selection can also be traced by
# the verbose argument:

cna(d.pacts, con = .9, cov = .85, verbose = TRUE)


}

