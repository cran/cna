\name{cna}
\alias{cna}
\alias{print.cna}


\title{Perform Coincidence Analysis}

\description{
The \code{cna} function performs Coincidence Analysis to identify minimally necessary
disjunctions of minimally sufficient conditions of all outcomes in the data
and, if possible, combines the recovered conditions to common-cause and/or
causal-chain structures.

}

\usage{
cna(x, ordering = NULL, strict = FALSE, con = 1, cov = 1,
      notcols = NULL, maxstep = 5, suff.only = FALSE, what = "mac")

\method{print}{cna}(x, what = x$what, digits = 3, nsolutions = 5,
      row.names = FALSE, show.cases = FALSE, ...)
}

\arguments{
  \item{x}{A data frame or an object of class \dQuote{truthTab} (as output by \code{\link{truthTab}}).}
  \item{ordering}{A list of character vectors reflecting the causal ordering of
        the factors in \code{x}.}
  \item{strict}{Logial; if \code{TRUE}, factors on the same level of the causal
        ordering are \emph{not} potential causes of each other.}
  \item{con}{Minimum consistency of a sufficient condition (values between 0 and 1).}
  \item{cov}{Minimum coverage of a necessary condition (values between 0 and 1).}
  \item{maxstep}{Maximum number of steps in the algorithm for finding atomic
        solution formulas.}
  \item{suff.only}{Logical; if \code{TRUE}, the function only searches for minimally
        sufficient conditions and does not search for atomic and complex solution
        formulas.}
  \item{notcols}{A character vector of factors to be negated in \code{x}. If \code{notcols = "all"}, all factors in \code{x} are negated.}
  \item{what}{A character vector specifying what to print.}
  \item{digits}{Number of digits to print in consistency and coverage scores.}
  \item{nsolutions}{Maximum number of conditions, atomic and complex solutions to print.}
  \item{row.names}{Are passed to \code{\link{print.data.frame}}.}
  \item{show.cases,\dots}{Logical value specifying whether the attribute \dQuote{cases}
        is printed.}
}

\details{
Separately for each potential outcome factor, \code{cna} first searches all
minimally sufficient conditions (msc) that meet the cut-off given by \code{con}.
Then, it disjunctively combines these minimally sufficient conditions to minimally
necessary conditions that meet the cut-off given by \code{cov}. The resulting
expressions are the atomic solution formulas (asf) for every outcome factor. The default value for \code{con} and \code{cov} is 1. 

Atomic solution formulas are built from the bottom up. That is, in a first step, \code{cna}
checks whether the msc are necessary for an outcome, next disjunctions of two msc,
then of three, etc. are tested for necessity. The argument \code{maxstep} defines
the number of such steps to be carried out, i.e. disjunctions of up to \code{maxstep}
msc are examined as potential asf. Differently put, the value of \code{maxstep}
determines the maximal number of disjuncts in the resulting asf. The default \code{maxstep} is 5.

Note that the default consistency and coverage cut-offs of 1 frequently will not yield any atomic solution formulas because real-life data tend to feature noise due to uncontrolled background influences. In such cases, users should gradually lower consistency and coverage cut-offs (e.g. in steps of 0.05) until \code{cna} finds solution formulas -- for the aim of a CNA is to find solutions with the highest possible consistency and coverage scores. Consistency and coverage cut-offs should only be lowered below 0.75 with great caution. If cut-offs of 0.75 do not result in solutions, the corresponding data feature such a high degree of noise that there is a severe risk of causal fallacies.  

If \code{cna} finds asf, it combines them to complex solution formulas (csf). Csf are built by conjunctively concatenating asf with different outcome factors [asf with identical outcome factors are not combined, for they do not represent one complex causal structure but model ambiguities with respect to one outcome]. For instance, the two asf (D + U <-> L) and (G + L <-> E) can be combined to the csf (D + U <-> L) * (G + L <-> E), which represents a causal chain from D + U via L to E.

When prior causal knowledge about an investigated process is available, \code{cna}
can be told not to treat certain factors as potential causes of other factors by
means of the argument \code{ordering}. If specified, that argument defines a causal
ordering for the factors in \code{x}. For example,
\code{ordering = list(c("A",} \code{ "B"), "C")} determines that \code{C} is causally
located \emph{after} \code{A} and \code{B}, meaning that \code{C} is \emph{not}
a potential cause of \code{A} and \code{B}. In consequence, \code{cna} only checks
whether \code{A} and \code{B} can be modeled as causes of \code{C}; the test for a
causal dependency in the other direction is skipped. If the argument \code{ordering}
is not specified or if it is given the \code{NULL} value (which is the argument's default value),
\code{cna} searches for dependencies between all factors in \code{x}.

The argument \code{strict} determines whether the elements of one level in an
ordering can be causally related or not. For example, if
\code{ordering = list(c("A", "B"), "C")} and \code{strict = TRUE}, then \code{A} and \code{B} --
which are on the same level of the ordering -- are excluded to be causally related
and \code{cna} skips corresponding tests. By contrast, if
\code{ordering = list(c("A", "B"), "C")} and \code{strict = FALSE}, then \code{cna}
also searches for dependencies among \code{A} and \code{B}. The default is \code{strict} \code{ = FALSE}.

The argument \code{notcols} is used to calculate asf and csf
for negated factors. If \code{notcols = "all"}, all factors in \code{x} are negated,
i.e. uppercase factor names are turned into lowercase names and vice versa, and
value 1 is turned into value 0 and vice versa. If \code{notcols} is given a character vector 
of factors in \code{x}, only the factors in that vector are negated. For example, \code{notcols = c("A", "B")}
determines that only factors \code{A} and \code{B} are negated. Note that if an \code{ordering}
is specified and if some factors are negated by means of \code{notcols}, they must
likewise appear negated in the \code{ordering}. That is, if \code{notcols = c("A", "B")},
write \code{ordering = list(c("a", "b"), "C")} instead of \code{ordering =} \code{list(c("A", "B"), "C")}.
Likewise, if \code{notcols = "all"}, an ordering must be given in terms of negated factors, e.g.
\code{ordering} \code{= list(c("a", "b"), "c"))}. The default is no negations, i.e. \code{notcols = NULL}.

The argument \code{suff.only} is applicable in cases of very ambiguous solutions. It may happen
that \code{x} can be modeled in terms of so many atomic and complex solution formulas that \code{cna}
does not terminate before the computer's internal memory is exhausted. In such a case, \code{suff.only = TRUE} forces \code{cna} to stop the analysis after the minimization of sufficient conditions, which will normally yield results even in cases of extreme solution ambiguities. In that manner, it is possible to shed at least some light on the dependencies among the factors in \code{x}, in spite of an incomputable solution space.

The argument \code{what} can be specified both for the \code{cna} and the \code{print}
function. It regulates what elements of the output of \code{cna} are printed. If
\code{what} is given the value \dQuote{\code{t}}, the truth table is printed; if
it is given an \dQuote{\code{m}}, the msc are printed; if it is given an \dQuote{\code{a}},
the asf are printed; if it is given a \dQuote{\code{c}}, the csf are printed.
\code{what = "all"} or \code{what = "tmac"} determine that the full output is
printed. The default is \code{what = "mac"}.

The arguments \code{digits}, \code{nsolutions}, and \code{show.cases} only apply to the \code{print} function, which takes an object of class \dQuote{cna} as first input. \code{digits} determines how many digits of consistency and coverage scores
are printed, while \code{nsolutions} fixes the number of conditions and solutions
to print. \code{nsolutions} applies separately to minimally sufficient conditions,
atomic solution formulas, and complex solution formulas. \code{nsolutions = "all"} recovers all minimally sufficient conditions, atomic and complex solution formulas. \code{show.cases} is applicable if the \code{what} argument is given the value \dQuote{\code{t}}. In that case, \code{show.cases = TRUE} yields a truth table featuring a \dQuote{cases} column, which assigns cases to configurations.



}

\value{
\code{cna} returns an object of class \dQuote{cna}. Objects of class \dQuote{cna} are lists with the following components:

\tabular{rl}{
\code{call}: \tab the executed function call\cr
\code{x}:\tab the processed data frame or truth table\cr
\code{ordering}:\tab the implemented ordering\cr
\code{truthTab}: \tab the object of class "truthTab"\cr
\code{solution}: \tab the solution object, which itself is composed of lists exhibiting msc, asf, and csf for\cr\tab all factors in \code{x}\cr
\code{what}:\tab the values given to the \code{what} argument
  }
}

\note{In the example described below (in \emph{Examples}), the two resulting complex solution formulas represent a common cause structure and a causal chain, respectively. The common cause structure is graphically depicted in figure (a) below, the causal chain in figure (b).

\if{html}{\figure{structures3.png}{Causal Structures}}
\if{latex}{\figure{structures3.png}{options: width=15cm}}
}

\references{
Baumgartner, Michael. 2009a. \dQuote{Inferring Causal Complexity.}
\emph{Sociological Methods & Research} 38(1):71-101.

Baumgartner, Michael. 2009b. \dQuote{Uncovering Deterministic Causal Structures:
A Boolean Approach.} \emph{Synthese} 170(1):71-96.

Baumgartner, Michael, and Ruedi Epple. 2014. \dQuote{A Coincidence Analysis of a
Causal Chain: The Swiss Minaret Vote.}
\emph{Sociological Methods & Research} 43(2):280-312.

Krook, Mona Lena. 2010.
\dQuote{Women's Representation in Parliament: A Qualitative Comparative Analysis.}
\emph{Political Studies} 58 (5):886-908.

Lam, Wai Fung, and Elinor Ostrom. 2010.
\dQuote{Analyzing the Dynamic Complexity of Development Interventions: Lessons
from an Irrigation Experiment in Nepal.}
\emph{Policy Sciences} 43 (2):1-25.

Wollebaek, Dag. 2010.
\dQuote{Volatility and Growth in Populations of Rural Associations.}
\emph{Rural Sociology} 75:144-166.
}

\seealso{\code{\link{truthTab}}, \code{\link{condition}}, \code{\link{condTbl}}, \code{\link{d.educate}}, \code{\link{d.irrigate}}, \code{\link{d.volatile}}, \code{\link{d.minaret}}}

\examples{
\donttest{ 
# Artificial data on high levels of education
#--------------------------------------------
# Load dataset.
data(d.educate)

# Exhaustive CNA without constraints on the search space; print complete solution without
# the truth table.
cna(d.educate)

# The two resulting complex solution formulas represent a common cause structure 
# and a causal chain, respectively. The common cause structure is graphically depicted 
# in (Note, figure (a)), the causal chain in (Note, figure (b)).


# Print only complex solution formulas.
cna(d.educate, what = "c")

# Print only atomic solution formulas.
cna(d.educate, what = "a")

# Print only minimally sufficient conditions.
cna(d.educate, what = "m")

# Print only the truth table.
cna(d.educate, what = "t")

# CNA with negations of the factors E and L.
cna(d.educate, notcols = c("E","L"))

# CNA with negations of all factors.
cna(d.educate, notcols = "all")


# Lam and Ostrom (2010) on the impact of development interventions on water adequacy in Nepal
#--------------------------------------------------------------------------------------------
# Load dataset. 
data(d.irrigate)

# CNA with causal ordering that corresponds to the ordering in Lam & Ostrom (2010); coverage 
# cut-off at 0.9 (consistency cut-off at 1).
cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9)

# The previous function call yields a total of 12 complex solution formulas, only
# 5 of which are printed in the default output. 
# Here is how to extract all 12 complex solution formulas.
cna.irrigate <- cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9)
csf(cna.irrigate)

# Extract all atomic solution formulas.
asf(cna.irrigate)

# Extract all minimally sufficient conditions.
msc(cna.irrigate)

# Alterantively, all minimally sufficient conditions, atomic and complex solution formulas
# can be recovered by means of the nsolutions argument of the print function.
print(cna.irrigate, nsolutions = "all")

# Print the truth table with the "cases" column.
print(cna.irrigate, what = "t", show.cases = TRUE)

# Only build solution formulas with maximally 3 disjuncts.
cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9, maxstep = 3)

# Only print 2 digits of consistency and coverage scores.
print(cna.irrigate, digits = 2)

# Build all but print only two minimally sufficient conditions for each factor and two 
# solution formulas.
print(cna(d.irrigate, ordering = list(c("A","R","F","L","C"),"W"), cov = 0.9), nsolutions = 2)

# CNA with a different ordering such that the factors on one level of the ordering are causally
# unrelated; consistency cut-off at 0.9 (coverage cut-off at 1); print only complex solution
# formulas.
cna(d.irrigate, ordering = list(c("A","R","L"),c("F","C"),"W"), strict = TRUE, con = 0.9, 
       what = "c")

# Same ordering with negation of factor C, consistency cut-off at 0.8, coverage cut-off at 0.9;
# print only complex solution formulas.
cna(d.irrigate, ordering = list(c("A","R","L"),c("F","c"),"W"), notcols = c("C"), con = 0.8,
       cov = 0.9, what = "c")

# Same ordering with negations of all factors, consistency cut-off at 0.75, coverage cut-off 
# at 0.75.
cna(d.irrigate, ordering = list(c("a","r","l"),c("f","c"),"w"), notcols = "all", con = 0.75,
       cov = 0.75)



# Wollebaek (2010) on very high volatility of grassroots associations in Norway
#------------------------------------------------------------------------------
# Load dataset. 
data(d.volatile)

# CNA with ordering from Wollebaek (2010) [Beware: due to massive ambiguities, this analysis
# will not terminate in reasonable time on most computers! In that case, R has to be 
# interrupted manually, e.g. by ESC, Ctr+C, or Alt+M+Enter.]
cna(d.volatile, ordering = list(c("PG","RB","EL","SE","CS","OD","PC","UP"),"VO2"))

# Using suff.only, CNA can be forced to abandon the analysis after minimization of sufficient 
# conditions. [This analysis terminates reasonably quickly.]
cna(d.volatile, ordering = list(c("PG","RB","EL","SE","CS","OD","PC","UP"),"VO2"), 
       suff.only = TRUE)

# Similarly, using maxstep, CNA can be forced to only search for atomic and complex solutions
# with a maximal number of disjuncts. [This analysis also terminates reasonably quickly, 
# yielding a total of 4264 complex solution formulas.]
cna(d.volatile, ordering = list(c("PG","RB","EL","SE","CS","OD","PC","UP"),"VO2"), maxstep = 3)


# Krook (2010) on representation of women in western-democratic parliaments
#--------------------------------------------------------------------------
# Load dataset. 
data(d.women)

# This example shows that CNA can infer which factors are causes and which ones
# are effects from the data. Without being told which factor is the outcome, 
# CNA reproduces the original QCA of Krook (2010).
cna(d.women)



# Baumgartner and Epple (2014) on the Swiss Minaret Initiative
#-------------------------------------------------------------
# load dataset
data(d.minaret)

# Set up the data frame for calibrated factors
smi <- data.frame(
 matrix(numeric(nrow(d.minaret) * (ncol(d.minaret))), ncol = ncol(d.minaret),
  dimnames = list(row.names(d.minaret), c(toupper(names(d.minaret))))
 )
)

# Calibration
smi$A  <- ifelse(d.minaret$a >= 28.0, 1, 0)
smi$L  <- ifelse(d.minaret$l >= 31.9, 1, 0)
smi$S  <- ifelse(d.minaret$s >= 14.5, 1, 0)
smi$T  <- ifelse(d.minaret$t >=  8.0, 1, 0)
smi$X  <- ifelse(d.minaret$x >= 38.2, 1, 0)
smi$M  <- ifelse(d.minaret$m >= 50.0, 1, 0)

# Replicate the results of Baumgartner and Epple (2014:290-96).
cna(smi, ordering = list(c("A","T"), c("L","S"),"X","M"), cov = 0.94)



# User defined data input
#------------------------
# Data input via data.frame()
dat1 <- data.frame(
A = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
B = c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0),
C = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0),
D = c(1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0),
E = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0),
G = c(1,1,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,0,0,0,0),
H = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0)
)

# CNA of dat1
cna(dat1)

# Same input via the frequency argument of the truthTab function.
dat1 <- data.frame(
A = c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
B = c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0),
C = c(1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0),
D = c(1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0),
E = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0),
G = c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
H = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)
)
dat1_tt <- truthTab(dat1, frequency = c(3,3,3,1,3,1,3,1,3,1,3,1,3,1,1,4))
cna(dat1_tt)


# Data input via matrix() is also possible
# dat2 <-
#  matrix(scan(what = integer(0)), ncol = 5, byrow = TRUE)
#  1 1 1 1 1
#  1 1 1 0 1
#  1 0 1 1 1
#  1 0 1 0 1
#  0 1 1 1 1
#  0 1 1 0 1
#  0 0 0 1 1
#  0 0 0 0 0
#  0 0 0 1 1
#  0 0 0 1 1
#  1 1 1 0 1
#  1 1 1 0 1
#  1 0 1 0 1
#  1 1 1 1 1
#
#cna(dat2)




}
}

