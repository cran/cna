\name{condition}
\alias{condition}
\alias{condition.default}
\alias{condition.condTbl}
\alias{print.condList}
\alias{print.cond}

\title{
Uncover relevant properties of msc, asf, and csf in a data frame or \code{configTable}
}

\description{
The \code{condition} function provides assistance to inspect the properties of msc, asf, and csf (as returned by \code{\link{cna}}) in a data frame or \code{configTable}, but also of any other Boolean function. \code{condition} reveals which configurations and cases instantiate a given msc, asf, or csf and lists consistency and coverage scores. 
}

\usage{
condition(x, ...)

\method{condition}{default}(x, ct, type, add.data = FALSE,
          force.bool = FALSE, rm.parentheses = FALSE, ..., tt)
\method{condition}{condTbl}(x, ct, ...)

\method{print}{condList}(x, ...)
\method{print}{cond}(x, digits = 3, print.table = TRUE, 
      show.cases = NULL, add.data = NULL, ...)
}

\arguments{
  \item{x}{Character vector specifying a Boolean expression as \code{"A + B*C -> D"}, where \code{"A"}, \code{"B"}, \code{"C"}, \code{"D"} are factor values appearing in \code{ct}.}
  \item{ct}{Data frame or \code{configTable} (see \code{\link{configTable}}).}
  \item{type}{Character vector specifying the type of \code{x}: \code{"auto"} (automatic detection; default), \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set).}
\item{add.data}{Logical; if \code{TRUE}, \code{ct} is attached to the output. Alternatively, \code{ct} can be requested by the \code{add.data} argument in \code{print.cond}.}
  \item{force.bool}{Logical; if \code{TRUE}, \code{x} is interpreted as a mere Boolean function, not as a causal model.} 
  \item{rm.parentheses}{Logical; if \code{TRUE}, parentheses around \code{x} are removed prior to evaluation.}
  \item{digits}{Number of digits to print in consistency and coverage scores.}
  \item{print.table}{Logical; if \code{TRUE}, the table assigning configurations and cases to conditions is printed.}
  \item{show.cases}{Logical; if \code{TRUE}, the attribute \dQuote{cases} of the \code{configTable} is printed; same default behavior as in \code{\link{print.configTable}}.}
  \item{\dots}{Arguments passed to methods.}
  \item{tt}{Argument \code{tt} is deprecated in \code{condition()}; use \code{ct} instead.}
}

\details{
Depending on the processed data frame or \code{configTable}, the solutions output by \code{\link{cna}} are often ambiguous; that is, it can happen that many solution formulas fit the data equally well. If that happens, the data alone are insufficient to single out one solution. While \code{\link{cna}} simply lists the possible solutions, the \code{condition} function is intended to provide assistance in comparing different minimally sufficient conditions (msc), atomic solution formulas (asf), and complex solution formulas (csf) in order to have a better basis for selecting among them. 

Most importantly, the output of the \code{condition} function highlights in which configurations and cases in the data an msc, asf, and csf is instantiated. Thus, if the user has independent causal knowledge about particular configurations or cases, the information received from \code{condition} may be helpful in selecting the solutions that are consistent with that knowledge. Moreover, the \code{condition} function allows for directly contrasting consistency and coverage scores or frequencies of different conditions contained in returned asf. 

The \code{condition} function is independent of \code{\link{cna}}. That is, any msc, asf, or csf---irrespective of whether they are output by \code{\link{cna}}---can be given as input to \code{condition}. Even Boolean expressions that do not have the syntax of CNA solution formulas can be passed to \code{condition}. %This makes it possible to also assess the properties of Boolean functions that are interesting or of relevance independently of \code{\link{cna}}.

The first required input \code{x} of \code{condition} is a character vector consisting of Boolean formulas composed of factor values that appear in \code{ct}, which is the second required input. \code{ct} can be a \code{configTable} or a data frame. If \code{ct} is a data frame and the \code{type} argument has its default value \code{"auto"}, \code{condition} first determines the data type and then converts the data frame to a \code{configTable}. The data type can also be manually specified by giving the \code{type} argument one of the values \code{"cs"}, \code{"mv"}, or \code{"fs"}.
%
%Data that feature factors taking values 1 or 0 only are called \emph{crisp-set}, in which case the \code{type} argument takes its default value \code{"cs"}. If the data contain at least one factor that takes more than two values, e.g. \{1,2,3\}, the data count as \emph{multi-value}, which is indicated by \code{type = "mv"}. Data featuring at least one factor taking real values from the interval [0,1] count as \emph{fuzzy-set}, which is specified by \code{type = "fs"}. 

Conjunction can be expressed by \dQuote{\code{*}} or \dQuote{\code{&}}, disjunction by \dQuote{\code{+}} or \dQuote{\code{|}}, negation can be expressed by \dQuote{\code{-}} or \dQuote{\code{!}} or, in case of crisp-set or fuzzy-set data, by changing upper case into lower case letters and vice versa, implication by \dQuote{\code{->}}, and equivalence by \dQuote{\code{<->}}. Examples are \itemize{
\item \code{A*b -> C, A+b*c+!(C+D),  A*B*C + -(E*!B), C -> A*B + a*b}
\item \code{(A=2*B=4 + A=3*B=1 <-> C=2)*(C=2*D=3 + C=1*D=4 <-> E=3)}
\item \code{(A=2*B=4*!(A=3*B=1)) | !(C=2|D=4)*(C=2*D=3 + C=1*D=4 <-> E=3)}
}

Three types of conditions are distinguished:
\itemize{
    \item The type \emph{boolean} comprises Boolean expressions that do not have the syntactic form of causal models, meaning the corresponding character strings in the argument \code{x} do not have an \dQuote{\code{->}} or \dQuote{\code{<->}} as main operator. Examples: \code{"A*B + C"} or \code{"-(A*B + -(C+d))"}. The expression is evaluated and written into a data frame with one column. Frequency is attached to this data frame as an attribute. 
    \item The type \emph{atomic} comprises expressions that have the syntactic form of atomic causal models, i.e. asf, meaning the corresponding character strings in the argument \code{x} have an \dQuote{\code{->}} or \dQuote{\code{<->}} as main operator. Examples: \code{"A*B + C -> D"} or \code{"A*B + C <-> D"}. The expressions on both sides of \dQuote{\code{->}} and \dQuote{\code{<->}} are evaluated and written into a data frame with two columns. Consistency and coverage are attached to these data frames as attributes.
    \item The type \emph{complex} represents complex causal models, i.e. csf. Example:\cr \code{"(A*B + a*b <-> C)*(C*d + c*D <-> E)"}. Each component must be a causal model of type \emph{atomic}. These components are evaluated separately and the results stored in a list. Consistency and coverage of the complex expression are then attached to this list.
  }
The types of the character strings in the input \code{x} are automatically discerned and thus do not need to be specified by the user.

If \code{force.bool = TRUE}, expressions with \dQuote{\code{->}} or \dQuote{\code{<->}} are treated as type \emph{boolean}, i.e. only their frequencies are calculated. Enclosing a character string representing a causal model in parentheses has the same effect as specifying \code{force.bool = TRUE}. \code{rm.parentheses = TRUE} removes parentheses around the expression prior to evaluation, and thus has the reverse effect of setting \code{force.bool = TRUE}.

If \code{add.data = TRUE}, \code{ct} is appended to the output such as to facilitate the analysis and evaluation of a model on the case level.

The \code{digits} argument of the \code{print} method determines how many digits of consistency and coverage scores are printed. If \code{print.table = FALSE}, the table assigning conditions to configurations and cases is omitted, i.e. only frequencies or consistency and coverage scores are returned. \code{row.names = TRUE} also lists the row names in \code{ct}. If rows in a \code{ct} are instantiated by many cases, those cases are not printed by default. They can be recovered by \code{show.cases = TRUE}.

}

\section{\code{print} method}{
\code{print.condList} essentially executes \code{print.cond} (the method printing a single condition)
successively for each list element/condition. All arguments in \code{print.condList} are thereby passed to \code{print.cond}, i.e. \code{digits}, \code{print.table}, \code{show.cases}, \code{add.data} can also be specified when printing the complete list of conditions.

The option \dQuote{spaces} controls how the conditions are rendered in certain contexts. The current setting is queried by typing \code{getOption("spaces")}. The option specifies characters that will be printed with a space before and after them. The default is \code{c("<->","->","+")}. A more compact output is obtained with \code{option(spaces = NULL)}. 
}

\value{
\code{condition} returns a nested list of objects, each of them corresponding to one element of the input vector \code{x}. The list has a class attribute \dQuote{condList}, the list elements (i.e., the individual conditions) are of class \dQuote{cond} and have a more specific class label \dQuote{booleanCond}, \dQuote{atomicCond} or \dQuote{complexCond}, relfecting the type of condition. The components of class \dQuote{booleanCond} or \dQuote{atomicCond} are amended data frames, those of class \dQuote{complexCond} are lists of amended data frames.
}


\references{
Emmenegger, Patrick. 2011. \dQuote{Job Security Regulations in Western Democracies: 
A Fuzzy Set Analysis.} \emph{European Journal of Political Research} 50(3):336-64.

Lam, Wai Fung, and Elinor Ostrom. 2010.
\dQuote{Analyzing the Dynamic Complexity of Development Interventions: Lessons
from an Irrigation Experiment in Nepal.}
\emph{Policy Sciences} 43 (2):1-25.

Ragin, Charles. 2008.
\emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. Chicago, IL:
University of Chicago Press.

}

\seealso{
\code{\link{condList-methods}} describes methods and functions processing the output of \code{condition}; see, in particular, the related \code{summary} and \code{as.data.frame} methods.

\code{\link{cna}}, \code{\link{configTable}}, \code{\link{condTbl}}, \code{\link{as.data.frame.condList}}, \code{\link{d.irrigate}}, \code{\link{shortcuts}}}


\examples{
# Crisp-set data from Lam and Ostrom (2010) on the impact of development interventions 
# ------------------------------------------------------------------------------------
# Build the configuration table for d.irrigate.
irrigate.ct <- configTable(d.irrigate)

# Any Boolean functions involving values of the factors "A", "R", "F", "L", "C", "W" in 
# d.irrigate can be tested by condition().
condition("A*r + L*C", irrigate.ct)
condition(c("A*r + !(L*C)", "A*-(L | -F)", "C -> A*R + C*l"), irrigate.ct)
condition(c("A*r + L*C -> W", "!(A*L*R -> W)", "(A*R + C*l <-> F)*(W*a -> F)"),
          irrigate.ct)

# Group expressions with "->" by outcome.
irrigate.con <- condition(c("A*r + L*C -> W", "A*L*R -> W", "A*R + C*l -> F", "W*a -> F"),
                          irrigate.ct)
group.by.outcome(irrigate.con)

# Pass minimally sufficient conditions inferred by cna() to condition().
irrigate.cna1 <- cna(d.irrigate, ordering = "A, R, L < F, C < W", con = .9)
condition(msc(irrigate.cna1)$condition, irrigate.ct)

# Pass atomic solution formulas inferred by cna() to condition().
irrigate.cna1 <- cna(d.irrigate, ordering = "A, R, L < F, C < W", con = .9)
condition(asf(irrigate.cna1)$condition, irrigate.ct)

# Group by outcome.
irrigate.cna1.msc <- condition(msc(irrigate.cna1)$condition, irrigate.ct)
group.by.outcome(irrigate.cna1.msc)

irrigate.cna2 <- cna(d.irrigate, con = .9)
irrigate.cna2a.asf <- condition(asf(irrigate.cna2)$condition, irrigate.ct)
group.by.outcome(irrigate.cna2a.asf)

# Return as regular data frame.
as.data.frame(irrigate.cna2a.asf)

# Add data.
(irrigate.cna2b.asf <- condition(asf(irrigate.cna2)$condition, irrigate.ct, 
                                     add.data = TRUE))

# No spaces before and after "+".
options(spaces = c("<->", "->" ))
irrigate.cna2b.asf

# No spaces at all.
options(spaces = NULL)
irrigate.cna2b.asf

# Restore the default spacing.
options(spaces = c("<->", "->", "+"))

# Print only consistency and coverage scores.
print(irrigate.cna2a.asf, print.table = FALSE)
summary(irrigate.cna2a.asf)

# Print only 2 digits of consistency and coverage scores.
print(irrigate.cna2b.asf, digits = 2)

# Instead of a configuration table as output by configTable(), it is also possible to provide 
# a data frame as second input. 
condition("A*r + L*C", d.irrigate)
condition(c("A*r + L*C", "A*L -> F", "C -> A*R + C*l"), d.irrigate)
condition(c("A*r + L*C -> W", "A*L*R -> W", "A*R + C*l -> F", "W*a -> F"), d.irrigate)
          
          
# Fuzzy-set data from Emmenegger (2011) on the causes of high job security regulations
# ------------------------------------------------------------------------------------
# Compare the CNA solution for outcome JSR to the solution presented by Emmenegger
# S*R*v + S*L*R*P + S*C*R*P + C*L*P*v -> JSR (p. 349), which he generated by fsQCA as
# implemented in the fs/QCA software, version 2.5.
jobsecurity.cna <- cna(d.jobsecurity, outcome = "JSR", con = .97, cov= .77,
                         maxstep = c(4, 4, 15))
compare.sol <- condition(c(asf(jobsecurity.cna)$condition, "S*R*v + S*L*R*P + S*C*R*P + 
                         C*L*P*v -> JSR"), d.jobsecurity)
summary(compare.sol)
print(compare.sol, add.data = d.jobsecurity)
group.by.outcome(compare.sol)

# There exist even more high quality solutions for JSR.
jobsecurity.cna2 <- cna(d.jobsecurity, outcome = "JSR", con = .95, cov= .8,
                          maxstep = c(4, 4, 15))
compare.sol2 <- condition(c(asf(jobsecurity.cna2)$condition, "S*R*v + S*L*R*P + S*C*R*P + 
                         C*L*P*v -> JSR"), d.jobsecurity)
summary(compare.sol2)
group.by.outcome(compare.sol2)


# Simulate multi-value data
# -------------------------
library(dplyr)
# Define the data generating structure.
groundTruth <- "(A=2*B=1 + A=3*B=3 <-> C=1)*(C=1*D=2 + C=2*D=3 <-> E=3)"
# Generate ideal data on groundTruth.
fullData <- allCombs(c(3, 3, 2, 3, 3))
idealData <- ct2df(selectCases(groundTruth, fullData))
# Randomly add 15% inconsistent cases.
inconsistentCases <- setdiff(fullData, idealData)
realData <- rbind(idealData, inconsistentCases[sample(1:nrow(inconsistentCases), 
                                               nrow(idealData)*0.15), ])
# Determine model fit of groundTruth and its submodels. 
condition(groundTruth, realData)
condition("A=2*B=1 + A=3*B=3 <-> C=1", realData)
condition("A=2*B=1 + A=3*B=3 <-> C=1", realData, force.bool = TRUE)
condition("(C=1*D=2 + C=2*D=3 <-> E=3)", realData)
condition("(C=1*D=2 + C=2*D=3 <-> E=3)", realData, rm.parentheses = TRUE)
condition("(C=1*D=2 +!(C=2*D=3 + A=1*B=1) <-> E=3)", realData)
# Manually calculate unique coverages, i.e. the ratio of an outcome's instances
# covered by individual msc alone (for details on unique coverage cf.
# Ragin 2008:63-68).
summary(condition("A=2*B=1 * -(A=3*B=3) <-> C=1", realData)) # unique coverage of A=2*B=1
summary(condition("-(A=2*B=1) * A=3*B=3 <-> C=1", realData)) # unique coverage of A=3*B=3

# Note that expressions must feature factor VALUES contained in the data, they may not 
# contain factor NAMES. The following calls produce errors.
condition("C*D <-> E", realData)
condition("A=2*B=1 + C=23", realData)
# In case of mv expressions, negations of factor values must be written with brackets.
condition("!(A=2)", realData)
# The following produces an error.
condition("!A=2", realData)
}
