\name{condition}
\alias{condition}
\alias{condition.default}
\alias{condition.condTbl}
\alias{print.cond}
\alias{group.by.outcome}


\title{
Uncover relevant properties of sufficient and necessary conditions in a truth table
}

\description{
Provides assistance to inspect the properties of sufficient and necessary conditions in a truth table, most notably, of minimally sufficient conditions that appear in solution formulas as returned by \code{\link{cna}}. \code{condition} reveals which configurations and cases instantiate a given condition or solution and lists consistency, coverage as well as unique coverage scores. 
}

\usage{
condition(x, ...)

\method{condition}{default}(x, tt, ...)
\method{condition}{condTbl}(x, tt, ...)

\method{print}{cond}(x, digits = 3, print.table = TRUE, row.names = FALSE, ...)

group.by.outcome(condlst, cases = TRUE)
}

\arguments{
  \item{x}{A vector of character strings specifying
        conditions as \code{"A+B*C -> D"}, where \code{"A","B","C","D"} are
        column names in \code{tt}.}
  \item{tt}{A truth table as produced by \code{\link{truthTab}} or a data frame.}
  \item{digits}{Number of digits to print in consistency and coverage scores.}
  \item{print.table}{Logical; specifies whether to print the table assigning configurations and cases to conditions.}
  \item{row.names}{In \code{print.cond}, these arguments are passed to
  \code{\link{print.data.frame}}.}
  \item{condlst}{A list of objects, each of them of class \dQuote{cond} as
        returned by \code{condition}.}
  \item{cases, \dots}{Logical; if \code{TRUE}, the returned data frame has a
        column \dQuote{cases}.}
}

\details{
Depending on the processed data frame or truth table, the solutions output by \code{\link{cna}} are sometimes ambiguous, that is, it can happen that many solution formulas fit the data equally well. In such cases, the data alone are insufficient to single out one solution that corresponds to the underlying causal structure. While \code{\link{cna}} simply lists the possible solutions, the \code{condition} function is intended to provide assistance in comparing different possible conditions or solutions in order to have a better basis for selecting among them. 

Most importantly, the output of the \code{condition} function highlights in which configurations and cases in the data an analyzed condition or atomic solution formula is instantiated. Thus, if the user has independent causal knowledge about particular configurations or cases, the information received from \code{condition} may be helpful in selecting the conditions or solutions that are consistent with that knowledge. Moreover, the \code{condition} function allows for directly contrasting consistency, coverage, and unique coverage scores or frequencies of different conditions contained in atomic solution formulas (for details on unique coverage cf. Ragin 2008:63-68). 

The \code{condition} function is independent of \code{\link{cna}}. That is, any conditions or atomic solution formulas -- irrespective of whether they are output by \code{\link{cna}} --  can be given as input to the \code{condition} function. Even Boolean expressions that do not have the standard syntax of CNA solution formulas can be fed into \code{condition}. This makes it possible to also test conditions or Boolean formulas for consistency, coverage, or frequency that are interesting or of relevance independently of \code{\link{cna}}.

The first required input \code{x} of \code{condition} is a character vector consisting of Boolean formulas exclusively composed of factor names that are column names of the truth table \code{tt} as produced by \code{\link{truthTab}}, which is the second required input. Instead of a truth table, it is also possible to give \code{condition} a data frame as second input, in which case \code{condition} first transforms the data frame into a truth table via \code{\link{truthTab}}.
The presupposed Boolean syntax is as follows: conjunction is expressed by \dQuote{\code{*}}, disjunction by \dQuote{\code{+}}, negation by changing upper case into lower case letters and vice versa, and implication by \dQuote{\code{->}}. Examples are \code{condition("A*b -> C", tt)} or \code{condition(c("A+b*c", "A*B*C", "C -> A*B + a*b"), tt)}.

If the Boolean expression given to \code{condition} has an \dQuote{\code{->}}, the character strings on both sides of \dQuote{\code{->}} are evaluated and written into a data frame with two columns. Consistency, coverage, and unique coverage are attached to these data frames as attributes along with an additional class attribute \dQuote{cond}. If the Boolean expression does not have an \dQuote{\code{->}}, the character string is evaluated and written into a data frame with one column. Frequency is attached to this data frame as attribute. In both cases, an amended data frame for each character string results, such that the value returned by \code{condition} is a list of such \dQuote{cond}-objects.

The \code{digits} argument of the \code{print} function determines how many digits of consistency and coverage scores are printed. If \code{print.table = FALSE}, the table assigning conditions to configurations and cases is omitted, i.e. only frequencies or consistency and coverage scores are returned. \code{row.names = TRUE} also lists the row names in \code{tt}.

\code{group.by.outcome} combines the \dQuote{cond}-data frames resulting from
\dQuote{\code{->}}-expressions with the same outcome (data frames with an identical second column)
into a common data frame with a larger number of columns. The additional
attributes (consistencies etc.) are thereby removed.
}

\value{
\code{condition} returns a list of objects, each of them of class \dQuote{cond}. 
An object of class \dQuote{cond} is a data frame of 0 and 1 with additional
attributes \dQuote{Consistency}, \dQuote{Coverage}, \dQuote{Unique Coverage}, \dQuote{Frequency}.

\code{group.by.outcome} returns a list of data frames.
}



\references{
Lam, Wai Fung, and Elinor Ostrom. 2010.
\dQuote{Analyzing the Dynamic Complexity of Development Interventions: Lessons
from an Irrigation Experiment in Nepal.}
\emph{Policy Sciences} 43 (2):1-25.

Ragin, Charles. 2008.
\emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. Chicago, IL:
University of Chicago Press.


}


\seealso{\code{\link{cna}}, \code{\link{truthTab}}, \code{\link{condTbl}}, \code{\link{d.irrigate}}}


\examples{
\donttest{ 
# Lam and Ostrom (2010) on the impact of development interventions on water adequacy in Nepal
#--------------------------------------------------------------------------------------------
# Load dataset. 
data(d.irrigate)

# Build a truth table for d.irrigate.
irrigate.tt <- truthTab(d.irrigate)

# Any Boolean functions involving the factors "A", "R", "F", "L", "C", "W" in d.irrigate can 
# be tested by condition.
condition("A*r + L*C", irrigate.tt)
condition(c("A*r + L*C", "A*L -> F", "C -> A*R + C*l"), irrigate.tt)
condition(c("A*r + L*C -> W", "A*L*R -> W", "A*R + C*l -> F", "W*a -> F"), irrigate.tt)

# Group expressions with "->" by outcome.
irrigate.con <- condition(c("A*r + L*C -> W", "A*L*R -> W", "A*R + C*l -> F", "W*a -> F"),
       irrigate.tt)
group.by.outcome(irrigate.con)

# Input minimally sufficient conditions inferred by cna into condition.
irrigate.cna1 <- cna(d.irrigate, ordering = list(c("A","R","L"),c("F","C"),"W"), con = 0.9)
condition(msc(irrigate.cna1)$condition, irrigate.tt)

# Input atomic solution formulas inferred by cna into condition.
irrigate.cna1 <- cna(d.irrigate, ordering = list(c("A","R","L"),c("F","C"),"W"), con = 0.9)
condition(asf(irrigate.cna1)$condition, irrigate.tt)

# Group by outcome.
irrigate.cna1.msc <- condition(msc(irrigate.cna1)$condition, irrigate.tt)
group.by.outcome(irrigate.cna1.msc)

irrigate.cna2 <- cna(d.irrigate, con = 0.9)
irrigate.cna2.asf <- condition(asf(irrigate.cna2)$condition, irrigate.tt)
group.by.outcome(irrigate.cna2.asf)

# Print only consistency and coverage scores.
print(irrigate.cna2.asf, print.table = FALSE)

# Print only 2 digits of consistency and coverage scores.
print(irrigate.cna2.asf, digits = 2)

# Instead of a truth table as output by truthTab, it is also possible to provide a data
# frame as second input. 
condition("A*r + L*C", d.irrigate)
condition(c("A*r + L*C", "A*L -> F", "C -> A*R + C*l"), d.irrigate)
condition(c("A*r + L*C -> W", "A*L*R -> W", "A*R + C*l -> F", "W*a -> F"), d.irrigate)




}
}
