\name{condition}
\alias{condition}
\alias{condition.default}
\alias{condition.condTbl}
\alias{print.cond}
\alias{group.by.outcome}

\title{
Facilities to inspect conditions in a truth table
}

\description{
Facilities to inspect conditions in a truth table.
}

\usage{
condition(x, ...)

\method{condition}{default}(x, tt, ...)
\method{condition}{condTbl}(x, tt, ...)

\method{print}{cond}(x, digits = 3, print.table = TRUE, row.names = FALSE, ...)

group.by.outcome(condlst, cases = TRUE)
}

\arguments{
  \item{x}{
In \code{condition.default} a vector of character strings specifying conditions as \code{"A+B*C -> D"}, 
where \code{"A","B","C","D"} are column names in \code{tt}.\cr
In \code{condition.condTbl}, \code{x} ist a \code{\link{condTbl}}.
}
  \item{tt}{
A \code{\link{truthTab}}. 
}
  \item{digits}{
Number of digits to print in consistencies and coverages.
}
  \item{print.table}{
Logical value specifying whether to print the truth table.
}
  \item{row.names, \dots}{
In \code{print.cond}, these arguments are passed to \code{\link{print.data.frame}}.
}
  \item{condlst}{
A list of objects, each of them of class \dQuote{cond}.
As returned by \code{condition}.
}
  \item{cases}{
A logical Value. If \code{TRUE} the returned data frame has a column \dQuote{cases}.
}}

\details{
In \code{condition}, 
the expressions on both sides of the \dQuote{\code{->}} in the character strings defining conditions are evaluated and written into a data frame with two columns.
Specific attributes are then attached to these data frames: consistency, coverage and an additional class attribute \dQuote{cond}, among others.
This yields one amended data frame for each character string, so that the value returned by \code{cond} is a \emph{list} of such \dQuote{cond}-objects.

\dQuote{\code{<->}} can be used instead of \dQuote{\code{->}} in the condition specifications, and \dQuote{one sided} conditions with no arrow at all are admissable, too.

\code{group.by.outcome} combines the \dQuote{cond}-data frames resulting from conditions with the same outcome (data frames with an identical second column) 
into a common data frame with a larger number of columns.
Note that the additional attributes (consistencies etc.) are thereby removed. 
}

\value{
\code{condition} returns a list of objects, each of them of class \dQuote{cond}. 
An objects of class \dQuote{cond} is a data frame of 0 and 1 with additional attributes.

\code{group.by.outcome} returns a list of data frames.
}


\seealso{
A general overview of the facilities in the \code{cna} package is given in \link{cna-package}.
}
