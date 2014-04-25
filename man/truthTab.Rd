\name{truthTab}
\alias{truthTab}
\alias{print.truthTab}

\title{
Generate a truth table
}
\description{
\code{truthTab} builds a truth table from an input table, which essentially means that duplicated rows are removed and the number of occurrences of each unique row is stored.
}

\usage{
truthTab(x, frequency = NULL, switch = FALSE, case.cutoff = 0)

\method{print}{truthTab}(x, row.names = FALSE, show.cases = TRUE, ...)
}

\arguments{
  \item{x}{
The input table. Must be a data frame or matrix with 0 and 1 as entries or something that is coerced to 0 and 1 by \code{as.integer},
i.e. logicals are also possible. NA's are not allowed.
}
  \item{frequency}{
Numeric vector of length \code{nrow(x)}. All elements must be nonegative, NA's are not allowed.
}
  \item{switch}{
Vector (integer, logical or character) specifying the columns in \code{x} to be \emph{switched}, i.e. 0 is replaced by 1 are vice versa.
}
  \item{case.cutoff}{
Minimum number of occurrences of a combination of the factors in \code{x}. Combinations with fewer than \code{case.cutoff} occurrences are removed.
}
  \item{show.cases}{
Logical value specifying whether the attribute \dQuote{cases} is printed.
}
  \item{row.names, \dots}{
Are passed to \code{\link{print.data.frame}}.
}}

\details{
The input table is reduced by removing duplicated rows, so that each row of the resulting truth table corresponds to a combination of the factors in \code{x}. 
The number of occurrences and enumeration of the cases (taken from \code{rownames(x)}) are saved in attributes \dQuote{no.of.cases} and \dQuote{cases}, respectively.
}

\value{
\code{truthTab} returns a data frame with additional class \dQuote{truthTab} and attributes \dQuote{no.of.cases} and \dQuote{cases}.
}

\seealso{
A general overview of the facilities in the \code{cna} package is given in \link{cna-package}.
}

