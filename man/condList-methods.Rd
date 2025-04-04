\name{condList-methods}
\alias{condList-methods}
\alias{summary.condList}
\alias{as.data.frame.condList}
\alias{group.by.outcome}

\title{
Methods for class \dQuote{condList}
}
\description{
The output of the \code{\link[cna]{condition}} (aka \code{\link[cna]{condList}}) function is a nested list of class \dQuote{condList} that contains one or several data frames. The utilities in \code{condList-methods} are suited for rendering or reshaping these objects in different ways.
}
\usage{
\method{summary}{condList}(object, n = 6, ...)

\method{as.data.frame}{condList}(x, row.names = attr(x, "cases"), optional = TRUE, nobs = TRUE, ...)

group.by.outcome(object, cases = TRUE)
}

\arguments{
  \item{object,x}{Object of class \dQuote{condList} as output by the \code{\link[cna]{condition}} function.}
  \item{n}{Positive integer: the maximal number of conditions to be printed.}
  \item{\dots}{Not used.}
  \item{row.names,optional}{As in \code{\link[base]{as.data.frame}}.}
  \item{nobs}{Logical; if \code{TRUE}, the returned data frame has a 
        column named \dQuote{n.obs} indicating how many cases instantiate a given configuration in the data.}
  \item{cases}{Logical; if \code{TRUE}, the returned data frame has a
        column named \dQuote{cases}.}
}

\details{
The \code{summary} method for class \dQuote{condList} prints the output of \code{\link{condition}} in a condensed manner. It is identical to \code{print}ing with \code{print.table = FALSE} (but with a different default of argument \code{n}), see \code{\link[cna]{print.condList}}.

The output of \code{condition} is a nested list of class \dQuote{condList} that contains one or several data frames. The method \code{as.data.frame} is a variant of the base method \code{\link[base]{as.data.frame}}. It offers a convenient way of combining the columns of the data frames in a \code{condList} into one regular data frame.
Columns appearing in several tables (typically the modeled outcomes) are included only once in the resulting data frame. The output of \code{as.data.frame} has syntactically invalid column names by default, including operators such as \code{"->"} or \code{"+"}.
Setting \code{optional = FALSE} converts the column names into syntactically valid names (using \code{\link{make.names}}). 

\code{group.by.outcome} takes a \code{condList} as input and combines the entries in that nested list into a data frame with a larger number of columns, combining all columns concerning the same outcome into the same data frame. The additional attributes (measures, info, etc.) are thereby removed.
}

\seealso{\code{\link[cna]{condition}}, \code{\link[cna]{condList}}, \code{\link[base]{as.data.frame}}, \code{\link{make.names}}
}

\examples{
# Analysis of d.irrigate data with standard evaluation measures.
ana1 <- cna(d.irrigate, ordering = "A, R, L < F, C < W", con = .9)
(ana1.csf <- condition(csf(ana1)$condition, d.irrigate))
# Convert condList to data frame.
as.data.frame(ana1.csf)
as.data.frame(ana1.csf[1]) # Include the first condition only
as.data.frame(ana1.csf, row.names = NULL)
as.data.frame(ana1.csf, optional = FALSE)
as.data.frame(ana1.csf, nobs = FALSE)
# Summary.
summary(ana1.csf)
# Analyze atomic solution formulas.
(ana1.asf <- condition(asf(ana1)$condition, d.irrigate))
as.data.frame(ana1.asf)  
summary(ana1.asf)
# Group by outcome.
group.by.outcome(ana1.asf)
# Analyze minimally sufficient conditions.
(ana1.msc <- condition(msc(ana1)$condition, d.irrigate))
as.data.frame(ana1.msc) 
group.by.outcome(ana1.msc)
summary(ana1.msc)
# Print more than 6 conditions.
summary(ana1.msc, n = 10)

# Analysis with different evaluation measures.
ana2 <- cna(d.irrigate, ordering = "A, R, L < F, C < W", con = .9, cov = .9,
            measures = c("PAcon", "PACcov"))
(ana2.csf <- condition(csf(ana2)$condition, d.irrigate))  
print(ana2.csf, add.data = d.irrigate, n=10)
as.data.frame(ana2.csf, nobs = FALSE, row.names = NULL)
summary(ana2.csf, n = 10)
}
