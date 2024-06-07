\name{condList-methods}
\alias{condList-methods}
\alias{summary.condList}
\alias{as.data.frame.condList}
\alias{group.by.outcome}

\title{
Methods for class \dQuote{condList}
}
\description{
The output of the \code{\link[cna]{condition}} function is a nested list of class \dQuote{condList} that contains one or several data frames. The utilities in \code{condList-methods} are suited for rendering or reshaping these objects in different ways.
}
\usage{
\method{summary}{condList}(object, ...)

\method{as.data.frame}{condList}(x, row.names = attr(x, "cases"), optional = TRUE, nobs = TRUE, ...)

group.by.outcome(object, cases = TRUE)
}

\arguments{
  \item{object,x}{An object of class \dQuote{condList}, the output of the \code{\link[cna]{condition}} function.}
  \item{\dots}{Not used.}
  \item{row.names,optional}{As in \code{\link[base]{as.data.frame}}.}
  \item{nobs}{Logical; if \code{TRUE}, the returned data frame has a 
        column named \dQuote{n.obs} indicating how many cases instantiate a given configuration in the data.}
  \item{cases}{Logical; if \code{TRUE}, the returned data frame has a
        column named \dQuote{cases}.}
}

\details{
The \code{summary} method for class \dQuote{condList} prints the output of \code{condition} in a condensed manner. It is identical to \code{print}ing with \code{print.table = FALSE}, see \code{\link[cna]{print.condList}}.

The output of \code{\link[cna]{condition}} is a nested list of class \dQuote{condList} that contains one or several data frames. The method \code{as.data.frame} is a variant of the base method \code{\link[base]{as.data.frame}}. It offers a convenient way of combining the columns of the data frames in a \code{condList} into one regular data frame.
Columns appearing in several tables (typically the modeled outcomes) are included only once in the resulting data frame. The output of \code{as.data.frame} has syntactically invalid column names by default, including operators such as \code{"->"} or \code{"+"}.
Setting \code{optional = FALSE} converts the column names into syntactically valid names (using \code{\link{make.names}}). 

\code{group.by.outcome} takes a \code{condlist} as input and combines the entries in that nested list into a data frame with a larger number of columns, combining all columns concerning the same outcome into the same data frame. The additional attributes (consistency etc.) are thereby removed.
}

\seealso{\code{\link[cna]{condition}}}

\examples{
ana1 <- cna(d.educate)
(csfList <- condition(csf(ana1)$condition, d.educate))
as.data.frame(csfList)
as.data.frame(csfList[1]) # Include the first condition only
as.data.frame(csfList, row.names = NULL)
as.data.frame(csfList, optional = FALSE)
as.data.frame(csfList, nobs = FALSE)

(asfList <- condition(asf(ana1)$condition, d.educate))
as.data.frame(asfList)  
group.by.outcome(asfList)
summary(asfList)

(mscList <- condition(msc(ana1)$condition, d.educate))
as.data.frame(mscList) 
group.by.outcome(mscList)
summary(mscList)
  
}
