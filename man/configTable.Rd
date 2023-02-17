\name{configTable}
\alias{configTable}
\alias{print.configTable}

\title{Assemble cases with identical configurations in a configuration table}

\description{
The \code{configTable} function assembles cases with identical configurations from a crisp-set, multi-value, or fuzzy-set data frame in a table called a \emph{configuration table}.
}

\usage{
configTable(x, type = c("auto", "cs", "mv", "fs"), frequency = NULL,
            case.cutoff = 0, rm.dup.factors = TRUE, rm.const.factors = TRUE,
           .cases = NULL, verbose = TRUE)

\method{print}{configTable}(x, show.cases = NULL, ...)
}

\arguments{
  \item{x}{Data frame or matrix.}
  \item{type}{Character vector specifying the type of \code{x}: \code{"auto"} (automatic detection; default), \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set).}
  \item{frequency}{Numeric vector of length \code{nrow(x)}. All elements must be
        non-negative.}
  \item{case.cutoff}{Minimum number of occurrences (cases) of a configuration
        in \code{x}. Configurations with fewer than \code{case.cutoff} occurrences (cases)
        are not included in the configuration table.}
  \item{rm.dup.factors}{Logical; if \code{TRUE}, all but the first of a set of factors with identical values in \code{x} are removed.}
  \item{rm.const.factors}{Logical; if \code{TRUE}, factors with constant values in \code{x} are removed.}
  \item{.cases}{Optional character vector of length \code{nrow(x)} to set case labels (row names).}
  \item{verbose}{Logical; if \code{TRUE}, some messages on the configuration table are printed.}
  \item{show.cases}{Logical; if \code{TRUE}, the attribute \dQuote{cases}
        is printed.}
  \item{\dots}{
        In \cr\code{print.configTable}: arguments passed to \code{\link{print.data.frame}}.}
}

\details{
The first input \code{x} of the \code{configTable} function is a data frame. To ensure that no misinterpretations of issued asf and csf can occur, users are advised to use only upper case letters as factor (column) names. Column names may contain numbers, but the first sign in a column name must be a letter. Only ASCII signs should be used for column and row names.

The \code{configTable} function merges multiple rows of \code{x} featuring the same configuration into one row, such that each row of the resulting table, which is called a \emph{configuration table}, corresponds to one determinate configuration of the factors in \code{x}.
The number of occurrences (cases) and an enumeration of the cases are saved as attributes
\dQuote{n} and \dQuote{cases}, respectively. The attribute \dQuote{n} is always printed in the output of \code{configTable}, the attribute \dQuote{cases} is printed if the argument \code{show.cases} is \code{TRUE} in the \code{print} method. 

The argument \code{type} allows for manually specifying the type of data; it defaults to \code{"auto"}, which induces automatic detection of the data type. \code{"cs"} stands for crisp-set data featuring factors that only take values 1 and 0; \code{"mv"} stands for multi-value data with factors that can take any non-negative integers as values; \code{"fs"} stands for fuzzy-set data comprising factors taking real values from the interval [0,1], which are interpreted as membership scores in fuzzy sets. 

Instead of multiply listing identical configurations in \code{x}, the \code{frequency} argument can
be used to indicate the frequency of each configuration in the data frame. \code{frequency} takes a numeric vector of length \code{nrow(x)} as value. For instance, \code{configTable(x, frequency = c(3,4,2,3))} determines that the first configuration in \code{x} is featured in 3 cases, the second in 4, the third in 2, and the fourth in 3 cases.

The \code{case.cutoff} argument is used to determine that configurations are only included in the configuration table if they are instantiated at least as many times in \code{x} as the number assigned to \code{case.cutoff}. Or differently, configurations that are instantiated less than  \code{case.cutoff} are excluded from the configuration table. For instance, \code{configTable(x, case.cutoff = 3)} entails that configurations with less than 3 cases are excluded.

\code{rm.dup.factors} and \code{rm.const.factors} allow for determining whether all but the first of a set of duplicated factors (i.e. factors with identical value distributions in \code{x}) are eliminated and whether constant factors (i.e. factors with constant values in all cases (rows) in \code{x}) are eliminated. From the perspective of configurational causal modeling, factors with constant values in all cases can neither be modeled as causes nor as outcomes; therefore, they can be removed prior to the analysis. Factors with identical value distributions cannot be distinguished configurationally, meaning they are one and the same factor as far as configurational causal modeling is concerned. Therefore, only one factor of a set of duplicated factors is standardly retained by \code{configTable}. 

\code{.cases} can be used to set case labels (row names). It is a character vector of length \code{nrow(x)}. 

The \code{row.names} argument of the \code{print} function determines whether the case labels of \code{x} are printed or not. By default, \code{row.names} is \code{TRUE} unless the (comma-separated) list of the \code{cases} exceeds 20 characters in at least one row.
}


\value{
An object of type \dQuote{configTable}, i.e. a data.frame with additional attributes \dQuote{type}, \dQuote{n} and \dQuote{cases}.
}

\note{For those users of \pkg{cna} that are familiar with Qualitative Comparative Analysis (QCA), it must be emphasized that a \emph{configuration table} is a different type of object than a QCA \emph{truth table}. While a truth table is a list indicating whether a minterm (i.e. a configuration of all exogenous factors) is sufficient for the outcome or not, a configuration table is simply an integrated representation of the input data that lists all configurations in the data exactly once. A configuration table does not express relations of sufficiency.
}


\references{

Greckhamer, Thomas, Vilmos F. Misangyi, Heather Elms, and Rodney Lacey. 2008.
\dQuote{Using Qualitative Comparative Analysis in Strategic Management Research: An Examination of Combinations of Industry, Corporate, and Business-Unit Effects.}
\emph{Organizational Research Methods} 11 (4):695-726.


}

\seealso{\code{\link{cna}}, \code{\link[cna]{condition}}, \code{\link{allCombs}}, \code{\link{d.performance}}, \code{\link{d.pacts}}}

\examples{# Manual input of cs data
# -----------------------
dat1 <- data.frame(
  A = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  B = c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0),
  C = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0),
  D = c(1,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,0,0,0),
  E = c(1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0)
)

# Default return of the configTable function.
configTable(dat1)

# Recovering the cases featuring each configuration by means of the print function.
print(configTable(dat1), show.cases = TRUE)

# The same configuration table as before can be generated by using the frequency argument 
# while listing each configuration only once.
dat1 <- data.frame(
  A = c(1,1,1,1,1,1,0,0,0,0,0),
  B = c(1,1,1,0,0,0,1,1,1,0,0),
  C = c(1,1,1,1,1,1,1,1,1,0,0),
  D = c(1,0,0,1,0,0,1,1,0,1,0),
  E = c(1,1,0,1,1,0,1,0,1,1,0)
)
configTable(dat1, frequency = c(4,3,1,3,4,1,10,1,3,3,3))

# Set (random) case labels.
print(configTable(dat1, .cases = sample(letters, nrow(dat1), replace = FALSE)),
      show.cases = TRUE)

# Configuration tables generated by configTable() can be input into the cna() function.
dat1.ct <- configTable(dat1, frequency = c(4,3,1,3,4,1,4,1,3,3,3))
cna(dat1.ct, con = .85, details = TRUE)

# By means of the case.cutoff argument configurations with less than 2 cases can
# be excluded (which yields perfect consistency and coverage scores for dat1).
dat1.ct <- configTable(dat1, frequency = c(4,3,1,3,4,1,4,1,3,3,3), case.cutoff = 2)
cna(dat1.ct, details = TRUE)



# Simulating multi-value data with biased samples (exponential distribution)
# --------------------------------------------------------------------------
dat1 <- allCombs(c(3,3,3,3,3))
set.seed(32)
m <- nrow(dat1)
wei <- rexp(m)
dat2 <- dat1[sample(nrow(dat1), 100, replace = TRUE, prob = wei),]
configTable(dat2) # 100 cases with 51 configurations instantiated only once.
configTable(dat2, case.cutoff = 2) # removing the single instances.

# Duplicated factors are not eliminated, constant factors are not eliminated.
dat3 <- selectCases("(A=1+A=2+A=3 <-> C=2)*(B=3<->D=3)*(B=2<->D=2)*(A=2 + B=1 <-> E=2)",
                    dat1)
configTable(dat3, rm.dup.factors = FALSE, rm.const.factors = FALSE)

# The same without messages about constant and duplicated factors.
configTable(dat3, rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)



# Large-N data with crisp sets from Greckhamer et al. (2008)
# ----------------------------------------------------------
configTable(d.performance[1:8], frequency = d.performance$frequency)

# Eliminate configurations with less than 5 cases.
configTable(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 5)

# Various large-N CNAs of d.performance with varying case cut-offs.
\donttest{cna(configTable(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 4),
    ordering = "SP", con = .75, cov = .6)
cna(configTable(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 5),
    ordering = "SP", con = .75, cov = .6)
cna(configTable(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 10),
    ordering = "SP", con = .75, cov = .6)
print(cna(configTable(d.performance[1:8], frequency = d.performance$frequency, 
    case.cutoff = 15), ordering = "SP", con = .75, cov = .6, what = "a"), 
    nsolutions = "all")
}
}
