\name{truthTab}

\alias{truthTab}
\alias{cstt}
\alias{mvtt}
\alias{fstt}
\alias{print.truthTab}

\title{Assemble cases with identical configurations in a truth table}

\description{
The \code{truthTab} function assembles cases with identical configurations from a crisp-set (cs), multi-value (mv), or fuzzy-set (fs) data frame in a table called a \emph{truth table} (which is a very different type of object for CNA than for the related method of QCA).
}

\usage{
truthTab(x, type = c("cs", "mv", "fs"), frequency = NULL,
         case.cutoff = 0, rm.dup.factors = TRUE, rm.const.factors = TRUE,
         .cases = NULL, verbose = TRUE)
cstt(...)
mvtt(...)
fstt(...)

\method{print}{truthTab}(x, show.cases = NULL, ...)
}

\arguments{
  \item{x}{Data frame or matrix.}
  \item{type}{Character vector specifying the type of \code{x}: \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set).}
  \item{frequency}{Numeric vector of length \code{nrow(x)}. All elements must be
        non-negative.}
  \item{case.cutoff}{Minimum number of occurrences (cases) of a configuration
        in \code{x}. Configurations with fewer than \code{case.cutoff} occurrences (cases)
        are not included in the truth table.}
  \item{rm.dup.factors}{Logical; if \code{TRUE}, all but the first of a set of factors with identical values in \code{x} are eliminated.}
  \item{rm.const.factors}{Logical; if \code{TRUE}, factors with constant values in \code{x} are eliminated.}
  \item{.cases}{Set case labels (row names): optional character vector of length \code{nrow(x)}.}
  \item{verbose}{Logical; if \code{TRUE}, some messages on the truth table are printed.}
  \item{show.cases}{Logical; if \code{TRUE}, the attribute \dQuote{cases}
        is printed.}
  \item{\dots}{
        In \code{cstt}, \code{mvtt}, \code{fstt}: any formal argument of \code{truthTab} except \code{type}.
        In \cr\code{print.truthTab}: arguments passed to \code{\link{print.data.frame}}.}
}

\details{
The first input \code{x} of the \code{truthTab} function is a data frame. To ensure that no misinterpretations of issued asf and csf can occur, users are advised to use only upper case letters as factor (column) names. Column names may contain numbers, but the first sign in a column name must be a letter. Only ASCII signs should be used for column and row names.

The \code{truthTab} function merges multiple rows of \code{x} featuring the same configuration into one row, such that each row of the resulting table, which is called a \emph{truth table}, corresponds to one determinate configuration of the factors in \code{x}.
The number of occurrences (cases) and an enumeration of the cases are saved as attributes
\dQuote{n} and \dQuote{cases}, respectively. The attribute \dQuote{n} is always printed in the output of \code{truthTab}, the attribute \dQuote{cases} is printed if the argument \code{show.cases} is \code{TRUE} in the \code{print} method. 

The argument \code{type} specifies the type of data. \code{"cs"} stands for crisp-set data featuring factors that only take values 1 and 0; \code{"mv"} stands for multi-value data with factors that can take any non-negative integers as values; \code{"fs"} stands for fuzzy-set data comprising factors taking real values from the interval [0,1], which are interpreted as membership scores in fuzzy sets. To abbreviate the specification of the data type using the \code{type} argument, the functions \code{cstt(x, ...)}, \code{mvtt(x, ...)}, and \code{fstt(x, ...)} are available as shorthands for \code{truthTab(x, type = "cs", ...)}, \cr\code{truthTab(x, type = "mv", ...)}, and \code{truthTab(x, type = "fs", ...)}, respectively.

Instead of multiply listing identical configurations in \code{x}, the \code{frequency} argument can
be used to indicate the frequency of each configuration in the data frame. \code{frequency} takes a numeric vector of length \code{nrow(x)} as value. For instance, \code{truthTab(x, frequency = c(3,4,2,3))} determines that the first configuration in \code{x} is featured in 3 cases, the second in 4, the third in 2, and the fourth in 3 cases.

The \code{case.cutoff} argument is used to determine that configurations are only included in the truth table if they are instantiated at least as many times in \code{x} as the number assigned to \code{case.cutoff}. Or differently, configurations that are instantiated less than the number given to \code{case.cutoff} are excluded from the truth table. For instance, \code{truthTab(x, case.cutoff = 3)} entails that configurations with less than 3 cases are excluded.

\code{rm.dup.factors} and \code{rm.const.factors} allow for determining whether all but the first of a set of duplicated factors (i.e. factors with identical value distributions in \code{x}) are eliminated and whether constant factors (i.e. factors with constant values in all cases (rows) in \code{x}) are eliminated. From the perspective of configurational causal modeling, factors with constant values in all cases can neither be modeled as causes nor as outcomes; therefore, they can be removed prior to the analysis. Factors with identical value distributions cannot be distinguished configurationally, meaning they are one and the same factor as far as configurational causal modeling is concerned. Therefore, only one factor of a set of duplicated factors is standardly retained by \code{truthTab}. 

\code{.cases} can be used to set case labels (row names). It is a character vector of length \code{nrow(x)}. 

The \code{row.names} argument of the \code{print} function determines whether the case labels of \code{x} are printed or not. By default, \code{row.names} is \code{TRUE} unless the (comma-separated) list of the \code{cases} exceeds 20 characters in one row at least.
}


\value{
An object of type \dQuote{truthTab}, i.e. a data.frame with additional attributes \dQuote{type}, \dQuote{n} and \dQuote{cases}.
}

\note{For those users of \pkg{cna} that are familiar with Qualitative Comparative Analysis (QCA), it must be emphasized that a \emph{truth table} is a very different type of object in the context of CNA than it is in the context of QCA. While a QCA truth table is a list indicating whether a minterm (i.e. a configuration of all exogenous factors) is sufficient for the outcome or not, a CNA truth table is simply an integrated representation of the input data that lists all configurations in the data exactly once. A CNA truth table does not express any relations of sufficiency whatsoever.
}


\references{
Aleman, Jose. 2009.
\dQuote{The Politics of Tripartite Cooperation in New Democracies: A Multi-level Analysis.}
\emph{International Political Science Review} 30 (2):141-162.

Greckhamer, Thomas, Vilmos F. Misangyi, Heather Elms, and Rodney Lacey. 2008.
\dQuote{Using Qualitative Comparative Analysis in Strategic Management Research: An Examination of Combinations of Industry, Corporate, and Business-Unit Effects.}
\emph{Organizational Research Methods} 11 (4):695-726.

Thiem, Alrik. 2018. \dQuote{QCApro: Advanced Functionality for Performing and Evaluating Qualitative Comparative Analysis.} \emph{R Package Version 1.1-2.} URL: http://www.alrik-thiem.net/software/.
}

\seealso{\code{\link{cna}}, \code{\link{condition}}, \code{\link{allCombs}}, \code{\link{d.performance}}, \code{\link{d.pacts}}}

\examples{
# Manual input of cs data
# -----------------------
dat1 <- data.frame(
  A = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  B = c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0),
  C = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0),
  D = c(1,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,0,0,0),
  E = c(1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0)
)

# Default return of the truthTab function.
truthTab(dat1)

# Recovering the cases featuring each configuration by means of the print function.
print(truthTab(dat1), show.cases = TRUE)

# The same truth table as before can be generated by using the frequency argument while
# listing each configuration only once.
dat1 <- data.frame(
  A = c(1,1,1,1,1,1,0,0,0,0,0),
  B = c(1,1,1,0,0,0,1,1,1,0,0),
  C = c(1,1,1,1,1,1,1,1,1,0,0),
  D = c(1,0,0,1,0,0,1,1,0,1,0),
  E = c(1,1,0,1,1,0,1,0,1,1,0)
)
truthTab(dat1, frequency = c(4,3,1,3,4,1,10,1,3,3,3))

# Set (random) case labels.
print(truthTab(dat1, .cases = sample(letters, nrow(dat1), replace = FALSE)),
      show.cases = TRUE)

# Truth tables generated by truthTab can be input into the cna function.
dat1.tt <- truthTab(dat1, frequency = c(4,3,1,3,4,1,4,1,3,3,3))
\donttest{cna(dat1.tt, con = .85, details = TRUE)}

# By means of the case.cutoff argument configurations with less than 2 cases can
# be excluded (which yields perfect consistency and coverage scores for dat1).
dat1.tt <- truthTab(dat1, frequency = c(4,3,1,3,4,1,4,1,3,3,3), case.cutoff = 2)
\donttest{cna(dat1.tt, details = TRUE)}


# Simulating multi-value data with biased samples (exponential distribution)
# --------------------------------------------------------------------------
dat1 <- allCombs(c(3,3,3,3,3))
set.seed(32)
m <- nrow(dat1)
wei <- rexp(m)
dat2 <- dat1[sample(nrow(dat1), 100, replace = TRUE, prob = wei),]
truthTab(dat2, type = "mv") # 100 cases with 46 configurations instantiated only once.
mvtt(dat2, case.cutoff = 2) # removing the single instances.

# Duplicated factors are not eliminated, constant factors are not eliminated.
dat3 <- selectCases("(A=1+A=2+A=3 <-> C=2)*(B=3<->D=3)*(B=2<->D=2)*(A=2 + B=1 <-> E=2)",
                    dat1, type = "mv")
\donttest{mvtt(dat3, rm.dup.factors = FALSE, rm.const.factors = FALSE)}


# truthTab with fuzzy-set data from Aleman (2009)
# -----------------------------------------------
# Include all cases.
tt.pacts <- fstt(d.pacts) 
\donttest{fscna(tt.pacts, con = .93, cov = .86, details = TRUE)}

# Only include configurations with at least 3 cases.
tt.pacts2 <- fstt(d.pacts, case.cutoff = 3) 
\donttest{fscna(tt.pacts2, con = .93, cov = .86, details = TRUE)}


# Large-N data with crisp sets from Greckhamer et al. (2008)
#-----------------------------------------------------------
truthTab(d.performance[1:8], frequency = d.performance$frequency)

# Eliminate configurations with less than 5 cases.
truthTab(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 5)

# Various large-N CNAs of d.performance with varying case cut-offs.
\donttest{cna(truthTab(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 4),
    ordering = list("SP"), con = .75, cov = .6)
cna(truthTab(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 5),
    ordering = list("SP"), con = .75, cov = .6)
cna(truthTab(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 10),
    ordering = list("SP"), con = .75, cov = .6)
print(cna(truthTab(d.performance[1:8], frequency = d.performance$frequency, case.cutoff = 15),
          ordering = list("SP"), con = .75, cov = .6, what = "a"), nsolutions = "all")
}
}