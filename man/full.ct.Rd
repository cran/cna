\name{full.ct}
\alias{full.ct}
\alias{full.ct.default}
\alias{full.ct.configTable}
\alias{full.ct.condTbl}
\alias{full.ct.cti}
\alias{allCombs}

\title{
Generate the logically possible value configurations of a given set of factors
}
\description{
The function \code{full.ct} generates a \code{\link{configTable}} with all (or a specified number of) logically possible value configurations of the factors defined in the input \code{x}. 
\code{x} can be a \code{configTable}, a data frame, an integer, a list specifying the factors' value ranges,  a character string expressing a condition featuring all admissible factor values, or a \code{\link{condTbl}}.

The function \code{allCombs} generates a \code{configTable} (of type \code{"mv"}) of all possible value configurations of \code{length(nvals)} factors, the first factor having \code{nvals[1]} values, the second \code{nvals[2]} values etc. The factors are labeled using capital letters. 
}

\usage{
full.ct(x, ...)

\method{full.ct}{default}(x, type = "auto", cond = NULL, nmax = NULL, ...)
\method{full.ct}{configTable}(x, cond = NULL, nmax = NULL, ...)
\method{full.ct}{condTbl}(x, nmax = NULL, ...)
\method{full.ct}{cti}(x, cond = NULL, nmax = NULL, ...)

allCombs(nvals)
}

\arguments{
  \item{x}{A \code{\link{configTable}}, a data frame, a matrix, an integer, a list specifying the factors' value ranges, a character vector featuring all admissible factor values, or a \code{\link{condTbl}} (see the Details and Examples below).}
  
  \item{type}{Character vector specifying the type of \code{x}: \code{"auto"} (automatic detection; default), \code{"cs"} (crisp-set), \code{"mv"} (multi-value),  or \code{"fs"} (fuzzy-set). (Manual specification of the \code{type} only has an effect if \code{x} is a data frame or matrix.)}
  
  \item{cond}{Optional character vector containing conditions in the syntax of msc, asf or csf. If it is not \code{NULL}, only factors appearing in \code{cond} are used.}
  
  \item{nmax}{Maximal number of rows in the  \code{configTable} output by \code{full.ct}. If \code{nmax} is smaller than the total number of logically possible configurations, a random sample of configurations is drawn. The default \code{nmax = NULL} outputs all logically possible configurations.}
  
  \item{\dots}{
  Further arguments passed to methods.
}
  \item{nvals}{An integer vector with values >0. }
}

\details{
\code{full.ct} generates all or \code{nmax} logically possible value configurations of the factors defined in \code{x}, which can either be a character vector or a \code{\link{condTbl}} or an integer or a list or a \code{\link{configTable}} or a data frame or a matrix. \itemize{
\item{If \code{x} is a character vector, it can contain conditions of any of the three types of conditions, \emph{boolean}, \emph{atomic} or \emph{complex} (see \code{\link[cna]{condition}}/\code{\link{condList}}). \code{x} must contain at least one factor. Factor names and admissible values are guessed from the Boolean formulas. If \code{x} contains multi-value factors, only those values are considered admissible that are explicitly contained in \code{x}. Accordingly, in case of multi-value factors, \code{full.ct} should be given the relevant factor definitions by means of a list (see below).
}
\item{If \code{x} is a \code{condTbl} as produced by \code{\link{msc}}, \code{\link{asf}}, \code{\link{csf}}, or \code{\link{condTbl}} and containing a character column \code{"condition"}, the output will be the same as when \code{full.ct} is applied to \code{x$condition}, which is a character vector containing conditions (see the previous bullet point).
}
\item{If \code{x} is an integer, the output is a configuration table of type \code{"cs"} with \code{x} factors. If \code{x <= 26}, the first \code{x} capital letters of the alphabet are used as the names of the factors. If \code{x > 26}, factors are named "X1" to "Xx". 
}
\item{If \code{x} is a list, \code{x} is expected to have named elements each of which provides the factor names with corresponding vectors enumerating their admissible values (i.e. their value ranges). These values must be non-negative integers.
}
\item{If \code{x} is a \code{configTable}, data frame, or matrix, \code{colnames(x)} are interpreted as factor names and the rows as enumerating the admissible values (i.e. as value ranges). If \code{x} is a data frame or a matrix, \code{x} is first converted to a \code{configTable} (the function \code{configTable} is called with \code{type} as specified in \code{full.ct}), and the \code{configTable} method of \code{full.ct} is then applied to the result. The \code{configTable} method uses all factors and factor values occurring in the \code{configTable}. If \code{x} is of type \code{"fs"}, 0 and 1 are taken as the admissible values.
}
}

The computational demand of generating all logically possible configurations increases exponentially with the number of factors in \code{x}. In order to get an output in reasonable time, even when \code{x} features more than about 15 factors, the argument \code{nmax} allows for specifying a maximal number of configurations to be returned (by random sampling).

If not all factors specified in \code{x} are of interest but only those in a given msc, asf, or csf, \code{full.ct} can be correspondingly restricted via the argument \code{cond}. For instance, \code{full.ct(d.educate, cond = "D + L <-> E")} generates the logically possible value configurations of the factors in the set \{D, L, E\}, even though \code{d.educate} contains further factors. The argument \code{cond} is primarily used internally to speed up the execution of various functions in case of high-dimensional data. 

The main area of application of \code{full.ct} is data simulation in the context of inverse search trials benchmarking the output of \code{cna} (see Examples below). While \code{full.ct} generates the relevant space of logically possible configurations of the factors in an analyzed factor set, \code{\link{selectCases}} selects those configurations from this space that are compatible with a given data generating causal structure (i.e. the ground truth), that is, it selects the empirically possible configurations. The ground truth can be randomly generated by the functions in \code{\link{randomConds}}. The function \code{\link{makeFuzzy}} generates fuzzy data from the output of \code{full.ct} or \code{selectCases}. And \code{\link{is.submodel}} can be used to check whether the models output by \code{cna} are true of the ground truth.

The method for class "cti" is for internal use only.

The function \code{allCombs} serves the same purpose as \code{full.ct} but is less general. It expects an integer vector with \code{nvals} values, >0, and then generates a \code{configTable} (of type \code{"mv"}) of all possible value configurations of \code{length(nvals)} factors, the first factor having \code{nvals[1]} values, the second \code{nvals[2]} values etc. The factors are labeled using capital letters.

}

\value{
A \code{\link{configTable}} of type \code{"cs"} or \code{"mv"} with the full enumeration of combinations of the factor values.
}

\seealso{
\code{\link{configTable}}, \code{\link{condition}}, \code{\link{condList}}, \code{\link{selectCases}}, \code{\link{makeFuzzy}}, 
 \code{\link{randomConds}}, \code{\link{is.submodel}}}

\examples{
# x is a character vector.
full.ct("A + B*c")
full.ct("A=1*C=3 + B=2*C=1 + A=3*B=1") 
full.ct(c("A + b*C", "a*D"))
full.ct("!A*-(B + c) + F")
full.ct(c("A=1", "A=2", "B=1", "B=0", "C=13","C=45"))

# x is a condTbl.
ana.pban <- cna(d.pban, ordering = "PB", con = .85, cov = .9, 
                    measures = c("PAcon", "PACcov")) 
full.ct(csf(ana.pban))

# x is a data frame.
full.ct(d.educate)
full.ct(d.jobsecurity)
full.ct(d.pban)

# x is a configTable.
full.ct(configTable(d.jobsecurity))
full.ct(configTable(d.pban), cond = "C=1 + F=0 <-> V=1") 

# x is an integer.
full.ct(6)
# Constrain the number of configurations to 1000.
full.ct(30, nmax = 1000) 

# x is a list.
full.ct(list(A = 0:1, B = 0:1, C = 0:1))  # cs
full.ct(list(A = 1:2, B = 0:1, C = 23:25))  # mv

# Simulating crisp-set data.
groundTruth.1 <- "(A*b + C*d <-> E)*(E*H + I*k <-> F)"
fullData <- ct2df(full.ct(groundTruth.1))
idealData <- ct2df(selectCases(groundTruth.1, fullData))
# Introduce 20% data fragmentation.
fragData <- idealData[-sample(1:nrow(idealData), nrow(idealData)*0.2), ] 
# Add 10% random noise.
incompData <- dplyr::setdiff(fullData, idealData)
(realData <- rbind(incompData[sample(1:nrow(incompData), nrow(fragData)*0.1), ], 
  fragData))

# Simulating multi-value data.
\donttest{groundTruth.2 <- "(JO=3 + TS=1*PE=3 <-> ES=1)*(ES=1*HI=4 + IQ=2*KT=5 <-> FA=1)"
fullData <- ct2df(full.ct(list(JO=1:3, TS=1:2, PE=1:3, ES=1:2, HI=1:4, IQ=1:5, KT=1:5, FA=1:2)))
idealData <- ct2df(selectCases(groundTruth.2, fullData))
# Introduce 20% data fragmentation.
fragData <- idealData[-sample(1:nrow(idealData), nrow(idealData)*0.2), ] 
# Add 10% random noise.
incompData <- dplyr::setdiff(fullData, idealData)
(realData <- rbind(incompData[sample(1:nrow(incompData), nrow(fragData)*0.1), ], 
  fragData))
}

# allCombs
# --------
# Generate all logically possible configurations of 5 dichotomous factors named "A", "B",
# "C", "D", and "E". 
allCombs(c(2, 2, 2, 2, 2)) - 1
# allCombs(c(2, 2, 2, 2, 2)) generates the value space for values 1 and 2, but as it is
# conventional to use values 0 and 1 for Boolean factors, 1 must be subtracted from
# every value output by allCombs(c(2, 2, 2, 2, 2)) to yield a Boolean data frame.

# Generate all logically possible configurations of 5 multi-value factors named "A", "B",
# "C", "D", and "E", such that A can take on 3 values {1,2,3}, B 4 values {1,2,3,4},
# C 3 values etc.
dat0 <- allCombs(c(3, 4, 3, 5, 3))
head(dat0)
nrow(dat0) # = 3*4*3*5*3

# Generate all configurations of 5 dichotomous factors that are compatible with the 
# causal chain (A*b + a*B <-> C)*(C*d + c*D <-> E).
dat1 <- allCombs(c(2, 2, 2, 2, 2)) - 1
(dat2 <- selectCases("(A*b + a*B <-> C)*(C*d + c*D <-> E)", dat1))

# Generate all configurations of 5 multi-value factors that are compatible with the 
# causal chain (A=2*B=1 + A=3*B=3 <-> C=1)*(C=1*D=2 + C=4*D=4 <-> E=3).
dat1 <- allCombs(c(3, 3, 4, 4, 3))
dat2 <- selectCases("(A=2*B=1 + A=3*B=3 <-> C=1)*(C=1*D=2 + C=4*D=4 <-> E=3)", dat1)
nrow(dat1)
nrow(dat2)                    

# Generate all configurations of 5 fuzzy-set factors that are compatible with the 
# causal structure A*b + C*D <-> E, such that con = .8 and cov = .8.
dat1 <- allCombs(c(2, 2, 2, 2, 2)) - 1
dat2 <- makeFuzzy(dat1, fuzzvalues = seq(0, 0.45, 0.01))
(dat3 <- selectCases1("A*b + C*D <-> E", con = .8, cov = .8, dat2))

# Inverse search for the data generating causal structure A*b + a*B + C*D <-> E from
# fuzzy-set data with non-perfect scores on standard consistency and coverage.
set.seed(3)
groundTruth <- "A*b + a*B + C*D <-> E"
dat1 <- allCombs(c(2, 2, 2, 2, 2)) - 1
dat2 <- makeFuzzy(dat1, fuzzvalues = 0:4/10)
dat3 <- selectCases1(groundTruth, con = .8, cov = .8, dat2)
ana1 <- cna(dat3, outcome = "E", con = .8, cov = .8)
any(is.submodel(asf(ana1)$condition, groundTruth))

}
