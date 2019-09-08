\name{makeFuzzy}
\alias{makeFuzzy}

\title{
Generate fuzzy-set data by simulating noise
}
\description{
Generates fuzzy-set data by simulating the addition of random noise from the uncontrolled causal background to a data frame featuring binary factors only.
}
\usage{
makeFuzzy(x, fuzzvalues = c(0, 0.05, 0.1), ...)
}

\arguments{
  \item{x}{Data frame or \code{truthTab} featuring binary factors with values 1 and 0 only.}
  \item{fuzzvalues}{Values to be added to the 0's and subtracted from the 1's.}
  \item{\dots}{Additional arguments are passed to \code{\link{truthTab}}.}
}

\details{In combination with \code{allCombs}, \code{full.tt} and \code{selectCases}, \code{makeFuzzy} is useful for simulating noisy data, which are needed for inverse search trials benchmarking the output of \code{cna}. \code{makeFuzzy} transforms a data frame or \code{truthTab} consisting of binary factors into a fuzzy-set \code{truthTab} by adding values selected at random from the argument \code{fuzzvalues} to the 0's and subtracting them from the 1's in the data frame. This transformation simulates the introduction of background noise into the data. \code{selectCases} can subsequently be applied to draw those fuzzy-set configurations from the resulting data that are compatible with a given data generating causal structure.
}

\value{
A \code{truthTab} of type "fs".
}

\seealso{\code{\link{selectCases}}, \code{\link{allCombs}}, \code{\link{full.tt}}, \code{\link{truthTab}}, \code{\link{cna}}, \code{\link{tt2df}}, \code{\link{condition}}
}

\examples{
# Fuzzify a binary 6x3 matrix with default fuzzvalues.
X <- matrix(sample(0:1, 18, replace = TRUE), 6)
makeFuzzy(X)

# ... and with customized fuzzvalues.
makeFuzzy(X, fuzzvalues = 0:5/10)
makeFuzzy(X, fuzzvalues = seq(0, 0.45, 0.01))

# Generate all configurations of 5 fuzzy-set factors that are compatible with the causal
# structure A*b + C*D <-> E, such that con = .8 and cov = .8.
dat1 <- allCombs(c(2, 2, 2, 2, 2)) - 1
dat2 <- makeFuzzy(dat1, fuzzvalues = seq(0, 0.45, 0.01))
(dat3 <- selectCases1("A*b + C*D <-> E", con = .8, cov = .8, dat2))
condition("A*b + C*D <-> E", dat3)

# First, generate all configurations of 5 dichotomous factors that are compatible with
# the causal chain (A*b + a*B <-> C)*(C*d + c*D <-> E) and, second, introduce background
# noise. 
dat1 <- full.tt(5)
dat2 <- selectCases("(A*b + a*B <-> C)*(C*d + c*D <-> E)", dat1)
(dat3 <- makeFuzzy(dat2, fuzzvalues = seq(0, 0.45, 0.01)))
condition("(A*b + a*B <-> C)*(C*d + c*D <-> E)", dat3)

# Inverse search for the data generating causal structure A*b + a*B + C*D <-> E from
# fuzzy-set data with non-perfect consistency and coverage scores.
dat1 <- full.tt(5)
set.seed(55)
dat2 <- makeFuzzy(dat1, fuzzvalues = 0:4/10)
dat3 <- selectCases1("A*b + a*B + C*D <-> E", con = .8, cov = .8, dat2)
fscna(dat3, ordering = list("E"), strict = TRUE, con = .8, cov = .8)
}
