\name{rreduce}
\alias{rreduce}

\title{Eliminate redundancies from a disjunctive normal form (DNF)}

\description{\code{rreduce} eliminate redundancies from disjunctive normal forms (DNF), i.e. disjunctions of conjunctions of literals. If there are several minimal DNF, \code{rreduce} selects one at random.}

\usage{
rreduce(cond, x = full.ct(cond), full = !missing(x), verbose = FALSE, maxiter = 1000,
        simplify2constant = TRUE) 
}

\arguments{
  \item{cond}{
A character string specifying a disjunctive normal form; can be either crisp-set or multi-value.
}
  \item{x}{A 
\code{truthTab} or \code{data.frame}; can be either crisp-set or multi-value.
}
  \item{full}{
Logical; if \code{TRUE} (the default), redundancies are eliminated relative to \code{full.ct(x)}, otherwise relative to \code{x}.
}
  \item{simplify2constant}{
Logical; if \code{TRUE} (the default), a tautologous or contradictory \code{cond} is reduced to a constant \code{"1"} or \code{"0"}, respectively. If \code{FALSE}, a minimal tautology or contradiction, i.e. \code{"A+a"} or \code{"A*a"}, will result.
}
  \item{verbose}{
Logical; if TRUE, the reduction process will be traced in the console. 
}
  \item{maxiter}{
Maximal number of iterations.  
}
}

\details{

\code{rreduce} successively eliminates conjuncts and disjuncts from a DNF \code{cond} as long as the truth scores in data \code{x} do not change, that is, as long as the result of \code{condition(cond, x)} remains the same. The only required argument is \code{cond}. If \code{x} is not provided, redundancies are eliminated relative to \code{full.ct(cond)}. If \code{x} is provided and \code{full = TRUE}, redundancies are eliminated relative to \code{full.ct(x)}. If \code{x} is provided and \code{full = FALSE}, redundancies are eliminated relative to \code{x}.

If there are more than one redundancy-free forms of \code{cond}, \code{rreduce} only returns a randomly chosen one. 
}

\value{
Redundancy-free disjunctive normal form (DNF).
}

\seealso{
\code{\link{full.ct}}.
}

\examples{
# Logical redundancies.
cond1 <- "A*b + a*B + A*C + B*C"
rreduce(cond1)
cond2 <- "A*b + a*B + A*B + a*b"
rreduce(cond2, simplify2constant = FALSE)

# Any Boolean expressions.
cond <- "!(A*B*C)*!(a*b*c)" # or "A + B*!(D + e) <-> C" 
x <- selectCases(cond) 
cond <- getCond(x) # Returns a DNF equivalent to cond, but with many redundancies.
rreduce(cond)      # Repeated execution results in different outputs
}
