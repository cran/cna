\name{rreduce}
\alias{rreduce}

\title{Eliminate redundancies from a disjunctive normal form (DNF)}

\description{\code{rreduce} eliminates redundancies from disjunctive normal forms (DNF), i.e. disjunctions of conjunctions of literals. If there are several minimal DNF, \code{rreduce} selects one at random.}

\usage{
rreduce(cond, x = full.ct(cond), niter = 1, full = !missing(x), verbose = FALSE, 
        maxiter = 1000, simplify2constant = TRUE) 
}

\arguments{
  \item{cond}{
A character string specifying a disjunctive normal form; can be either crisp-set or multi-value.
}
  \item{x}{A 
\code{configTable} or \code{data.frame}; can be either crisp-set or multi-value.
}
  \item{niter}{
An integer value \code{>=1} specifying the number of iterated minimization attempts. The minimal DNFs produced in each iteration are collected and returned.
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
Maximal number of iterations. This is a parameter of internal nature, usually not set by the user.
}
}

\details{

\code{rreduce} successively eliminates conjuncts and disjuncts from a DNF \code{cond} as long as the result of \code{condition(cond, x)} remains the same. The only required argument is \code{cond}. If \code{x} is not provided, redundancies are eliminated relative to \code{full.ct(cond)}. If \code{x} is provided and \code{full = TRUE}, redundancies are eliminated relative to \code{full.ct(x)}. If \code{x} is provided and \code{full = FALSE}, redundancies are eliminated relative to \code{x}.

If \code{cond} has more than one redundancy-free form, \code{rreduce} only returns a randomly chosen one in the default setting of \code{niter = 1}. By increasing \code{niter} to a value \code{>1}, \code{cond} is (randomly) minimized \code{niter} times. All resulting redundancy-free forms are collected and returned. This provides some insight about the amount of redundancy-free forms that \code{cond} has.
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
rreduce(cond1, niter = 10)
cond2 <- "A*b + a*B + A*B + a*b"
rreduce(cond2, simplify2constant = FALSE)

# Any Boolean expressions.
cond <- "!(A*B*C)*!(a*b*c)" # or "A + B*!(D + e) <-> C" 
x <- selectCases(cond) 
cond <- getCond(x) # Returns a DNF equivalent to cond, but with many redundancies.
rreduce(cond)      # Repeated execution results in different outputs.
rreduce(cond, verbose = TRUE)      
rreduce(cond, niter = 20) # 20 iterations yield 5 minimal forms.
}
