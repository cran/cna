\name{is.inus}
\alias{is.inus}

\title{
Test disjunctive normal forms for logical redundancies
}
\description{
\code{is.inus} checks for each element of a character vector specifying Boolean disjunctive normal forms (DNFs) whether it amounts to a minimally necessary disjunction of minimally sufficient conditions relative to all logically possible configurations of the factors contained in the DNF.

}

\usage{
is.inus(cond, x = NULL)
}

\arguments{
  \item{cond}{
Character vector specifying Boolean disjunctive normal forms (DNFs). Currently the permissible syntax is restricted to the operators \code{+}, \code{*} and \code{=} (in case of DNFs of type \code{"mv"}), with negation being expressed by lower case letters.
}
  \item{x}{An optional argument providing a \code{truthTab}, a data frame, or a list specifying the factors' value ranges if \code{cond} contains multi-value factors; if \code{x} is not \code{NULL}, \code{is.inus} tests whether \code{cond} is redundancy-free relative to \code{full.tt(x)}, otherwise relative to \code{full.tt(cond)}.
}
}

\details{
According to the regularity theory of causation underlying CNA, a Boolean dependency structure is causally interpretable only if it does not contain any redundant elements. Boolean dependency structures may feature various types of redundancies (Baumgartner and Falk 2018): redundancies in necessary and sufficient conditions or structural redundancies. 
Redundancies may obtain relative to an analyzed set of empirical data, which, typically, are fragmented and do not feature all logically possible configurations, or they may obtain for principled logical reasons, that is, relative to all configurations that are possible according to classical Boolean logic. 
While the function \code{\link{cna}} builds redundancy-free Boolean dependency structures based on empirical data, the function \code{is.inus} tests necessary and sufficient conditions for logical redundancies (\code{\link{redundant}} performs an analogous test for structural redundancies). 



\code{is.inus} takes a character vector \code{cond} specifying Boolean disjunctive normal forms (DNFs) as input and checks whether these DNFs are redundancy-free according to Boolean logic, that is, minimally necessary disjunctions of minimally sufficient conditions. A necessary disjunction is \emph{minimal} if, and only if, no proper sub-disjunction of it is necessary; and a sufficient conjunction is \emph{minimal} if, and only if, no proper sub-conjunction of it is sufficient (Grasshoff and May 2001). In the function's default call with \code{x = NULL}, this minimality test is performed relative to \code{full.tt(cond)}; if \code{x} is not \code{NULL}, the test is performed relative to \code{full.tt(x)}. As \code{full.tt(cond)} and \code{full.tt(x)} coincide in case of binary factors, the argument \code{x} has no effect in the crisp-set and fuzzy-set cases and, hence, does not have to be specified. In case of multi-value factors, however, the argument \code{x} should be specified in order to define the factors' value ranges (see details below).

A \code{cond} with \code{is.inus(cond)==FALSE} can be freed of logical redundancies by means of the \code{\link{minimalize}} function.


}

\value{
Logical vector of the same length as \code{cond}.
}

\references{%Baumgartner, Michael. 2015. \dQuote{Parsimony and causality.} \emph{Quality & Quantity} 49:839-856.

Baumgartner, Michael and Christoph Falk. 2018. \dQuote{Boolean Difference-Making: A Modern Regularity Theory of Causation}. \emph{PhilSci Archive}. url: http://philsciarchive.pitt.edu/id/eprint/14876.

Grasshoff, Gerd and Michael May. 2001. \dQuote{Causal Regularities.} In W Spohn, M Ledwig, M Esfeld (eds.), \emph{Current Issues in Causation}, pp. 85-114. Mentis, Paderborn.


%Mackie, John L. 1974. \emph{The Cement of the Universe: A Study of Causation}. Oxford: Oxford University Press.
}

\seealso{
\code{\link{condition}}, \code{\link{full.tt}}, \code{\link{redundant}}, \code{\link{minimalize}}, \code{\link{cna}}
}


\examples{
# Crisp-set case
# --------------
is.inus(c("A", "A + B", "A + a*B", "A + a", "A*a"))

is.inus("F + f*G")
is.inus("F*G + f*H + G*H")
is.inus("F*G + f*g + H*F + H*G")


# Multi-value case
# ----------------
mvdata <- mvtt(setNames(allCombs(c(2, 3, 2)) -1, c("C", "F", "V")))
is.inus("C=1 + F=2*V=0", mvdata)
is.inus("C=1 + F=2*V=0", list(C=0:1, F=0:2, V=0:1))
# When x is NULL, is.inus is applied to full.tt("C=1 + F=2*V=0"), which has only
# one single row. That row is then interpreted to be the only possible configuration, 
# in which case C=1 + F=2*V=0 is tautologous and, hence, non-minimal.
is.inus("C=1 + F=2*V=0") 
        
is.inus("C=1 + C=0*C=2", mvtt(d.pban))    # contradictory
is.inus("C=0 + C=1 + C=2", mvtt(d.pban))  # tautologous


# Fuzzy-set case 
# --------------
fsdata <- fstt(d.jobsecurity)
conds <- csf(cna(fsdata, con = 0.85, cov = 0.85))$condition
conds <- cna:::lhs(conds)
is.inus(conds, fsdata)
is.inus(c("S + s", "S + s*R", "S*s"), fsdata)

}

