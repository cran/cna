\name{randomConds}
\alias{randomConds}
\alias{randomAsf}
\alias{randomCsf}

\title{Generate random solution formulas}

\description{
Based on a set of factors and a corresponding data type---given as a data frame or \code{truthTab}---, \code{randomAsf} generates a random atomic solution formula (asf) and \code{randomCsf} a random (acyclic) complex solution formula (csf).
}

\usage{
randomAsf(x, outcome = NULL, compl = NULL, how = c("inus", "minimal"))
randomCsf(x, outcome = NULL, n.asf = NULL, compl = NULL)
}

\arguments{
  \item{x}{Data frame or \code{truthTab}; determines the number of factors, their names and their possible values.}
  \item{outcome}{Optional character vector (of length 1 in \code{randomAsf}) specifying the outcome factor(s) in the solution formula; must be a subset of \code{names(x)}.}
  \item{compl}{Integer vector specifying the maximal complexity of the formula (i.e. number of factors in msc; number of msc in asf).}
  \item{how}{Character string, either \code{"inus"} or \code{"minimal"}, specifying whether the generated solution formula is redundancy-free relative to \code{full.tt(x)} or relative to \code{x} (see details).}
  \item{n.asf}{Integer scalar specifying the number of asf in the csf. Is overridden by \code{length(outcome)} if \code{outcome} is not \code{NULL}. Note that \code{n.asf} is limited to \code{ncol(x)-2}.}
}


\details{
%To benchmark the reliability of a method of causal inference it must be tested to what degree the method recovers the true data-generating structure \eqn{\Delta} or proper substructures of \eqn{\Delta} from data of varying quality. As \eqn{\Delta} is not normally known in real-life discovery contexts, the reliability of a method cannot be assessed by applying it to real-life data. Instead, reliability benchmarking is done in so-called \emph{inverse searches}, which reverse the order of causal discovery as normally conducted in scientific practice. An inverse search comprises three steps: (1) a causal structure \eqn{\Delta} is drawn/presupposed (as ground truth), (2) artificial data \code{x} is simulated from \eqn{\Delta}, possibly featuring various deficiencies (e.g. noise, limited diversity, etc.), and (3) \code{x} is processed by the benchmarked method in order to check whether its output meets the tested reliability benchmark (e.g. whether the output is true of or identical to \eqn{\Delta}). 

\code{randomAsf} and \code{randomCsf} can be used to randomly draw data generating structures (ground truths) in inverse search trials benchmarking the output of \code{\link{cna}}. In the regularity theoretic context in which the CNA method is embedded, a causal structure is a redundancy-free Boolean dependency structure. Hence, \code{randomAsf} and \code{randomCsf} both produce redundancy-free Boolean dependency structures. \code{randomAsf} generates structures with one outcome, i.e. atomic solution formulas (asf), \code{randomCsf} generates structures with multiple outcomes, i.e. complex solution formulas (csf), that are free of cyclic substructures. In a nutshell, \code{randomAsf} proceeds by, first, randomly drawing disjunctive normal forms (DNFs) and by, second, eliminating redundancies from these DNFs. \code{randomCsf} essentially consists in repeated executions of \code{randomAsf}.

The only mandatory argument of \code{randomAsf} and \code{randomCsf} is a data frame or a \code{truthTab} \code{x} defining the factors (with their possible values) from which the generated asf and csf shall be drawn. If asf and csf are built from multi-value or fuzzy-set factors, \code{x} must be a \code{truthTab}.

The optional argument \code{outcome} determines which factors in \code{x} shall be treated as outcomes. If \code{outcome} is at its default value \code{NULL}, \code{randomAsf} and \code{randomCsf} randomly draw factor(s) from \code{x} to be treated as outcome(s). 

The argument \code{compl} controls the complexity of the generated asf and csf. More specifically, the \emph{initial} complexity of asf and csf (i.e. the number of factors included in msc and the number of msc included in asf prior to redundancy elimination) is drawn from the vector \code{compl}. As this complexity might be reduced in the subsequent process of redundancy elimination, issued asf or csf will often have lower complexity than specified in \code{compl}. The default value of \code{compl} is determined by the number of columns in \code{x}. Assigning unduly high values to \code{compl} results in an error.


\code{randomAsf} has the additional argument \code{how} with the two possible values \code{"inus"} and \code{"minimal"}. \code{how = "inus"} determines that the generated asf is redundancy-free relative to all logically possible configurations of the factors in \code{x}, i.e. relative to \code{full.tt(x)}, whereas in case of \code{how = "minimal"} redundancy-freeness is imposed only relative to all configurations actually contained in \code{x}, i.e. relative to \code{x} itself. Typically \code{"inus"} should be used; the value \code{"minimal"} is relevant mainly in repeated \code{randomAsf} calls from within \code{randomCsf}.

The argument \code{n.asf} controls the number of asf in the generated csf. Its value is limited to \code{ncol(x)-2} and overridden by \code{length(outcome)}, if \code{outcome} is not \code{NULL}. Analogously to \code{compl}, \code{n.asf} specifies the number of asf prior to redundancy elimination, which, in turn, may further reduce these numbers. That is, \code{n.asf} provides an upper bound for the number of asf in the resulting csf.


%\code{randomAsf} works as follows:
%\enumerate{
%\item If \code{compl} is \code{NULL}, it is set to \code{2:min(ncol(x)-1, 4)}. %Otherwise, all elements in \code{compl} are eliminated that are greater than \code{ncol(x)-1} or smaller than \code{1}.
%\item The initial number of disjuncts (msc) is determined by randomly sampling one element from \code{compl}.
%\item The length of each msc (i.e. the number of factor values that are conjunctively combined) is determined by randomly sampling from \code{compl}.
%\item If \code{outcome} is \code{NULL}, the outcome is chosen by randomly selecting one factor.
%\item The msc are generated by random sampling of factor values. 
%\item The resulting expression is not necessarily redundancy-free. If it is not, it is \code{cna:::rreduce}d with respect to \code{x} (wrt \code{full.tt(x)} if \code{how = "inus"}).
%\item The result is returned .
%}

%\code{randomCsf} essentially consists in repeated executions of \code{randomAsf}. Its steps are:
%\enumerate{
%\item If \code{outcome} is \code{NULL}: 
%\itemize{
%\item Determine the number of asf as an integer drawn at random from \code{2:min(ncol(x)-2, 4)}.
%\item Determine outcomes at random by sampling from all factors.
%}
%\item Iterate over outcomes and repeatedly call \code{randomAsf}:
%\itemize{
%\item The first asf is generated with the exogeneous factors \code{setdiff(names(x), outcome)} as potential effects; 
%\item The second outcome has the potential effects of the first asf plus the first outcome as potential effects; 
%\item etc.
%}
%Hereby, \code{randomAsf} is called with \code{how = "minimal"} and \code{x} equal to the subset of \code{full.tt(x)} having positive scores in all earlier asfs. This procedure guarantees that no redundant factors or disjuncts or asf remain in the formula.
%}
}

\value{The randomly generated formula, a character string.}

\seealso{
\code{\link{is.submodel}}, \code{\link{selectCases}}, \code{\link{full.tt}}, \code{\link{truthTab}}, \code{\link{cna}}.
}

\examples{
# randomAsf
# ---------
# Asf generated from explicitly specified binary factors.
randomAsf(full.tt("H*I*T*R*K"))
randomAsf(full.tt("Johnny*Debby*Aurora*Mars*James*Sonja"))

# Asf generated from a specified number of binary factors.
randomAsf(full.tt(7))

# Asf generated from an existing data frame.
randomAsf(d.educate)

# Specify the outcome.
randomAsf(d.educate, outcome = "G")

# Specify the complexity.
randomAsf(full.tt(7), compl = 2)
randomAsf(full.tt(7), compl = 3:4)

# Redundancy-freeness relative to x instead of full.tt(x).
randomAsf(d.educate, outcome = "G", how = "minimal")

# Asf with multi-value factors (x must be given as a truthTab).
randomAsf(mvtt(allCombs(c(3,4,3,5,3,4))))

# Asf from fuzzy-set data (x must be given as a truthTab).
randomAsf(fstt(d.jobsecurity))
randomAsf(fstt(d.jobsecurity), outcome = "JSR")

# Generate 20 asf.
\donttest{replicate(20, randomAsf(full.tt(7), compl = 2:3))}


# randomCsf
# ---------
# Csf generated from explicitly specified binary factors.
randomCsf(full.tt("H*I*T*R*K*Q*P"))

# Csf generated from a specified number of binary factors.
randomCsf(full.tt(7))

# Specify the outcomes.
randomCsf(d.volatile, outcome = c("RB","SE"))

# Specify the complexity.
randomCsf(d.volatile, outcome = c("RB","SE"), compl = 2)
randomCsf(full.tt(7), compl = 3:4)

# Specify the number of asf.
randomCsf(full.tt(7), n.asf = 3)

# Csf with multi-value factors (x must be given as a truthTab).
randomCsf(mvtt(allCombs(c(3,4,3,5,3,4))))

# Generate 20 csf.
\donttest{replicate(20, randomCsf(full.tt(7), n.asf = 2, compl = 2:3))


# Inverse searches
# ----------------
# === Ideal Data ===
# Draw the data generating structure. (Every run yields different 
# targets and data.)
target <- randomCsf(full.tt(5), n.asf = 2)
target
# Select the cases compatible with the target.
x <- selectCases(target)
# Run CNA without an ordering.
mycna <- cna(x, maxstep = c(4, 4, 12), rm.dup.factors = FALSE)
# Extract the first 100 csf (depending on the seed, there may be
# more than 100 csf).
csfs <- csf(mycna, 100)
# Eliminate possible structural redundancies from the csf.
min.csfs <- minimalizeCsf(csfs$condition, x)$condition
# Check whether the target is completely returned.
any(unlist(lapply(min.csfs, identical.model, target)))

# === Data fragmentation (20% missing observations) ===
# Draw the data generating structure. (Every run yields different 
# targets and data.)
target <- randomCsf(full.tt(7), n.asf = 2)
target
# Generate the complete data.
x <- tt2df(selectCases(target))
# Introduce fragmentation.
x <- x[-sample(1:nrow(x), nrow(x)*0.2), ] 
# Run CNA without an ordering.
mycna <- cna(x, maxstep = c(4, 4, 12), rm.dup.factors = FALSE)
# Extract and minimize the first 100 csf (depending on the seed, there may be
# more than 100 csf).
csfs <- csf(mycna, 100)
min.csfs <- minimalizeCsf(csfs$condition, x)
# Check whether (a submodel of) the target is actually returned.
any(is.submodel(min.csfs$condition, target))

# === Data fragmentation and noise (20% missing observations, noise ratio of 0.05) ===
# Multi-value data.
# Draw the data generating structure. (Every run yields different 
# targets and data.)
fullData <- mvtt(allCombs(c(4,4,4,4,4)))
target <- randomCsf(fullData, n.asf=2, compl = 2:3)
target
# Generate the complete data.
x <- tt2df(selectCases(target, fullData))
# Introduce fragmentation.
x <- x[-sample(1:nrow(x), nrow(x)*0.2), ] 
# Introduce random noise.
x <- rbind(tt2df(fullData[sample(1:nrow(fullData), nrow(x)*0.05), ]), x)  
# Run CNA without an ordering.
mycna <- mvcna(x, con = .75, cov = .75, maxstep = c(3, 3, 12), rm.dup.factors = F)
# Extract and minimize the first 100 csf (depending on the seed, there may be
# more than 100 csf).
csfs <- csf(mycna, 100)
min.csfs <- if(nrow(csfs)>0) {
              as.vector(minimalizeCsf(csfs$condition, mvtt(x))$condition)
            } else {NA} 
# Check whether no causal fallacy (no false positive) is returned.
if(length(min.csfs)==1 && is.na(min.csfs)) {
      TRUE } else {any(is.submodel(min.csfs, target))}

}
}