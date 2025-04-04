\name{condTbl}
\alias{condTbl}
\alias{print.condTbl}
\alias{as.data.frame.condTbl}
\alias{as.condTbl}
\alias{condTbl}

\title{
Create summary tables for conditions
}

\description{
The function \code{condTbl} returns a table of class \dQuote{condTbl}, which is a \code{data.frame} summarizing selected features of specified conditions (boolean, atomic, complex), e.g. scores on evaluation measures such as consistency and coverage. In contrast to a \code{\link{condList}}, a \code{condTbl} only shows summary measures and does not provide any information at the level of individual cases in the data. 

The objects output by the functions \code{\link{msc}}, \code{\link{asf}}, and \code{\link{csf}} are such tables, as well as those returned by \code{\link{detailMeasures}}.

\code{as.condTbl} reshapes a \code{condList} as output by \code{\link[cna]{condition}} and \code{\link[cna]{condList}} to a \code{condTbl}.

\code{condTbl(x, ...)} executes \code{\link[cna]{condList}(x, ...)} and then turns its output into a \code{condTbl} by applying \code{as.condTbl}.
}

\usage{
as.condTbl(x, ...)
condTbl(x, ...)

\method{print}{condTbl}(x, n = 20, digits = 3, quote = FALSE, row.names = TRUE, 
      printMeasures = TRUE, ...)
\method{as.data.frame}{condTbl}(x, ...)
}

\arguments{
  \item{x}{In \code{as.condTbl}, \code{x} is a list of evaluated conditions, i.e. an object of class \if{html}{\dQuote{\code{condList}}}\if{latex}{\dQuote{\code{cond}-\code{List}}}, as returned by \code{condition}. 
  In \code{condTbl(x, ...)}, \code{x} and \code{...} are the same as in \code{\link{condList}(x, ...)} or \code{\link{condition}(x, ...)}. \cr
  In the dedicated methods of \code{print} and \code{as.data.frame}, \code{x} is a \code{condTbl}.
  }

  \item{n}{Maximal number of rows of the \code{condTbl} to be printed.}
  
  \item{digits}{Number of digits to print in evaluation measures and solution attributes (cf. \code{\link{detailMeasures}}).}
  
  \item{quote, row.names}{As in \code{\link{print.data.frame}}.}
  
  \item{printMeasures}{Logical; if \code{TRUE}, the output indicates which \code{measures} for sufficiency and necessity evaluation were used (provided the evaluated conditions are not boolean).}
  
  \item{\dots}{All arguments in \code{condTbl(x, ...)} are passed on to \code{\link[cna]{condList}}.}
}

\details{

The function \code{as.condTbl} takes an object of class \dQuote{condList} returned by the \code{\link{condition}} function as input and reshapes it in such a way as to make it identical to the output returned by \code{\link{msc}}, \code{\link{asf}}, and \code{\link{csf}}.

The function \code{condTbl} is identical with \code{as.condTbl(condition(...))} and \if{html}{}\if{latex}{\cr}\code{as.condTbl(condList(...))}, respectively.
It thus takes any set of arguments that are valid in \code{condition} and \code{condList} and transforms the result into an object of class \dQuote{condTbl}. 

The argument \code{digits} applies to the \code{print} method. It determines how many digits of the evaluation measures and solution attributes (e.g. standard consistency and coverage, exhaustiveness, faithfulness, or coherence) are printed. The default value is 3.

}

\value{
\code{The functions as.condTbl} and \code{condTbl} return an object of class \dQuote{condTbl}, a concise summary table featuring a set of conditions (boolean, atomic, complex), their outcomes (if the condition is an atomic or complex solution formula), and their scores on given summary measures (e.g. consistency and coverage).
Technically, an object of class \dQuote{\code{condTbl}} is a \code{\link{data.frame}} with an additional class attribute \code{"condTbl"}. It prints slightly differently by default than a \code{data.frame} with respect to column alignment and number of digits.

The section \dQuote{Value} in \code{\link{cna-solutions}} has an enumeration of the columns that are most commonly present in a \code{condTbl}.
}



%\references{
%Baumgartner, Michael and Christoph Falk. 2023. \dQuote{Boolean Difference-Making: A Modern Regularity Theory of Causation}. \emph{The British Journal for the Philosophy of Science}, 74(1), 171-197. 

%Lam, Wai Fung, and Elinor Ostrom. 2010.
%\dQuote{Analyzing the Dynamic Complexity of Development Interventions: Lessons
%from an Irrigation Experiment in Nepal.}
%\emph{Policy Sciences} 43 (2):1-25.
%}


\seealso{\code{\link{cna}}, \code{\link{configTable}}, \code{\link{cna-solutions}}, \code{\link[cna]{condition}}, \code{\link[cna]{condList}}, \code{\link{detailMeasures}}}

\examples{
# Candidate asf for the d.jobsecurity data.
x <- "S*R + C*l + L*R + L*P <-> JSR"
# Create summary tables.
condTbl(x, d.jobsecurity)
# Using non-standard evaluation measures.
condTbl(x, d.jobsecurity, measures = c("PAcon", "PACcov"))

# Candidate csf for the d.jobsecurity data.
x <- "(C*R + C*V + L*R <-> P)*(P + S*R <-> JSR)"
# Create summary tables.
condTbl(x, d.jobsecurity)
# Non-standard evaluation measures.
condTbl(x, d.jobsecurity, measures = c("Ccon", "Ccov"))

# Boolean conditions.
cond <- c("-(P + S*R)", "C*R + !(C*V + L*R)", "-L+(S*P)")
condTbl(cond, d.jobsecurity) # only frequencies are returned

# Do not print measures.
condTbl(x, d.jobsecurity) |> print(printMeasures = FALSE)
# Print more digits.
condTbl(x, d.jobsecurity) |> print(digits =  10)
# Print more measures.
detailMeasures(x, d.jobsecurity,
               what = c("Ccon", "Ccov", "PAcon", "PACcov"))
               
# Analyzing d.jobsecurity with standard evaluation measures.
ana1 <- cna(d.jobsecurity, con = .8, cov = .8, outcome = "JSR")
# Reshape the output of the condition function in such a way as to make it identical to the
# output returned by msc, asf, and csf.
head(as.condTbl(condition(msc(ana1), d.jobsecurity)), 3)
head(as.condTbl(condition(asf(ana1), d.jobsecurity)), 3)
head(as.condTbl(condition(csf(ana1), d.jobsecurity)), 3)
head(condTbl(csf(ana1), d.jobsecurity), 3) # Same as preceding line
}
