\name{print.cna}
\alias{print.cna}

\title{\code{print} method for an object of class \dQuote{cna}}

\description{
By default, the method \code{print.cna} first lists the implemented ordering (if any) as well as the pre-specified outcome(s) (if any). Second, it shows the measures \code{con} and \code{cov} used for model building. Third, the top 5 asf and, fourth, the top 5 csf are reported, along with an indication of how many solutions exist in total. To print all msc, asf, and csf, the value of \code{nsolutions} can be suitably increased, or the functions in \code{\link{cna-solutions}} can be used.


While msc and asf are stored in the output object of \code{\link{cna}}, csf are not. The latter are derived from the inventory of asf at execution time (by running the function \code{\link{csf}}) whenever a \dQuote{cna} object is printed.
}

\usage{
\method{print}{cna}(x, what = x$what, digits = 3, nsolutions = 5, 
      printMeasures = TRUE, details = x$details, show.cases = NULL, 
      verbose = FALSE, ...)
}

\arguments{
  \item{x}{Object of class \dQuote{cna}.}
  \item{what}{Character string specifying what to print; \code{"t"} for the configuration table, \code{"m"} for msc, \code{"a"} for asf, \code{"c"} for csf, and \code{"all"} for all. Defaults to \code{"ac"} if \code{suff.only = FALSE}, and to \code{"m"} otherwise (for \code{suff.only} see \code{\link{cna}}).}
  \item{digits}{Number of digits to print in evaluation measures (e.g. consistency and coverage) as well as in exhaustiveness, faithfulness, and coherence scores.}
  \item{nsolutions}{Maximum number of msc, asf, and csf to print. Alternatively, \code{nsolutions = "all"} will print all solutions.}
\item{printMeasures}{Logical; if \code{TRUE}, the output indicates which \code{measures} for sufficiency and necessity evaluation were used.}
  \item{details}{Character vector specifying the evaluation measures and additional solution attributes to be printed. Possible elements are all the measures in  \code{\link{showMeasures}}. Can also be \code{TRUE}/\code{FALSE}. If \code{FALSE} (default), no additional measures are returned; \code{TRUE} resolves to \code{c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence")}. See also \code{\link{detailMeasures}}.}
  \item{show.cases}{Logical; if \code{TRUE}, the attribute \dQuote{cases} of the analyzed \code{\link{configTable}} is printed (see \code{\link{print.configTable}}).}
  \item{verbose}{Logical; passed to \code{\link{csf}}.}
\item{\dots}{
  Arguments passed to other \code{print}-methods.}
}

\details{
The argument \code{what} regulates what items of the output of \code{\link{cna}} are printed. If the string assigned to
\code{what} contains the character \dQuote{\code{t}}, the configuration table is printed; if
it contains an \dQuote{\code{m}}, the msc are printed; if it contains an \dQuote{\code{a}}, the asf are printed; if it contains a \dQuote{\code{c}}, the csf are printed.
\code{what = "all"} and \code{what = "tmac"} print all output items. 
If the argument \code{suff.only} is set to \code{TRUE} in the \code{cna} call that generated \code{x}, \code{what} defaults to \code{"m"}. 

The argument \code{digits} determines how many digits of the evaluation measures and solution attributes are printed, while \code{nsolutions} fixes the number of conditions and solutions to print.\cr \code{nsolutions} applies separately to minimally sufficient conditions, atomic solution formulas, and complex solution formulas. \code{nsolutions = "all"} recovers all minimally sufficient conditions, atomic and complex solution formulas. \code{show.cases} is applicable if the \code{what} argument is given the value \dQuote{\code{t}}. In that case, \code{show.cases = TRUE} yields a configuration table featuring a \dQuote{cases} column, which assigns cases to configurations. 


The option \dQuote{spaces} controls how the conditions are rendered. The current setting is queried by typing \code{getOption("spaces")}. The option specifies characters that will be printed with a space before and after them. The default is \code{c("<->","->","+")}. A more compact output is obtained with \code{option(spaces = NULL)}. 
}

\seealso{\code{\link{cna}}, \code{\link{csf}}, \code{\link{cna-solutions}}, \code{\link{detailMeasures}}, \code{\link{showMeasures}}}




\examples{
# Analysis of crisp-set data.
cna.educate <- cna(d.educate)
cna.educate
# Print only complex solution formulas.
print(cna.educate, what = "c")
# Print only atomic solution formulas.
print(cna.educate, what = "a")
# Print only minimally sufficient conditions.
print(cna.educate, what = "m")
# Print only the configuration table.
print(cna.educate, what = "t")
# Print solutions with spaces before and after "*".
options(spaces = c("<->", "->", "*" ))
cna(d.educate, details = c("e", "f", "PAcon", "PACcov"))
# Restore the default of the option "spaces".
options(spaces = c("<->", "->", "+"))

# Analysis of multi-value data. 
cna.pban <- cna(d.pban, outcome = "PB=1", cov = .95, maxstep = c(6, 6, 10), 
								what = "all")
cna.pban
# Print only the atomic solution formulas.
print(cna.pban, what = "a", nsolutions = "all")
# Do not print the specification of the evaluation measures.
print(cna.pban, what = "a", nsolutions = "all", printMeasures = FALSE)
# Print further details.
print(cna.pban, nsolutions = "all", details = c("AACcon", "AAcov", "ex", "fa"))
# Print more digits.
print(cna.pban, nsolutions = "all", digits = 6)
# Print the configuration table with the "cases" column.
print(cna.pban, what = "t", show.cases = TRUE, printMeasures = FALSE)
}
