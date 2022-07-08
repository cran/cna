\name{cna-internals}

\alias{cna-internals}
\alias{getCond}
\alias{noblanks}
\alias{rhs}
\alias{lhs}
\alias{extract_asf}
\alias{ctInfo}
\alias{qcond_csf}
\alias{qcond_asf}
\alias{qcond_bool}
\alias{stdCond}
\alias{matchCond}
\alias{fs2cs}
\alias{C_recCharList2char}
\alias{C_relist_Int}
\alias{relist1}
\alias{C_mconcat}
\alias{hstrsplit}
\alias{C_redund}
\alias{getComplexity}
\alias{getCondType}

\title{Internal functions in the cna package}

\description{
These functions are mainly for internal purposes and will not normally be called directly by the user.
}

\section{Overview of Functions}{
  \describe{
    \item{\code{noblanks}, \code{lhs}, \code{rhs}, \code{extract_asf}:}{
      Manipulation of character vectors containing conditions.
    }
    \item{\code{ctInfo}:}{
      Alternative internal represenation of a \code{\link{configTable}}.
    }
    \item{\code{qcond_bool}, \code{qcond_asf}, \code{qcond_csf}:}{
      Fast evaluation of certain aspects of the well-formedness of conditions.
    }
    \item{\code{getCondType}, \code{getComplexity}:}{
      Extract condition type and complexity values from a character vector containing conditions.
    }
    \item{\code{fs2cs}:}{
      Convert an \dQuote{fs} configTable to \dQuote{cs}.
    }
    \item{\code{stdCond}, \code{matchCond}:}{
      Standardize and match conditions.
    }
    \item{\code{relist1}, \code{hstrsplit}, \code{C_mconcat}, \code{C_recCharList2char}, \code{C_relist_Int}:}{
      Utility functions for conversion between different represenations of conditions.
    }
    \item{\code{C_redund}:}{
      Internal version of the test for structural redundancy (used in \code{\link{redundant})}.
    }
    \item{\code{getCond}:}{
      Derive a condition from a data set. 
    }
  }
}

\keyword{internal}
