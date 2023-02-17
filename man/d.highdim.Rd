\name{d.highdim}

\docType{data}

\alias{d.highdim}

\title{Artificial data with 50 factors and 1191 cases}

\description{
These crisp-set data are simulated from a presupposed data generating structure (i.e. a causal chain). They feature 20\% noise and massive fragmentation (limited diversity). \code{d.highdim} is used to illustrate CNA's capacity to analyze high-dimensional data.
}

\usage{d.highdim}

\format{
The data frame contains 50 factors (columns), V1 to V50, and 1191 rows (cases). It was simulated from the following data generating structure:
\deqn{(v2*V10 + V18*V16*v15 <-> V13)*(V2*v14 + V3*v12 + V13*V19 <-> V11)}
20\% of the cases in \code{d.highdim} are incompatible with that structure, meaning they are affected by noise or measurement error. The fragmentation is massive, as there is a total of 281 trillion (\eqn{2^{48}}) configurations over the set \{V1,...,V50\} that are compatible with that structure. 
}


\source{
\code{d.highdim} has been generated with the following code:\cr\cr
\code{RNGversion("4.0.0")}\cr
\code{set.seed(39)}\cr
\code{m0 <- matrix(0, 5000, 50)}\cr
\code{dat1 <- as.data.frame(apply(m0, c(1,2), function(x) sample(c(0,1), 1)))} \cr
\code{target <- "(v2*V10 + V18*V16*v15 <-> V13)*(V2*v14 + V3*v12 + V13*V19 <-> V11)"}\cr
\code{dat2 <- ct2df(selectCases(target, dat1))}\cr
\code{incomp.data <- dplyr::setdiff(dat1, dat2)} \cr\cr

\code{no.replace <- round(nrow(dat2)*0.2)}\cr
\code{a <- dat2[sample(nrow(dat2), nrow(dat2)-no.replace, replace = FALSE),]}\cr
\code{b <- some(incomp.data, no.replace)}\cr
\code{d.highdim <- rbind(a, b)}\cr
\code{head(d.highdim)}
}

\keyword{datasets}
