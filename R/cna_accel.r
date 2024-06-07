
# function hasSubsetInM
# y integer matrix
# x integer matrix, ncol(x) <= ncol(y)
# returns logical vector of length nrow(y),
#   position i is TRUE if there is one element in the rows of x which is a subset of x[i, ]
hasSubsetInM <- function(y, x){
  stopifnot(is.matrix(x), is.matrix(y),
            typeof(x) == "integer", typeof(y) == "integer",
            ncol(x) <= ncol(y))
  C_hasSubsetInM(y, x)
}

# function findAllSubsets
#   x     integer matrix
#   k     integer vector, k<=length(cols)
#   cols  column subsetting in x, all 1<=k<=ncol(x)
# returns a matrix with k columns containing all subsets of
# k elements appearing in a row of x[, cols]
findAllSubsets <- function(x, k, cols = seq_len(ncol(x))){
  if (k == 1){
    return(matrix(sort(unique.default(x[, cols])), ncol = 1))
  }
  out <- combn(
    cols, k,
    FUN = function(a) C_uniqueCombs(x, a),
    simplify = FALSE)
  do.call(rbind, out)
}


# conj_conCov: Fast calculation of coverages of conjunctions of conjunctions
#   x      A "scores" matrix with numeric values between 0 and 1
#   cols   Integer matrix with selections of columns of x as rows
#   y      Numeric vector with values between 0 and 1: scores of outcome variable
#   f      Integer vector: frequencies of the rows of x
# value  A matrix with 2 rows and length(cols) columns:
#        consistencies and coverages of conjunctions of columns of x
conj_conCov <- function(cols, x, y, f){
  out <- apply(cols, 1, C_conj_conCov, x, y, f)
  if (!is.matrix(out)) out <- matrix(out, nrow = 2L)
  rownames(out) <- c("con", "cov")
  out
}

# disj_conCov: same as conj_conCov for disjunctions instead of conjunctions
#   x      A "scores" matrix with numeric values between 0 and 1
#   cols   Integer vector: selection of columns of x
#   y      Numeric vector with values between 0 and 1: scores of outcome variable
#   f      Integer vector: frequencies of the rows of x
# value  A matrix with 2 rows and length(cols) columns:
#        consistencies and coverages of conjunctions of columns of x
disj_conCov <- function(cols, x, y, f){
  out <- apply(cols, 1, C_disj_conCov, x, y, f)
  if (!is.matrix(out)) out <- matrix(out, nrow = 2L)
  rownames(out) <- c("con", "cov")
  out
}
