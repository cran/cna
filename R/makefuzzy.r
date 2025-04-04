
# function makeFuzzy:
# ==================
# Subsets a data frame x based on a condition cond
# Arguments:
#   x          data frame or configTable
#   fuzzvalues values to add/substract at random from elements of x
# Value: resulting configTable of type "fs"
makeFuzzy <- function(x, fuzzvalues = c(0, .05, .1), ...){
  if (is.null(colnames(x))){
    colnames(x) <- make.unique(LETTERS[seq_len(ncol(x))])
  }
  xfuzzy <- ct2df(x)
  ulx <- unlist(xfuzzy, use.names = FALSE)
  xfuzzy[, ] <- ifelse(as.logical(ulx),
                       1 - sample(fuzzvalues, length(ulx), replace = TRUE),
                       sample(fuzzvalues, length(ulx), replace = TRUE))
  configTable(xfuzzy, type = "fs", rm.dup.factors = FALSE, rm.const.factors = FALSE, ...)
}

