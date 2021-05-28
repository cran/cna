
# Generic function 'some'
# =======================
#   taken from 'car' package
# some <- function (x, ...) UseMethod("some")

# some.data.frame
# ===============
# Method taken from the car package and modified
some.data.frame <- function (x, n = 10, replace = TRUE, ...)
{
    nr <- nrow(x)
    if (!replace) n <- min(n, nr)
    i <- sort(sample(nr, n, replace = replace))
    x[i, , drop = FALSE]
}

# some.configTable
# ================
# some os a generic function from package 'car'
# added argument replace
some.configTable <- function (x, n = 10, replace = TRUE, ...){
  nr <- nrow(x)
  if (!replace) n <- min(n, nr)
  i <- sort(sample(nr, n, replace = replace))
  xx <- x[i, ]
  hasDupCases <- vapply(attr(xx, "cases"), anyDuplicated, 1L) > 0L
  if (any(hasDupCases)){
    attr(xx, "cases")[any(hasDupCases)] <- lapply(attr(xx, "cases")[any(hasDupCases)], make.unique)
  }
  configTable(xx, rm.dup.factors = FALSE, rm.const.factors = FALSE, ...)
}

