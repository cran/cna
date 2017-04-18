
# Generic function 'some'
# =======================
#   taken from 'car' package
some <- function (x, ...) UseMethod("some")

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

# some.truthTab
# =============
# some os a generic function from package 'car'
# added argument replace
some.truthTab <- function (x, n = 10, replace = TRUE, ...){
  nr <- nrow(x)
  if (!replace) n <- min(n, nr)
  i <- sort(sample(nr, n, replace = replace))
  xx <- x[i, ]
  attr(xx, "cases") <- lapply(attr(xx, "cases"), make.unique)
  truthTab(xx)
}

