
exhaustive <- function(cond, x, ...){
  cond <- noblanks(cond)
  exhaust1(x, cond, ...)
}

# ==== exhaust1() ====
# exhaust1: tests if a cond is exhaustive for a tt

# Generic function
# Switch order of first 2 args to provide dispatching on x
exhaust1 <- function(x, cond, ...) UseMethod("exhaust1")

# ==== Method for class 'tti' ====
#   cond      character vector with the cond
#   tt        truthTab
exhaust1.tti <- function(x, cond, ...){
  tti.full <- full.tt(x)
  IDdata <- rowID(x)
  IDfull <- rowID(tti.full)
  stopifnot(IDdata %in% IDfull, #anyDuplicated(IDdata) == 0L, 
            anyDuplicated(IDfull) == 0L)
  complementary.sc <- tti.full$scores[is.na(match(IDfull, IDdata)), , drop = FALSE]
  qcnd <- qcond_csf(cond, complementary.sc, flat = TRUE)

  ll <- attr(qcnd, "csflengths")
  eq <- matrix(qcnd[, 1, ] != qcnd[, 2, ], nrow = dim(qcnd)[1], ncol = dim(qcnd)[3])
  r <- rep(seq_along(ll), ll)
  setNames(colAlls(eq %*% outer(r, seq_along(ll), "==") > 0), cond)
}

# ==== Method for class 'truthTab' ====
# Function suited for interactive use
exhaust1.truthTab <- function(x, cond, ...){
  tti <- tt.info(x)
  exhaust1.tti(tti, cond, ...)
}

# ==== Default Method (for matrix or data.frame) ====
# builds 
#   x       truthTab
# value:    truthTab, mv if original is mv, cs else
exhaust1.default  <- function(x, cond, ...){
  if (is.matrix(x) || is.data.frame(x)){
    x <- truthTab(x)
    exhaust1.truthTab(x, cond, ...)
  } else {
    stop("Invalid specification of arguments")
  }
}
