
exfaith <- function(cond, x, ...){
  exff(x, noblanks(cond), ...)
}

# ==== exff() ====
# calcuate exhaustiveness/Faithfulness of a cond

# Generic function
# Switch order of first 2 args to provide dispatching on x
exff <- function(x, cond, ...) UseMethod("exff")

# ==== Method for class 'tti' ====
#   cond      character vector with the cond
#   x         truthTab
#   num       Return numeric value in [0, 1] (if FALSE: logical)
#   tti.full  full.tt(x)
exff.tti <- function(x, cond, num = TRUE, names = TRUE, tti.full = full.tt(x), 
                     qc_full = qcond_csf(cond, tti.full$scores, flat = TRUE),
                     ...){
  stopifnot(identical(x[c("resp_nms", "nVal", "uniqueValues", 
      "config")], tti.full[c("resp_nms", "nVal", "uniqueValues", 
      "config")]))
  IDdata <- rowID(x)
  IDfull <- rowID(tti.full)
  n <- length(IDfull)
  stopifnot(IDdata %in% IDfull, anyDuplicated(IDfull) == 0L)
  in.data <- !is.na(match(IDfull, IDdata))
  #qc_full <- qcond_csf(cond, tti.full$scores, flat = TRUE)
  ll <- attr(qc_full, "csflengths")
  eq <- matrix(qc_full[, 1, ] == qc_full[, 2, ], 
               nrow = n, ncol = dim(qc_full)[3])
  r <- rep(seq_along(ll), ll)
  cond.ok <- eq %*% outer(r, seq_along(ll), "==") == rep(ll, each = n)
  if (num){  
    out <- cbind(
      colSums(in.data & cond.ok) / colSums(cond.ok), # exhaustiveness
      colSums(in.data & cond.ok) / sum(in.data))     # faithfulness
  } else {
    out <- cbind(
      !colAnys(cond.ok & !in.data), # exhaustive
      !colAnys(!cond.ok & in.data)) # faithful
  }
  colnames(out) <- c("exhaustiveness", "faithfulness")
  if (names) rownames(out)  <- cond
  out
}

# ==== Method for class 'truthTab' ====
# Function suited for interactive use
exff.truthTab <- function(x, cond, ...){
  tti <- tt.info(x)
  exff.tti(tti, cond, ...)
}

# ==== Default Method (for matrix or data.frame) ====
# builds 
#   x       truthTab
# value:    truthTab, mv if original is mv, cs else
exff.default  <- function(x, cond, ...){
  if (is.matrix(x) || is.data.frame(x)){
    x <- truthTab(x)
    exff.truthTab(x, cond, ...)
  } else {
    stop("Invalid specification of arguments")
  }
}

# ------------------------------------------------------------------------------

if (F){
  library(cna)
  cna:::internals()
  
  dat1 <- allCombs(c(2,2,2,2,2))-1
  dat2 <- selectCases("A*D + B*e <-> C", dat1)
  
  exfaith(c("A*D + B*e <-> C", "A*D <-> C", "A*D + B*e <-> E"), 
          dat2)
  exfaith(c("A*D + B*e <-> C", "A*D <-> C", "A*D + B*e <-> E"), 
          cna:::tt.info(dat2), num = F)
  
  condTbl(c("A*D + B*e <-> C", "A*D <-> C", "A*D + B*e <-> E"), 
          dat2)
  
  # exhaustive but not faithful
  dat3 <- selectCases("A*D + B*e -> C", dat1)
  exfaith("A*D + B*e <-> C", cna:::tt.info(dat3))
  
  # faithful but not exhaustive
  dat4 <- selectCases("A*D + B*e <-> C", dat1)[-1, ]
  exfaith("A*D + B*e <-> C", cna:::tt.info(dat4))
  
  
  
  mycond <- "A*D + B*e <-> C" # consequens C or E
  mydata <- dat3
  
  full <- full.tt(mydata)
  D <- as.integer(cna:::rowID(cna:::tt.info(full)) %in% cna:::rowID(cna:::tt.info(mydata)))
  cc <- condition(mycond, full)[[1]]
  A <- cc[[1]]
  C <- cc[[2]]
  E <- condition(mycond, full, force.bool = TRUE)[[c(1, 1)]]
  
  condTbl(mycond, mydata)
  sum(D*A*C) / sum(D*A)  # con
  sum(D*A*C) / sum(D*C)  # cov
  
  exfaith(mycond, cna:::tt.info(mydata))
  sum(D*E) / sum(E)  # exhaustive
  sum(D*E) / sum(D)  # faithful
  
  exfaith(mycond, cna:::tt.info(mydata))
  exhaustive(mycond, mydata)
}  
