
# function "coherence"
# coherence of conditions (typically complex conditions)
# returns 1 for an atomicCond
# returns NA for a booleanCond
# cond   character vector
# x      truthTab or data frame
# type   type of truthTab 
coherence <- function(cond, tt, type){
  if (inherits(tt, "truthTab")) {
    type <- attr(tt, "type")
  } else {
    if (missing(type)) type <- "cs"
    tt <- truthTab(tt, type = type)
  }
  cnd <- condition.default(cond, tt)
  nms <- nmsList(cnd)
  repr <- vapply(nms, csf_representation, character(1), USE.NAMES = FALSE)
  coh <- numeric(length(cond))
  names(coh) <- repr
  isComplexCond <- vapply(cnd, inherits, logical(1), "complexCond", USE.NAMES = FALSE)
  coh[!isComplexCond] <- NA
  if (any(isComplexCond))
    for (i in which(isComplexCond)){
      r <- strsplit(repr[[i]], ", ")[[1]]
      if (length(r) == 1){
        coh[[i]] <- coh1(cnd[[i]])
      } else {
        coh[[i]] <- .min(coherence(r, tt))
      }
    }
  coh
}


# *** Auxiliary functions *** 

# nmsList: returns names(x[[i]]) if inherits(x[i], "complexCond"),
# or names(x)[[i]] otherwise
nmsList <- function(x){
  # x   a list of conds (value of condition)
  out <- as.list(names(x))
  isComplexCond <- vapply(x, inherits, logical(1), "complexCond")
  out[isComplexCond] <- lapply(x[isComplexCond], names)
  out
}
# csf_representation: Determines the connectivity pattern within a complexCond 
# x is a character vetor with strings representing each an atomicCond
csf_representation <- function(x){
  if (length(x) == 1L) return(x)
  px <- visible2parsed(toupper(as.character(x)))
  ff <- lapply(px, all.vars)
  D <- diag(length(x)) * NA
  #  rownames(D) <- colnames(D) <- x
  lower <- cbind(row(D)[lower.tri(D)],
                 col(D)[lower.tri(D)])
  anyCommon <- function(s1, s2) length(intersect(s1, s2)) > 0L
  D[lower] <-
    mapply(function(i1, i2) 1- anyCommon(ff[[i1]], ff[[i2]]),
           lower[, 1], lower[, 2], SIMPLIFY = TRUE)
  grps <- cutree(hclust(as.dist(D), method = "single"), h = 0.5)
  out <- lapply(split(paste0("(", x, ")"), grps), paste, collapse = "*")
  do.call(paste, list(out, collapse = ", "))
}
# .min: as min(..., na.rm = TRUE), but returs NA if ... is empty or all NA
.min <- function(...){
  if (all(is.na(unlist(list(...))))) return(NA)
  min(..., na.rm = TRUE)
}
# coh1: calculates coherences
coh1 <- function(cnd){
  if (!inherits(cnd, "complexCond")) return(NA)
  n <- attr(cnd, "n")
  leftright <- lapply(cnd, function(a) 1 - abs(a[[1]] - a[[2]]))
  conj <- sum(do.call(pmin, c(leftright, list(na.rm = TRUE))) * n)
  disj <- sum(do.call(pmax, c(leftright, list(na.rm = TRUE))) * n)
  conj / disj
}

if (FALSE){
  datXY <- some(allCombs(rep(2, 8)) - 1, 20)
  coherence(c("(A*B <-> C)*(D+E <-> F)", "A*B <-> C", "(A*B <-> C)",
              "(A*B <-> C)*(C+D <-> E)", "(A*B <-> C)*(D*E <-> F)*(F*G <-> H)"), datXY)
  
  coherence("(A*B <-> C)*(D+E <-> F)", datXY)
  coherence("A*B <-> C", datXY)
  coherence("(A*B <-> C)", datXY)
  coherence("(A*B <-> C)*(C + D <-> E)", datXY)
  coherence("(A*B <-> C)*(D*E <-> F)*(F*G <-> H)", datXY)
}
