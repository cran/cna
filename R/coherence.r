
# function "coherence"
# coherence of conditions (typically complex conditions)
# returns 1 for an atomicCond
# returns NA for a booleanCond
# cond   character vector
# x      truthTab or data frame
# type   type of truthTab 
coherence <- function(cond, tt, type){
  if (inherits(tt, "truthTab")){
    type <- attr(tt, "type")
  } else {
    if (missing(type)) type <- "cs"
    tt <- truthTab(tt, type = type, rm.dup.factors = FALSE, rm.const.factors = FALSE)
  }
  stdClasses <- c("stdBoolean", "stdAtomic", "stdComplex")
  inhStd <- stdClasses[as.logical(inherits(cond, stdClasses, which = TRUE))]
  # Shortcut for stdBoolean and stdAtomic
  if (length(inhStd) == 1 && inhStd %in% c("stdBoolean", "stdAtomic")){
    val <- switch(inhStd, stdBoolean = NA_real_, stdAtomic = 1)
    coh <- rep(val, length(cond))
    names(coh) <- cond
    return(coh)
  }
  cond <- noblanks(cond) # don't use noblanks earlier because it removes the class attribute
  .coher(cond, tt, 
         std = identical(inhStd, "stdComplex"))
}


# .coher(): internal function calculating coherence measures
#   cond    char evtor, conditions, typically csf's
#   tt      truthTab
#   std     Logical: Does cond contain csf's in standard form?
#   names   Logical: Add names to toutput vector?
#   tti, qc_tt
#           These "intermediate results" can be passed directly
#           NOTE THAT THIS ONLY WORKS IF  STD=true !!
.coher <- function(cond, tt, std = inherits(cond, "stdComplex"),
                   names = TRUE, tti = tt.info(tt), 
                   qc_tt = qcond_csf(cond, tti$scores, flat = TRUE)){
  coh <- rep(NA_real_, length(cond))
  if (std){
    nms <- C_relist_Char(attr(qc_tt, "condition"), attr(qc_tt, "csflengths"))
    isComplexCond <- rep(TRUE, length(cond))
  } else {
    cnd <- condition.default(cond, tt, rm.parentheses = FALSE)
    nms <- nmsList(cnd)
    isAtomicCond <- vapply(cnd, inherits, logical(1), "atomicCond", USE.NAMES = FALSE)
    if (any(isAtomicCond)){
      coh[isAtomicCond] <- 1
    }
    isComplexCond <- vapply(cnd, inherits, logical(1), "complexCond", USE.NAMES = FALSE)
  }
  if (any(isComplexCond)){
    repr <- csf_representation(nms)
    ccond1 <- strsplit(repr[isComplexCond], ",", fixed = TRUE)
    ccond2 <- happly(ccond1, extract_asf)
    hl <- hlengths(ccond2)
    csflen1 <- vapply(relist1(hl[[2]], hl[[1]]), max, integer(1)) <= 1
    coh[isComplexCond][csflen1] <- 1
    if (!all(csflen1)){
      f <- if (missing(tt)) tti$freq else attr(tt, "n")
      n <- length(f)
      if (std){
        subs <- rep(!csflen1, collapse(hl, 2)[[1]])
        x <- qc_tt[, , subs, drop = FALSE]
      } else {
        cnd <- cnd[isComplexCond][!csflen1]
        ll <- lengths(cnd, use.names = FALSE)
        x <- array(unlist(cnd, use.names = F),
                   c(n, 2, sum(ll)))
      }  
      simil <- 1 - abs(x[, 1, , drop = FALSE] - x[, 2, , drop = FALSE])
      dim(simil) <- dim(simil)[c(1, 3)]
      r1 <- relist1(simil, hlengths(ccond2[!csflen1])[[2]]*n)
      r2 <- lapply(r1, matrix, n)
      coh1 <- colSums(vapply(r2, rowMins, numeric(n))*f) / 
        colSums(vapply(r2, rowMaxs, numeric(n))*f)
      coh[isComplexCond][!csflen1] <- 
        vapply(relist1(coh1, lengths(ccond2)[!csflen1]), min, numeric(1),
               na.rm = TRUE)
    }
  }
  if (names) names(coh) <- if (any(isComplexCond)) repr else cond
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
#   x is a list of character vetors as retuned by extract_asf()
csf_representation <- function(asfs){
  out <- character(length(asfs))
  ll <- lengths(asfs)
  l1 <- ll == 1
  out[l1] <- unlist(asfs[l1])
  if (any(!l1)){
    u <- toupper(unlist(asfs[!l1], recursive = F))
    ff <- relist1(lapply(visible2parsed(u), all.vars),
                  ll[!l1])
    out[!l1] <- mapply(.repr1, ff, asfs[!l1], 
                       SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }
  out
}
.repr1 <- function (ff, x){
  D <- diag(length(x)) * NA
  lower <- cbind(row(D)[lower.tri(D)], col(D)[lower.tri(D)])
  D[lower] <- mapply(function(i1, i2) 1 - anyCommon(ff[[i1]], 
      ff[[i2]]), lower[, 1], lower[, 2], SIMPLIFY = TRUE)
  grps <- cutree(hclust(as.dist(D), method = "single"), h = 0.5)
  out <- C_mconcat(split(paste0("(", x, ")"), grps), "*")
  C_concat(out, sep = ",")
}
anyCommon <- function(s1, s2) any(match(s1, s2, nomatch = 0L) > 0L)

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
