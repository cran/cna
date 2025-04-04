
# Generic function coherence
coherence <- function(x, ...){
  UseMethod("coherence")
}

# method coherence.default
# coherence of conditions (typically complex conditions)
# returns 1 for an atomicCond
# generates error for a booleanCond
# x      character vector
# ct     configTable or data frame
# type   type of configTable
coherence.default <- function(x, ct, type, ...){
  if (length(x) == 0L) return(numeric(0))
  x <- noblanks(x)
  if (inherits(ct, "configTable")){
    type <- attr(ct, "type")
  } else {
    if (missing(type)) type <- "auto"
    ct <- configTable(ct, type = type, rm.dup.factors = FALSE, rm.const.factors = FALSE)
  }
  cti <- ctInfo(ct)
  qtypes <- .qcondType(x, colnames(cti$scores), cti$type, 
                       stdComplex.multiple.only = FALSE) 
  ok <- qtypes %in% c("stdAtomic", "stdComplex")
  if (any(!ok)){
    stop("Invalid condition(s):\n", 
         paste0("  ", x[!ok], collapse = "\n"),
         "\ncoherence() expects valid asf or csf in standard form.",
         call. = FALSE)
  }
  coherence(cti, cond = x)
}


# coherence.cti: internal function calculating coherence measures
#   x       cti
#   cond    char vector, conditions, typically csf's
#   names   Logical: Add names to toutput vector?
coherence.cti <- function(x, cond, names = TRUE, ...){
  qc_ct <- qcond_csf(cond, x$scores, flat = TRUE)
  out <- rep(NA_real_, length(cond))
  nms <- C_relist_Char(attr(qc_ct, "condition"), attr(qc_ct, "csflengths"))
  isComplexCond <- rep(TRUE, length(cond))
  repr <- csf_representation(nms)
  ccond1 <- strsplit(repr[isComplexCond], ",", fixed = TRUE)
  ccond2 <- happly(ccond1, extract_asf)
  hl <- hlengths(ccond2)
  csflen1 <- vapply(relist1(hl[[2]], hl[[1]]), max, integer(1)) <= 1
  out[isComplexCond][csflen1] <- 1
  if (!all(csflen1)){
    f <- x$freq
    n <- length(f)
    subs <- rep(!csflen1, collapse(hl, 2)[[1]])
    x <- qc_ct[, , subs, drop = FALSE]
    simil <- 1 - abs(x[, 1, , drop = FALSE] - x[, 2, , drop = FALSE])
    dim(simil) <- dim(simil)[c(1, 3)]
    r1 <- relist1(simil, hlengths(ccond2[!csflen1])[[2]]*n)
    r2 <- lapply(r1, matrix, n)
    out1 <- colSums(vapply(r2, rowMins, numeric(n))*f) / 
      colSums(vapply(r2, rowMaxs, numeric(n))*f)
    out[isComplexCond][!csflen1] <- 
      vapply(relist1(out1, lengths(ccond2)[!csflen1]), min, numeric(1),
             na.rm = TRUE)
  }
  if (names) names(out) <- ifelse(attr(qc_ct, "csflengths")>1, repr, cond)
  out
}

# *** Auxiliary functions *** 

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
