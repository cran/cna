
# Generic function minimalizeCsf
minimalizeCsf <- function(x, ...){
  UseMethod("minimalizeCsf")
}
# Default method (for character vector)
minimalizeCsf.default <- function(x, ct = NULL, verbose = FALSE, ..., data){
  
    # Ensure backward compatibility of argument 'data'
    if (!missing(data)){
      warning("Argument 'data' is deprecated in minimalizeCsf(); use 'ct' instead.", 
              call. = FALSE)
      if (missing(ct)) ct <- data
    }
  
  if (length(x) == 0){
    return(emptyCondTbl())
  }
  x <- noblanks(x)
  nodata <- is.null(ct)
  if (nodata){
    ct0 <- auxConfigTable(x)
    if (attr(ct0, "type") == "mv") 
      message("minimalizeCsf() with cond of type \"mv\" usually requires explicit specification of x")
  } else {
    ct0 <- auxConfigTable(x, ct)
  }
  
  ct <- if (nodata) ct0 else configTable(ct, rm.dup.factors = FALSE, rm.const.factors = FALSE, 
                                         verbose = FALSE)
  cti <- ctInfo(ct)
  cti0 <- ctInfo(ct0)
  qtypes <- .qcondType(x, colnames(cti$scores), cti$type, 
                       stdComplex.multiple.only = FALSE) 
  ok <- qtypes %in% c("stdAtomic", "stdComplex")
  if (any(!ok)){
    stop("Invalid condition(s):\n", 
         paste0("  ", x[!ok], collapse = "\n"),
         "\nminimalizeCsf() expects valid asf or csf in standard form.",
         call. = FALSE)
  }
  out <- minimalizeCsf.cti(cti, cond = x, verbose = verbose, x0 = cti0)
  if (useCtiList(cti)){
    cti <- ctiList(cti, cond = out$condition)
    if (nodata){
      ctifull <- full.ct(cti)
      getcc <- function(...) qcondTbl_csf(...)[, c("con", "cov"), drop = FALSE]
      cc <- mapply(getcc,
                   condstr = split(ctifull$conds, ctifull$indices), 
                   sc = lapply(ctifull$tbls, "[[", "scores"), 
                   freqs = lapply(ctifull$tbls, "[[", "freq"),
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)
      out[c("consistency", "coverage")] <- 
        do.call(rbind, cc)[getPos(ctifull), , drop = FALSE]  
    }
  } else if (nodata){
    ctifull <- full.ct(cti0, cond = out$condition)
    out[c("consistency", "coverage")] <- 
      qcondTbl_csf(out$condition, ctifull$scores, ctifull$freq)[c("con", "cov")]
  }
  out$inus <- .det(cti, out$condition, what = "inus", in.csf = TRUE)$inus
  as.condTbl(out, condClass = "stdComplex")
}

minimalizeCsf.cti <- function(x, cond, verbose = FALSE, ..., x0 = x){
  if (useCtiList(x0)){
    xLst <- ctiList(x0, cond)
    xfull <- full.ct(xLst, cond)
    minim <- .minCsf(xfull, cond, verbose = verbose)
  } else {
    xfull <- full.ct(x, cond)
    minim <- .minCsf(xfull, cond, verbose = verbose)
  }
  out <- qcondTbl_csf(minim, 
                      x$scores, 
                      x$freq)
  names(out)[3:4] <- c("consistency", "coverage")
  out$complexity <- getComplexity(out$condition)
  out
}

# Method for class cna
minimalizeCsf.cna <- function(x, n = 20, verbose = FALSE, ...){
  csfs <- csf(x, n.init = max(n), inus.only = FALSE)$condition
  if (length(csfs) == 0){
    return(emptyCondTbl())
  }
  if (length(n)>1){
    n <- n[n<length(csfs)]
    csfs <- csfs[n]
  }
  minimalizeCsf.default(csfs, ct = x$configTable, verbose = verbose, ...)
}

# Auxiliary function .redCsf
# takes a logical matrix x, as returned by redundant()
# Returns a character vector of conditions resulting from reducing the conds in
# rownames(x) according to x
.redCsf <- function(x){
  nms <- names(x)
  names(x) <- NULL
  which_asf_red <- lapply(x, which)
  orig <- rep(seq_along(x), lengths(which_asf_red))
  asfList <- extract_asf(nms)
  asfList <- mapply("[", asfList[orig], -unlist(which_asf_red), SIMPLIFY = FALSE)
  structure(C_mconcat(happly(asfList, function(x) paste0("(", x, ")")), sep = "*"), 
            orig = orig)
}

# recursive function .minCsf
# takes cond (either cti or full.cti) and 
# returns cond without redundancies
.minCsf <- function(x, cond, ...){
  UseMethod(".minCsf")
}

.minCsf.cti <- function(x, cond, verbose = FALSE, ...){
  # x ist expected to be "full"!!
  rr <- .redund(x, cond, full = F, simplify = FALSE)
  anyRed <- m_any(rr)
  if (verbose){
    n.asf <- lengths(rr, use.names = FALSE)
    cat("csf-length ", n.asf[1], ": ",
        sum(anyRed), " of ", length(cond), " csf are reducible ",
        sep = "")
  }
  noRed <- names(rr)[!anyRed]
  if (!any(anyRed)){
    if (verbose) cat("\n")
    return(noRed)
  }
  withRed <- rr[anyRed]
  condNew <- .redCsf(withRed)
  dups <- duplicated(condNew)
  if (verbose) cat("(", sum(!dups), " uniques)\n", sep = "")
  condNew <- condNew[!dups]
  recurs <- .minCsf.cti(x, condNew, verbose = verbose)
  c(recurs, noRed)
}
