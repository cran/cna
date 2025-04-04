
# Generic function minimalizeCsf
minimalizeCsf <- function(x, ...){
  UseMethod("minimalizeCsf")
}
# Default method (for character vector)
minimalizeCsf.default <- function(
    x, ct = NULL, 
    measures = c("standard consistency", "standard coverage"), 
    removeSubCsf = TRUE, verbose = FALSE, ...){
  measures <- resolveMeasures(measures)
  if (length(x) == 0){
    return(emptyCondTbl("stdComplex", measures = measures))
  }
  x <- noblanks(x)
  nodata <- is.null(ct)
  if (nodata){
    ct <- ct0 <- auxConfigTable(x)
    if (attr(ct0, "type") == "mv") 
      message("minimalizeCsf() with cond of type \"mv\" usually requires explicit specification of x")
  } else {
    ct <- configTable(ct, rm.dup.factors = FALSE, rm.const.factors = FALSE, 
                      verbose = FALSE)
    ct0 <- auxConfigTable(x, ct)
  }
  # cti is the actual data, cti0 is a auxConfigTable
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
  out <- minimalizeCsf.cti(cti, cond = x, conCovDef = measures$def, 
                           removeSubCsf = removeSubCsf, verbose = verbose, 
                           x0 = cti0)
  if (useCtiList(cti)){
    cti <- ctiList(cti, cond = out$condition)
    if (nodata){
      ctifull <- full.ct(cti)
      getcc <- function(...) qcondTbl_csf(...)[, c("con", "cov"), drop = FALSE]
      cc <- mapply(getcc, condstr = split(ctifull$conds, ctifull$indices), 
                   sc = lapply(ctifull$tbls, "[[", "scores"), 
                   freqs = lapply(ctifull$tbls, "[[", "freq"),
                   MoreArgs = list(conCovDef = measures$def),
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)
      out[c("con", "cov")] <- 
        do.call(rbind, cc)[getPos(ctifull), , drop = FALSE]  
    }
  } else if (nodata){
    ctifull <- full.ct(cti0, cond = out$condition)
    out[c("con", "cov")] <- 
      qcondTbl_csf(out$condition, ctifull$scores, ctifull$freq, 
                   conCovDef = measures$def)[c("con", "cov")]
  }
  out$inus <- .det(cti, out$condition, what = "inus", in.csf = TRUE)$inus
  as.condTbl(out, condClass = "stdComplex", measures = measures)
}

minimalizeCsf.cti <- function(x, cond, conCovDef = 1:2, removeSubCsf = TRUE, 
                              verbose = FALSE, ..., x0 = x){
  if (useCtiList(x0)){
    xLst <- ctiList(x0, cond)
    xfull <- full.ct(xLst, cond)
  } else {
    xfull <- full.ct(x, cond)
  }
  minim <- .minCsf(xfull, cond, verbose = verbose)
  # remove solution csfs that are a submodel of another csf
  if (removeSubCsf) minim <- minim[!findSubCsf(minim)]
  out <- qcondTbl_csf(minim, x$scores, x$freq, conCovDef = conCovDef)
  names(out)[3:4] <- c("con", "cov")
  out$complexity <- getComplexity(out$condition)
  # sort the remaining solution
  ord <- with(out, order(complexity, -con*cov, 
                         outcomeOrder(outcome, x$resp_nms)))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}
# Aux fn
# outc  a char vec containing comma-sparated lists of outcomes, e.g. "A,B"
outcomeOrder <- function(outc, values){  
  outcms <- strsplit(outc, ",", fixed = TRUE)
  outcms <- happly(outcms, match, table = values)
  ll <- lengths(outcms)
  maxl <- max(ll)
  red <- ll < maxl
  if (any(red)){
    outcms[red] <- lapply(outcms[red], function(x) c(rep(0L, maxl - length(x)), x))
  }
  outcms <- matrix(unlist(outcms), ncol = maxl, byrow = TRUE)
  ord <- do.call(order, split.default(outcms, col(outcms)))
  match(seq_along(outc), ord)
}


# find solution csfs that are a submodel of another csf
findSubCsf <- function(cond){
  asfList <- extract_asf(cond)
  asfs <- unique(unlist(asfList))
  match_asf <- happly(asfList, match, asfs)
  match_asf[] <- lapply(match_asf, sort)
  rmSol <- C_hasSupersetIn(match_asf, match_asf, ignore_equals = TRUE)
  rmSol
}

# Method for class cna
minimalizeCsf.cna <- function(x, n = 20, verbose = FALSE, ...){
  csfs <- csf(x, n.init = max(n), inus.only = FALSE)$condition
  if (length(csfs) == 0){
    return(emptyCondTbl("stdComplex", details = getDetailCols(x$details),
                        measures = x$measures))
  }
  if (length(n)>1){
    n <- n[n<length(csfs)]
    csfs <- csfs[n]
  }
  minimalizeCsf.default(csfs, ct = x$configTable, 
                        measures = x$measures, verbose = verbose, ...)
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
