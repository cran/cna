
# Auxiliary function .redCsf
# takes a logical matrix x, as returned by redundant()
# Returns a character vector of conditions resulting from reducing the conds in
# rownames(x) according to x
.redCsf <- function(x){
  nms <- rownames(x)
  rownames(x) <- NULL
  
  k <- ncol(x)
  
  tr1 <- t(x)
  cnr <- row(tr1)[tr1]
  logdiag <- 1-diag(k)
  mode(logdiag) <- "logical"
  logmat <- logdiag[, cnr, drop = FALSE]
  
  rs <- rowSums(x)
  orig <- rep(seq_along(rs), rs)
  asfmat <- do.call(cbind, extract_asf(nms))[, orig, drop = FALSE]
  stopifnot(identical(dim(logmat), dim(asfmat)))
  
  asfmat <- matrix(asfmat[logmat], k-1)
  asfmat[] <- paste0("(", asfmat, ")")
  
  out <- do.call(paste, c(split(asfmat, row(asfmat)), sep = "*"))
  structure(out, originalCond = orig)
}  

# recursive function .minCsf
# takes cond and (either tti or full.tti)
# returns cond without redundancies
.minCsf <- function(cond, tti = NULL, full.tti = NULL, verbose = FALSE){
  ids <- attr(cond, "id")
  if (is.null(attr(cond, "id")))
    ids <- as.list(seq_along(cond))

  if (is.null(full.tti)){
    full.tti <- full.tt(tti)
  }
  
  rr <- redundant(cond, full.tti)
  anyRed <- rowAnys(rr)
  n.asf <- ncol(rr)
  if (verbose)
    cat("csf-lengths ", n.asf, ": ",
        sum(anyRed), " of ", length(cond), " csf are reducible ",
        sep = "")
  noRed <- rownames(rr)[!anyRed]
  attr(noRed, "id") <- ids[!anyRed]
  attr(noRed, "n.asf") <- rep(n.asf, length(noRed))
  if (!any(anyRed)){
    if (verbose) cat("\n")
    return(noRed)
  }
  withRed <- rr[anyRed, , drop = FALSE]
  condNew <- .redCsf(withRed)
  idsNew <- ids[anyRed][attr(condNew, "originalCond")]
  splitId <- split(idsNew, condNew)
  dups <- duplicated(condNew)
  if (verbose) cat("(", sum(!dups), " uniques)\n", sep = "")
  condNew <- condNew[!dups]
  condNew <- structure(condNew, 
                       id = unname(splitId[condNew]),
                       n.asf = rep(n.asf - 1L, sum(!dups)))
  recurs <- .minCsf(condNew, NULL, full.tti = full.tti, verbose = verbose)
  structure(c(recurs, noRed), 
            id = lapply(c(attr(recurs, "id"), attr(noRed, "id")),
                        function(...) unique.default(unlist(...)),
                        recursive = FALSE, use.names = FALSE),
            n.asf = c(attr(recurs, "n.asf"), attr(noRed, "n.asf")))
}

# Generic function minimalizeCsf
minimalizeCsf <- function(x, ...){
  UseMethod("minimalizeCsf")
}
# Default method (for character vector)
minimalizeCsf.default <- function(x, data, verbose = FALSE, ...){
  x <- noblanks(x)
  extract_asf <- extract_asf
  tt <- truthTab(data)
  minim <- .minCsf(x, tt.info(tt), verbose = verbose)
  redundantParts <- vector("list", length(minim))
  for (i in seq_along(minim)){
    redasf <- 
      lapply(extract_asf(x[attr(minim, "id")[[i]]]), 
             setdiff, extract_asf(minim[[i]])[[1]])
    redasf <- happly(redasf, function(x) if (length(x)) paste0("(", x, ")") else character(0))
    redundantParts[[i]] <- C_mconcat(redasf, "*")
  }
  out <- qcondTbl_csf(minim, 
                      tt.info(tt)$scores, 
                      attr(tt, "n"))
  out$n.asf <- attr(minim, "n.asf")
  out$redundantParts <- redundantParts 
  class(out) <- c("minimalizeCsf", "data.frame")
  out
}
# Method for class cna
minimalizeCsf.cna <- function(x, n = 20, verbose = FALSE, ...){
  csfs <- csf(x, max(n))$condition
  if (length(n)>1){
    n <- n[n<length(csfs)]
    csfs <- csfs[n]
  }
  minimalizeCsf.default(csfs, data = x$truthTab, verbose = verbose, ...)
}

# Clearer display of solutions:
print.minimalizeCsf <- function(x, subset = 1:5, ...){
  n <- nrow(x)
  subset <- intersect(subset, seq_len(n))
  cat("Object of class 'minimalizeCsf' containing", n, "solution(s)\n")
  if (length(subset) < n) 
    cat("Printing a subset of", length(subset), "solution(s)\n")
  cat("\n")
  for (i in subset){
    cat("=== Solution ", i, " ===\nCondition:\n", sep = "")
    writeLines(c(x$condition[i], ""))
    print.data.frame(x[i, -match(c("condition", "redundantParts"), names(x))])
    writeLines(c("\nredundant parts:",
                 unlist(x[i, 6])))
    cat("\n")
  }
  invisible()
}

