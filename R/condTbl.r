
# extract msf from cna-object
msc <- function(x){
  stopifnot(inherits(x, "cna"))
  msc.list <- lapply(x$solution, "[[", "msc")
  if (all(vapply(msc.list, is.null, logical(1))))
    return(data.frame(outcome = character(0),
                      condition = character(0),
                      consistency = numeric(0),
                      coverage = numeric(0),
                      complexity = numeric(0)))
  all.msc <- do.call(rbind, msc.list)
  outcome <- rep(names(msc.list),
                 vapply(msc.list, function(fr) if(is.null(fr)) 0L else nrow(fr), integer(1)))
  out <- 
    data.frame(outcome = outcome,
               condition = paste(all.msc$condition, "->", outcome),
               consistency = all.msc$consistency,
               coverage = all.msc$coverage,
               complexity = all.msc$complexity,
               row.names = NULL, stringsAsFactors = FALSE)
  structure(out, class = c("condTbl", "data.frame"))
  }

# extract msf from cna-object
asf <- function(x){
  stopifnot(inherits(x, "cna"))
  asf.list <- lapply(x$solution, "[[", "asf")
  if (all(vapply(asf.list, is.null, logical(1))))
    return(data.frame(outcome = character(0),
                      condition = character(0),
                      consistency = numeric(0),
                      coverage = numeric(0),
                      complexity = numeric(0),
                      stringsAsFactors = FALSE))
  all.asf <- do.call(rbind, asf.list)
  outcome <- rep(names(asf.list),
                 vapply(asf.list, function(fr) if(is.null(fr)) 0L else nrow(fr), integer(1)))
  out <-                
    data.frame(outcome = outcome,
               condition = paste(all.asf$condition, "<->", outcome),
               consistency = all.asf$consistency,
               coverage = all.asf$coverage,
               complexity = all.asf$complexity,
               row.names = NULL, stringsAsFactors = FALSE)
  structure(out, class = c("condTbl", "data.frame"))
  }

# Extract csf from cna object
csf <- function(x, n = 20, tt = x$truthTab, asfx = asf(x)){
  if (nrow(asfx) == 0)
    return(data.frame(condition = character(0), consistency = numeric(0),
           coverage = numeric(0)), complexity = integer(0), coherence = numeric(0), 
           stringsAsFactors = FALSE)
  splitasf <- split(asfx, asfx$outcome)
  n.csf <- prod(vapply(splitasf, nrow, integer(1)))
  head_with_ties <- function(x, n) {
    if (nrow(x) <= n) {
      n <- nrow(x)
    } else {
      notDup <- which(!duplicated(x[c("consistency", "coverage", "complexity")]))
      if (all(notDup <= n)){
        n <- nrow(x)
      } else {
        n <- min(notDup[notDup > n]) - 1L
      }
    }
    x[seq_len(n), ]
  }
  splitasf <- lapply(splitasf, head_with_ties, n)
  rnms.grid <- do.call(expand.grid, c(lapply(splitasf, function(x) seq_len(nrow(x))),
    KEEP.OUT.ATTRS = FALSE))
  splitasf.expanded <- mapply(function(asf, gr) asf[gr, , drop = FALSE],
    splitasf, rnms.grid, SIMPLIFY = FALSE)
  X <- do.call(cbind, splitasf.expanded)
  csfCon <- apply(X[grepl("\\.consistency$", names(X))], 1, min)
  csfCov <- apply(X[grepl("\\.coverage$", names(X))], 1, min)
  csfCompl <- apply(X[grepl("\\.complexity$", names(X))], 1, sum)
  # select top N here...!
  if (n < n.csf){
    message("Only ", n, " of the ", n.csf, " complex solutions (csf) have been calculated.")
  }
  n <- min(n, nrow(X))
  csfOrder <- order(csfCompl, -csfCon * csfCov)[seq_len(n)]
  X <- X[csfOrder, , drop = FALSE]
  csfOutc <- do.call(paste, c(X[grepl("\\.outcome$", names(X))], list(sep = ",")))
  conds <- X[grepl("\\.condition$", names(X))]
  csfName <- unname(apply(conds, 1, csf_representation))
  csfCoh <- vapply(strsplit(csfName, ","),
                   function(x) .min(vapply(x, coherence, numeric(1), tt = tt)),
                   numeric(1), USE.NAMES = FALSE)
  out <- data.frame(outcome = csfOutc, condition = csfName, consistency = csfCon[csfOrder],
    coverage = csfCov[csfOrder], complexity = csfCompl[csfOrder], coherence = csfCoh,
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  class(out) <- c("condTbl", "data.frame")
  out
}

# print method for class condTbl
print.condTbl <- function(x, digits = 3, ...) print.data.frame(x = x, digits = digits, ...)
  
getOutcome <- function(x) vapply(strsplit(x, "-> ", fixed = TRUE),
                                 function(x) x[[2]],
                                 character(1))

# as.condTbl: builds a "condTbl"-object from a list of "cond"-objects 
as.condTbl <- function (condlst, ...){
  stopifnot(is.list(condlst), all(vapply(condlst, inherits, logical(1), "cond")))
  outcome <- character(length(condlst))
  condType <- vapply(condlst, function(x) class(x)[[1]], character(1), USE.NAMES = FALSE)
  if (any(condType == "booleanCond")) 
    outcome[condType == "booleanCond"] <- "(No outcome)"
  if (any(condType == "atomicCond")) 
    outcome[condType == "atomicCond"] <- getOutcome(names(condlst[condType == "atomicCond"]))
  if (any(condType == "complexCond")) 
    outcome[condType == "complexCond"] <- vapply(
      condlst[condType == "complexCond"], 
      function(x) paste(getOutcome(names(x)), collapse = ","),
      character(1), USE.NAMES = FALSE)
  out <- data.frame(outcome = outcome, condition = names(condlst), stringsAsFactors = FALSE)
  if (any(!is.na(con <- getAttribute(condlst, "consistency", use.names = FALSE))))
  out$consistency <- con
  if (any(!is.na(cov <- getAttribute(condlst, "coverage", use.names = FALSE))))
    out$coverage <- cov
  if (any(!is.na(f <- getAttribute(condlst, "freq", use.names = FALSE))))
    out$rel.freq <- f
  class(out) <- c("condTbl", "data.frame")
  out
}


# "Quick" version of condition+as.conTbl
#  -> used internally in cna
#  -> desgined for 'atomic' conditions
quickCondTbl <- function(conds, zname, tt){
  complete.conds <- paste(conds, "<->", zname)
  px <- visible2parsed(complete.conds)
  negateCase <- attr(tt, "type") %in% c("cs", "fs")
  cc <- vapply(atomicCond(px, tt = tt, negateCase = negateCase), 
               function(x) unlist(getInfo(x)[2:3]),
               numeric(2))
  structure(data.frame(outcome = zname, condition = conds, t.default(cc), stringsAsFactors = FALSE),
            class = c("condTbl", "data.frame"))
}


# getAttribute: 
# auxiliary function to extract attributes from a list of "cond"-objects
getAttribute <- function(condlst, attrName, use.names = TRUE){
  val <- lapply(condlst, attr, attrName)
  val[vapply(val, is.null, logical(1))] <- NA
  if (!use.names) val <- unname(val)
  unlist(val)
  }
  
