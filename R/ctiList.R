
# x must is a cti object
# cond a (general) cond
ctiList <- function(x, cond){
  cond <- noblanks(cond)
  condListed <- extract_asf(cond)
  csfLengths <- lengths(condListed)
  ff <- hstrsplit(condListed, c("<*->", "+", "*"), 
                  relist = FALSE, fixed = c(FALSE, TRUE, TRUE))
  if (x$type == "mv") ff[] <- sub("=[0-9]+", "", as.vector(ff))
  ll <- as.vector(rowsum(collapse(attr(ff, "lengths"), 2:3)[[1]], 
                         rep(seq_along(csfLengths), csfLengths)))
  nmsList <- C_relist_Char(toupper(as.vector(ff)), ll)
  nmsList <- lapply(nmsList, function(x) sort.default(unique.default(x)))
  nmsStrings <- C_mconcat(nmsList, "\r")
  uNmsStrings <- unique(nmsStrings)
  uNmsList <- strsplit(uNmsStrings, "\r", fixed = TRUE)
  if (!all(unique(unlist(uNmsList)) %in% names(x$nVal))){  
    uNmsList[] <- lapply(uNmsList, intersect, names(x$nVal))
  }
  indices <- match(nmsStrings, uNmsStrings)
  tbls <- lapply(uNmsList, function(nms) selectFactors(x, nms))
  structure(c(
    x[c("type", "resp_nms", "nVal", "uniqueValues")],    
    list(scores = x$scores[0, , drop = FALSE],
         conds = cond, 
         tbls = tbls, 
         indices = indices)), 
    class = "ctiList")
}

# decide whether full.ct() is used separately by variable set in cond
useCtiList <- function(cti, maxnrow = getOption("ctiListLimit")){
  sepflag <- getOption("useCtiList")
  if (sepflag %in% c(TRUE, FALSE))
    return(sepflag)
  prod(cti$nVal) > maxnrow
}

# factor selection in cti
selectFactors <- function(x, nms){
  if (setequal(names(x$nVal), nms)) return(x)
  innms <- names(x$nVal)
  x$nVal <- x$nVal[nms]
  x$uniqueValues <- x$uniqueValues[nms]
  x$config <- x$config[nms]
  keepValues <- unique(unlist(x$config, use.names = FALSE))
  x$resp_nms <- x$resp_nms[if (x$type == "mv") keepValues else match(nms, innms)]
  x$config[] <- C_relist_Int(seq_along(keepValues), x$nVal)
  x$valueId <- x$valueId[, nms, drop = FALSE]
  x$valueId[] <- match(x$valueId, keepValues)
  rownames(x$valueId) <- NULL
  x$scores <- x$scores[, keepValues, drop = FALSE]
  
  if (anyDuplicated(x$scores)>0){
    rows <- do.call(paste, c(asplit(x$scores, 2), list(sep = "\r")))
    urows <- unique(rows)
    rowId <- match(rows, unique(rows))
    urows <- match(seq_along(urows), rowId)
    x$valueId <- x$valueId[urows, , drop = FALSE]
    x$scores <- x$scores[urows, , drop = FALSE]
    x$freq <- as.vector(rowsum(x$freq, rowId))
  }
  x
}

# getPos: 
getPos <- function(x){
  out <- integer(length(x$conds))
  b <- 0
  for (i in seq_along(x$tbls)){
    pos <- which(x$indices == i)
    out[pos] <- b + seq_along(pos)
    b <- b + length(pos)
  }
  out
}

# Select a subset of conds in a ctiList
`[.ctiList` <- function(x, i){
  out <- x
  out$conds <- x$conds[i]
  out$indices <- x$indices[i]
  uInd <- unique(out$indices)
  out$tbls <- out$tbls[uInd]
  out$indices <- match(out$indices, uInd)
  out  
}

full.ct.ctiList <- function(x, ...){
  x$tbls[] <- lapply(x$tbls, full.ct.cti)
  x$freq <- prod(x$nVal)
  x
}


.det.ctiList <- function(x, cond = x$conds, 
    what = c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence", "redundant"), 
    available = what, cycle.type = "factor", in.csf = FALSE, ...){
  out <- mapply(.det.cti, 
    x = x$tbls, cond = split(x$conds, x$indices), 
    MoreArgs = list(what = what, available = available, cycle.type = cycle.type, 
                    in.csf = in.csf),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- do.call(rbind, out)
  out[getPos(x), , drop = FALSE]
}


.inus.ctiList <- function(x, cond, qtypes, full = FALSE, 
                          const.ok = FALSE, csf.info = FALSE, ...){
  out0 <- mapply(.inus.cti, 
    x = x$tbls, cond = split(x$conds, x$indices), 
    qtypes = split(qtypes, x$indices), 
    MoreArgs = list(full = full, const.ok = const.ok, csf.info = csf.info),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- do.call(c, out0)
  out <- out[getPos(x)]
  if (csf.info){
    csf.info <- do.call(rbind, lapply(out0, attr, "csf.info"))
    csf.info <- csf.info[getPos(x), , drop = FALSE]
    attr(out, "csf.info") <- csf.info
  }
  out
}

.redund.ctiList <- function(x, cond, simplify = TRUE, full = FALSE, names = TRUE, 
                            ...){
  out <- mapply(.redund.cti, x = x$tbls, cond = split(x$conds, x$indices), 
    MoreArgs = list(simplify = FALSE, full = full, names = names),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- unlist(out, recursive = FALSE)[getPos(x)]
  if (simplify && length(ul <- unique(lengths(out, use.names = FALSE))) == 1L){
    nms <- names(out)
    out <- matrix(unlist(out, use.names = FALSE), ncol = ul, byrow = TRUE)
    if (names) rownames(out) <- nms
  }
  out
}

exff.ctiList <- function(x, cond, num = TRUE, names = TRUE, 
    cti.full = full.ct(x, cond = cond), 
    ...){
  cti <- lapply(x$tbls, unique_cti_cs)
  out <- mapply(calcExff, 
                cti, cti.full = cti.full$tbls, cond = split(x$conds, x$indices), 
                MoreArgs = list(num = num, names = names),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- do.call(rbind, out)
  out[getPos(x), , drop = FALSE]
}


.minCsf.ctiList <- function(x, cond = x$conds, verbose = FALSE, ...){
  # x ist expected to be "full"!!
  rr <- .redund.ctiList(x, cond, full = TRUE, simplify = FALSE)
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
  condNewOrigin <- which(anyRed)[attr(condNew, "orig")]
  dups <- duplicated(condNew)
  if (verbose) cat("(", sum(!dups), " uniques)\n", sep = "")
  x$conds <- condNew[!dups]
  x$indices <- x$indices[condNewOrigin][!dups]
  if (!all(seq_along(x$tbls) %in% x$indices)){
    used <- intersect(seq_along(x$tbls), x$indices)
    x$tbls <- x$tbls[used]
    x$indices <- match(x$indices, used)
  }
  recurs <- .minCsf.ctiList(x, verbose = verbose)
  c(recurs, noRed)
}


