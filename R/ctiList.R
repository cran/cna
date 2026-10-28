
# x must is a cti object

# Purpose of ctiList:
# Allow computing lighter 'full.cti'.
# A ctiList is always related to a set of conditions;
# every tbls-component of ctiList contains only the factors
# that are present in the respective conds.
# 
# A simple example where it is used:
# cond <- replicate(10, randomAsf(sample(LETTERS, 6)))
# is.inus(cond) # <- calls ctiList() in .inus.configTable()

# cond a (general) cond
ctiList <- function(x, cond){
  cond <- noblanks(cond)
  factGrps <- .createFactorGroups(cond)
  tbls <- lapply(factGrps$nms, function(nms) selectFactors(x, nms))
  structure(c(
    x[c("type", "resp_nms", "nVal", "uniqueValues", 
        if (x$type == "fs") "fsInfo")],
    list(scores = x$scores[0, , drop = FALSE],
         conds = cond, 
         tbls = tbls, 
         indices = factGrps$indices)), 
    class = "ctiList")
}

# Create grouping of conds according to factors apprearing in them (includes lhs and rhs)
# Params
#   cond      character vect
#   x         a cti (optional, but required depending on context)
#   outcomes  Logical: If FALSE, rhs&lhs variables are returned indiscriminately
#             in output element 'nms'. If FALSE, 'nms'  contains only lhs factors
#             and rhs factors are included in 'outcomes'
#             !!IMPORTANT!! In current implementation, outcomes=TRUE requires that cond has only asfs!
#             
# Returns 
#   list(nms, indices) where nms ist a list of char vectors with factor names
#   additional elements 'outcomes' if requested.
#   
# Examples/Illustration:
# set.seed(3)
# cond <- replicate(3, randomCsf(6, compl = 2))
# cna:::.createFactorGroups(cond, ctInfo(full.ct(cond)))
# cna:::.createFactorGroups(unlist(extract_asf(cond)), 
#                           ctInfo(full.ct(cond)), outcomes = T)
#   
.createFactorGroups <- function(cond, x, outcomes = FALSE){
  
  condListed <- extract_asf(cond)
  csfLengths <- lengths(condListed)
  if (outcomes){
    stopifnot(csfLengths == 1)
    outFact <- rhs(unlist(condListed))
    condListed[] <- lhs(condListed)
  }
  ff <- hstrsplit(condListed, c("<*->", "+", "*"), 
                  relist = FALSE, fixed = c(FALSE, TRUE, TRUE))
  if (!missing(x)){
    if (x$type == "mv") ff[] <- sub("=[0-9]+", "", as.vector(ff))
    factNms <- names(x$nVal)
  } else {
    values <- sort(unique(ff))
    if (any(grepl("=", values))) ff[] <- sub("=[0-9]+", "", as.vector(ff))
    factNms <- unique(toupper(ff))
  }
  ll <- as.vector(rowsum(collapse(attr(ff, "lengths"), 2:3)[[1]], 
                         rep(seq_along(csfLengths), csfLengths)))
  nmsList <- C_relist_Char(toupper(as.vector(ff)), ll)
  nmsList <- lapply(nmsList, function(x) sort.default(unique.default(x)))
  nmsStrings <- C_mconcat(nmsList, "\r")
  uNmsStrings <- unique(nmsStrings)
  uNmsList <- strsplit(uNmsStrings, "\r", fixed = TRUE)
  if (!all(unique(unlist(uNmsList)) %in% factNms)){  
    uNmsList[] <- lapply(uNmsList, intersect, factNms)
  }
  indices <- match(nmsStrings, uNmsStrings)
  out <- list(nms = uNmsList, indices = indices)
  if (outcomes) out$outcomes <- outFact
  out
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


.det.ctiList <- function(x, cond = x$conds, what = TRUE, 
    cycle.type = "factor", in.csf = FALSE, ..., condType = NULL){
  if (length(condType) == length(x$indices)){
    argCondType <- split(condType, x$indices) 
  } else {
    argCondType <- vector("list", length(x$tbls))
  }
  out <- mapply(.det.cti, 
    x = x$tbls, cond = split(x$conds, x$indices), condType = argCondType,
    MoreArgs = list(what = what, cycle.type = cycle.type, in.csf = in.csf, ...),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (isTRUE(what)){
    what <- unique(unlist(nms <- lapply(out, names)))
    incomplete <- !vapply(nms, setequal, what, FUN.VALUE = logical(1))
    if (any(incomplete)){
      # Case what=TRUE and some partial conds contain only asfs, some contain csf
      # -> then the set of columns in partial output tables differ
      out[incomplete] <- lapply(out[incomplete], .appendDetCols, cols = what)
    }
  }
  out <- do.call(rbind, out)
  out[getPos(x), , drop = FALSE]
}
.appendDetCols <- function(x, cols){
  if (setequal(names(x), cols)) return(x[cols])
  newCols <- setdiff(cols, names(x))
  if ("coherence" %in% newCols) x$coherence <- 1
  if ("redundant" %in% newCols) x$redundant <- FALSE
  if (any(c("fm_structural", "fm_empirical") %in% newCols)) 
    x[intersect(c("fm_structural", "fm_empirical"), newCols)] <- NA
  x[cols]
}

.inus.ctiList <- function(x, cond, full = FALSE, qtypes, 
                          const.ok = FALSE, csf.info = FALSE, 
                          def = "implication", func.minim = FALSE, ...){
  out0 <- mapply(.inus.cti, 
    x = x$tbls, cond = split(x$conds, x$indices), 
    qtypes = split(qtypes, x$indices), 
    MoreArgs = list(full = full, const.ok = const.ok, csf.info = csf.info, 
                    def = def, func.minim = func.minim),
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

.exff.ctiList <- function(x, cond, num = TRUE, names = TRUE, 
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


.fm_struct.ctiList <- function(x, cond, verbose, ...){
  out <- mapply(.fm_struct.cti, 
    x = x$tbls, cond = split(x$conds, x$indices), 
    MoreArgs = list(verbose = verbose, ...),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- do.call(c, out)
  out[getPos(x)]
}

