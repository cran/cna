
# fs2cs: 
# ======
# transforms a fs-ct into a sc-ct
# Note: fs2cs is designed to work with a data.frame, not a configTable!
fs2cs <- function(x, cutoff = 0.5, border = c("up", "down", "drop")){
  if (cutoff <= 0 | cutoff >= 1)
    stop("cutoff must be >0 and <1")
  border <- match.arg(border)
  if (border == "drop") {
    removeLines <- !rowAnys(x == cutoff)
    x <- x[removeLines, , drop = FALSE]
  }
  xout <- if (inherits(x, "configTable")) as.data.frame(x, warn = FALSE) else x
  xout[] <- rep(0L, nrow(x))
  if (border == "up"){
    xout[x >= cutoff] <- 1L
  } else {
    xout[x > cutoff] <- 1L
  }
  x[] <- xout
  x
}

# ctInfo: 
# ========
# Auxiliary function that extracts useful informations from a configTable
ctInfo <- function(ct, cutoff = 0.5, border = c("down", "up", "drop")){
  stopifnot(inherits(ct, "configTable"))
  type <- attr(ct, "type")
  border <- match.arg(border)
  ctmat <- as.matrix(ct)
  
  if (type == "cs") {
    uniqueValues <- lapply(ct, function(x) 1:0)
    resp_nms <- names(ct)
    valueId <- 2 - ctmat
  } else if (type == "mv") {
    uniqueValues <- lapply(ct, function(x) sort(unique.default(x)))
    resp_nms <- mapply(paste, names(ct), uniqueValues, MoreArgs = list(sep = "="), 
                       SIMPLIFY = FALSE)
    resp_nms <- unlist(resp_nms, use.names = FALSE)
    valueId <- mapply(match, ct, uniqueValues, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    if (!is.matrix(valueId))
      valueId <- matrix(valueId, nrow = nrow(ct), 
                        dimnames = list(NULL, names(uniqueValues)))
  } else if (type == "fs") {
    ct.r <- fs2cs(ctmat, cutoff = cutoff, border = border)
    rownames(ct.r) <- NULL
    resp_nms <- names(ct)
    uniqueValues <- sapply(resp_nms, function(x) 1:0, simplify = FALSE)
    valueId <- 2 - ct.r
  }
  stopifnot(resp_nms == toupper(resp_nms))
  nVal <- lengths(uniqueValues)
  valueId <- sweep(valueId, 2, 
                   c(if (length(nVal)) 0, cumsum(nVal[-length(nVal)])), 
                   "+")
  stopifnot(valueId%%1 == 0)
  mode(valueId) <- "integer"
  config <- split(seq_len(sum(nVal)), 
                  rep(seq_along(nVal), nVal))
  names(config) <- names(ct)
  # 'scores' matrix
  if (prod(dim(ct)) == 0){
    scores <- matrix(numeric(0), 0, 0)
  } else {
    if (type %in% c("cs", "mv")){
      scores <- do.call(cbind, 
                        mapply(outer, asplit(valueId, 2), config, "==", 
                               SIMPLIFY = FALSE))
      mode(scores) <- "integer"
    } else { # type=="fs"
      scores <- rbind(ctmat, 1-ctmat)
      dim(scores) <- c(nrow(ctmat), 2*ncol(ctmat))
    }
    colnames(scores) <- switch(type, 
      mv = resp_nms,
      as.vector(rbind(names(ct), tolower(names(ct)))))
  }
  mode(scores) <- "numeric"
  freq <- attr(ct, "n")
  structure(mget(c("type", "resp_nms", "nVal", "uniqueValues", "config", 
                   "valueId", "scores", "freq")),
            class = c("cti", "tti"))
}

# subset method for class "cti"
subset.cti <- function(x, i, ...){
  x$valueId <- x$valueId[i, , drop = FALSE]
  x$scores <- x$scores[i, , drop = FALSE]
  x$freq <- x$freq[i]
  x
}

# old function name (for compatibility of material outside the package)
tt.info <- ctInfo

# function to build 'scores' matrix
# --- reintroduced because of cnaOpt dependence!
factMat <- function(type){
  switch(type, 
    cs =  function(x, ct = tt, tt){
      structure(cbind(ct[[x]], 1 - ct[[x]]), 
                .Dimnames = list(NULL, c(x, tolower(x))))},
    mv = function(x, ct = tt, tt){
      v <- eval(parse(text = sub("=", "==", x, fixed = TRUE), keep.source = FALSE), ct)
      matrix(as.integer(v), dimnames = list(NULL, x))},
    fs = factMat <- function(x, ct = tt, tt){
      structure(cbind(ct[[x]], 1 - ct[[x]]), 
                .Dimnames = list(NULL, c(x, tolower(x))))}
  )
}

# function check.ordering
# =======================

check.ordering <- function (ordering, cti){
  if (is.null(ordering)) return(NULL)
  if (is.character(ordering) && length(ordering) == 1L)
    ordering <- ordering_from_string(ordering)
  # check ordering: object structure
  if (!is.list(ordering) || 
      !all(vapply(ordering, is.character, logical(1)) | 
           (hasFactor <- vapply(ordering, is.factor, logical(1)))
           )){
    stop("ordering must be either NULL or a list of character vectors or a character string.")
  }
  if (any(hasFactor)){
    ordering[hasFactor] <- lapply(ordering[hasFactor], as.character)
  }
  ordering <- happly(ordering, toupper)
  orderingUnlisted <- unlist(ordering, use.names = FALSE, recursive = FALSE)
  # check ordering: names in config table
  validEntries <- names(cti$nVal)
  if (!all(orderingUnlisted %in% validEntries)){
    stop("Factor listed in ordering does not exist in configTable: ", 
         paste0(setdiff(orderingUnlisted, validEntries), collapse = ","))
  }
  # check ordering: no duplicates
  if (anyDuplicated(orderingUnlisted))
    stop("At least one factor appears twice in the ordering.")
  # Complete ordering
  expand.ordering(ordering, cti)
}  
expand.ordering <- function(ordering, cti){
  orderingUnlisted <- unlist(ordering)
  nms <- names(cti$nVal)
  if (!all(nms %in% orderingUnlisted)){
    ordering <- c(list(setdiff(nms, orderingUnlisted)), 
                  ordering)
  }
  ordering
}
ordering_from_string <- function(x){
  hstrsplit(noblanks(x), split = c("<", ","))[[1]]
}


# potential.effects:
# ==================
potential.effects <- function(cti, zname, ordering = NULL, strict = FALSE){
  if (cti$type == "mv") zname <- sub("=.+$", "", zname)
  nms <- names(cti$nVal)
  poteff0 <- setdiff(nms, zname)
  if (is.null(ordering)) return(poteff0)
  ordLevelZ <- which(vapply(ordering, function(x) any(zname %in% x), logical(1)))
  poteff <- unlist(ordering[seq_len(ordLevelZ - strict)], use.names = FALSE)
  intersect(poteff0, poteff)  # order taken from poteff0
}



# intList, intList2:
# ------------------
# An "intList" is a representation of a formula as a list of integers,
# where the integers refer to colnames in scores
# Example: If the colnames in scores are c("A", "a", "B", "b", "C", "c") then
#   "A+b*C" ist then represented as list(1L, c(4L, 5L))
# An "recIntList" is a list of "intList" objects.
is.intList <- function(x){
  is.list(x) && all(vapply(x, is.integer, logical(1)))
}
is.recIntList <- function(x) is.list(x) && all(vapply(x, is.intList, integer(1)))

# All possible structures of asf 
# maxstep integer of length 3:
#   element 1: maximum number of terms in conjunctions
#   element 2: maximum number of conjunctions in disjunction
#   element 3: maximum total number of terms
# examples: 
# struct(c(2, 3, 5))
# struct(c(3, 2, 6))
allStructs <- function(maxstep){
  maxCompl <- maxstep[[3]]
  if (maxCompl == 1) return(list(1L))
  maxnum <- maxstep[[1]]
  maxlen <- maxstep[[2]]
  one.less <- allStructs(c(maxnum, maxlen, maxCompl - 1L))
  out <- c(list(list(rep(1L, maxCompl))),
           lapply(one.less, add12each))
  out <- unlist(out, recursive = FALSE)
  out <- out[vapply(out, max, integer(1)) <= maxnum]
  out <- out[lengths(out) <= maxlen]
  out <- lapply(out, sort)
  unique.default(c(one.less, out))
}
# auxiliary function
add12each <- function(x)
  mapply(function(l, i){
    l[i] <- l[i] + 1L
    l
  },
  rep(list(x), length(x)),
  seq_along(x),
  SIMPLIFY = FALSE)


# functions related to 'exclude'
resolveExclude <- function(excl, outcomes, notcols, fVals){
  if (!is.character(excl) && !is.null(excl)){
    stop("'exclude' must be NULL or a character vector.")
  }
  if (!length(excl)) return(excl)
  excl <- noblanks(excl)
  # output before further adjustments
  out <- structure(strsplit(lhs(excl), ",", fixed = TRUE), 
                   names = rhs(excl))
  # check input
  nms <- sub("*", "", names(out), fixed = TRUE)
  unlisted <- unlist(out, use.names = FALSE)
  values <- sub("*", "", unlisted, fixed = TRUE)
  validValues <- c(names(fVals), unlist(fVals, use.names = FALSE))
  ok_nms <- nms %in% validValues
  ok_vals <- values %in% validValues
  if (!all(ok_nms, ok_vals)){
    ok_up <- toupper(c(nms, values)) %in% validValues
    if (!all(ok_up)) 
      stop("'exclude' is invalid", 
           " because it specifies factor values not contained in the data", 
           " or is syntatically incorrect.")
    # record values that need to be adjusted in exclude
    # (strings with mixed lower/uppercase
  }
  # adjust case according to adjValues
  if (!all(ok_nms)){
    adjNms <- nms[!ok_nms]
    names(out) <- adjust2upper(adjNms, names(out))
  }
  adjValues <- values[!ok_vals]
  adjElement <- values %in% adjValues
  if (any(adjElement)){
    unlisted[adjElement] <- adjust2upper(adjValues, unlisted[adjElement])
    out[] <- C_relist_Char(unlisted, lengths(out))
  }
  # adjust outcomes acc. to notcols (only binary data)
  negated_outcome <- outcomes %in% toupper(notcols)
  outcomes[negated_outcome] <- tolower(outcomes[negated_outcome])
  mv_data <- any(grepl("=", unlist(fVals, use.names = FALSE), fixed = TRUE))
  if (mv_data){
    # factor names in mv case -> add star
    # - rhs
    factNm <- toupper(names(out)) %in% sub("=[0-9+]$", "", outcomes)
    names(out)[factNm] <- paste0(toupper(names(out)[factNm]), "*")
    # - lhs
    factNm <- toupper(unlist(out)) %in% sub("=[0-9+]$", "", names(fVals))
    unlisted <- unlist(out, use.names = FALSE)
    unlisted[factNm] <- paste0(toupper(unlisted[factNm]), "*")
    out[] <- C_relist_Char(unlisted, lengths(out))
  }
  starred_outcome <- grepl("*", names(out), fixed = TRUE)
  if (any(starred_outcome)){
    st_outc <- toupper(sub("*", "", names(out)[starred_outcome], fixed = TRUE))
    st_outc <- intersect(st_outc, names(fVals))
    stVals <- fVals[st_outc]
    out <- c(
      structure(rep(out[starred_outcome], lengths(stVals)), 
                names = unlist(fVals[st_outc], use.names = FALSE)),
      out[!starred_outcome])
  }
  if (anyDuplicated(names(out))>0){
    out <- split.default(unlist(out, use.names = FALSE), 
                         rep(names(out), lengths(out)))
  }
  out <- out[intersect(outcomes, names(out))]
  # resolve "star"-notation on lhs
  if (any(has_star <- grepl("*", unlist(out, use.names = FALSE), fixed = TRUE))){
    has_star <- structure(C_relist_Log(has_star, lengths(out)), 
                          names = names(out))
    for (zname in names(out)[m_any(has_star)]){
      has_star1 <- has_star[[zname]]
      factnm <- toupper(sub("*", "", out[[zname]][has_star1], fixed = TRUE))
      if (any(factnm %in% names(fVals))){
        out[[zname]] <- unique(c(unlist(fVals[factnm], use.names = FALSE), 
                                 out[[zname]][!has_star1]))
      }
    }
  }
  out
}

factorValues <- function(cti){
  structure(
    C_relist_Char(colnames(cti$scores), cti$nVal), 
    names = names(cti$nVal))
}
# factorValues(ctInfo(configTable(d.educate)))
# factorValues(ctInfo(configTable(d.pban))) -> mv case not yet properly resolved!!

# aux fun adjust2upper:
#   patterns  value strings to be replaced by uppercase chars
#   x         vector to be adjusted
# Returns a adjusted vefsion of x  
adjust2upper <- function(patterns, x){
  x_nostar <- sub("*", "", x, fixed = TRUE)
  doAdj <- rowAnys(
    matrix(vapply(patterns, 
                  function(pa) match(x_nostar, table = pa, nomatch = 0) > 0, 
                  FUN.VALUE = logical(length(x_nostar))), 
           nrow = length(x_nostar)))
  x[doAdj] <- toupper(x[doAdj])
  x
}
# adjust2upper(c("Abc", "bCd"), c("Abc*", "Abcd", "xyz"))
