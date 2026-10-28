
#' fm_structural(): structural functional minimality.
#' Returns logical vector of same length as cond.
#' Result only depends on structure of the individual cond 
fm_structural <- function(cond, x = full.ct(cond), verbose = FALSE){
  if (length(cond) == 0) return(logical(0))
  cond <- noblanks(cond)
  if (!inherits(x, "cti")){
    if (!missing(x)) x <- auxConfigTable(cond, x)
    if (!inherits(x, "configTable")) x <- configTable(x)
    x <- ctInfo(x)
  }
  checkStdCondTypes(cond, x, functionName = "fm_structural")
  if (useCtiList(x)) x <- ctiList(x, cond)
  .fm_struct(x, cond = cond, verbose = verbose)
}

.fm_struct <- function(x, cond, ...){
  UseMethod(".fm_struct")
}

.fm_struct.cti <- function(x, cond, verbose = FALSE, ...){
  x <- full.ct(x)
  asfs <- extract_asf(cond)
  factGrps <- .createFactorGroups(unlist(asfs), outcomes = TRUE)
  evalCond <- qcond_csf(cond, x, force.bool = TRUE)
  mode(evalCond) <- "logical"
  lhsInd <- C_relist_Int(factGrps$indices, lengths(asfs))
  rhsVars <- C_relist_Char(factGrps$outcomes, lengths(asfs))
  tbl <- as.data.frame(x$scores)
  if (x$type == "mv"){
    tbl <- data.frame(tbl, cti2df(x), check.names = FALSE)
  }
  out <- rep(NA, length(cond))
  n_msgs <- if (verbose <= 0){
    0L
  } else if (isTRUE(verbose)){
    5L
  } else {
    round(verbose)
  }
  verbose <- n_msgs > 0
  count_msgs <- 0L
  for (i in seq_along(cond)){
    rowSelect <- evalCond[, i]
    out[[i]] <- .fm_struct_1cond(cond[[i]], factGrps$nms[lhsInd[[i]]], 
                                 rhsVars[[i]], 
                                 lapply(tbl, "[", rowSelect), 
                                 verbose = verbose)
    if (verbose && !out[[i]]) count_msgs <- count_msgs + 1L
    if (count_msgs >= n_msgs) verbose <- FALSE
  }
  if (verbose && sum(!out) > n_msgs){
    message("| This information is printed for the first ", n_msgs, 
            " of ", sum(!out), " functionally non-minimal formulas;\n",  
            "| Specify verbose=<n> to see the information for <n> formulas.")
  }
  names(out) <- cond
  out
}

.fm_struct_1cond <- function(cond, lhsVars, outcomes, tbl, verbose = FALSE){
  fm_structural <- TRUE
  for (i in seq_along(lhsVars)){
    for (j in seq_along(lhsVars[[i]])){
      if (length(lhsVars[[i]]) > 1){
        spl <- split.default(tbl[[outcomes[[i]]]], 
                             .int(tbl[lhsVars[[i]][-j]]), 
                             drop = TRUE)
      } else {
        spl <- list(tbl[[outcomes[[i]]]])
      }
      brk <- all(sapply(spl, isConstant))
      if (brk){
        if (verbose)
          message("Factor ", lhsVars[[c(i, j)]], " is functionally redundant in asf ",
                  i, " of ", cond)
          return(FALSE)
      }
    }
  }
  fm_structural
}
# quick calculation of an interaction between integer vectors
# avoids usage of the slow base::interaction() in split.default()
.int <- function(f){
  if (is.atomic(f)){
    f
  } else if (length(f) == 1){
    f[[1]]
  } else if (length(f) == 2){
    (max(f[[1]]) + 1) * f[[2]] + f[[1]]
  } else {
    .int(list(f[[1]], .int(f[-1])))
  }
}

# ------------------------------------------------------------------------------

#' fm_empirical(): empirical functional minimality.
#' Returns logical vector of same length as cond.
#' Result depends on complete set cond 
fm_empirical <- function(cond, verbose = FALSE){
  if (length(cond) == 0) return(logical(0))
  cond <- noblanks(cond)
  cti <- ctInfo(auxConfigTable(cond))  # required for checking cond type
  checkStdCondTypes(cond, cti, functionName = "fm_empirical")
  .fm_emp(cond, verbose = verbose)
}

# "Internal" version of fm_empirical without the intitial formal checks:
.fm_emp <- function(cond, verbose = FALSE){
  # Group by set of outcomes
  splCond <- extract_asf(cond)
  uSplCond <- unlist(splCond, recursive = FALSE, use.names = FALSE)
  .cnds <- rhs(uSplCond)
  # sort outcomes within csf
  ll <- lengths(splCond)
  .cnds[] <- .cnds[order(rep(seq_along(ll), ll), .cnds)]
  outcomeGrps <- C_mconcat(C_relist_Char(.cnds, ll), sep = "|")
  grps <- match(outcomeGrps, uGrps <- unique(outcomeGrps))
  n_msgs <- if (verbose <= 0){
    0L
  } else if (isTRUE(verbose)){
    5L
  } else {
    round(verbose)
  }
  verbose <- n_msgs > 0

  # Apply .fm_emp_1() separately by set of outcomes
  out <- rep(NA, length(cond))
  reducedForms <- character(length(cond))
  nRed <- integer(length(cond))
  for (i in seq_along(uGrps)){
    fmList <- .fm_emp_1(cond[grps == i], verbose = verbose)
    out[grps == i] <- fmList$fm
    if (verbose){
      reducedForms[grps == i] <- fmList$red1
      nRed[grps == i] <- fmList$nred
    }
  }
  # messages if verbose=T
  if (verbose){
    ii <- head(which(!out), n_msgs)
    for (i in ii){
      msg <- paste0(cond[i], " is not fm_empirical because of\n",
                    "  ", reducedForms[[i]])
      if (nRed[[i]] > 1){
        msg <- paste0(msg, "\n  and ", nRed[[i]] - 1, 
                      " further formula", if (nRed[[i]] > 2) "s", ".")
      }
      message(msg)
    }
  }
  if (verbose && sum(!out) > n_msgs){
    message("| This information is printed for the first ", n_msgs,
            " of ", sum(!out), " functionally non-minimal formulas;\n",
            "| Specify verbose=<n> to see the information for <n> formulas.")
  }
  names(out) <- cond
  out
}

# .fm_emp_1(): Works for a set of conds with identical set of outcomes
.fm_emp_1 <- function(cond, verbose){
  cond <- as.vector(cond)
  l <- length(cond)
  asfs <- extract_asf(cond)
  n_asf <- length(asfs[[1]])
  stopifnot(lengths(asfs) == n_asf)

  out <- matrix(TRUE, l, l)
  matList <- vector("list", n_asf)
    
  for (i in seq_len(n_asf)){
    a <- vapply(asfs, "[[", i, FUN.VALUE = character(1))
    ma <- match(a, ua <- unique(a))
    lhf <- lhsFactors(ua)
    lhfstring <- C_mconcat(lhf, "\r")
    mlhf <- match(lhfstring, unique(lhfstring))
    ulhf <- lhf[!duplicated(mlhf)]
    ss <- findSubsets(ulhf)
    matList[[i]] <- ss[mlhf, mlhf, drop = FALSE][ma, ma, drop = FALSE]
  }
  tbl <- do.call(cbind, lapply(matList, as.vector))
  fmMat <- rowAnyNAs(tbl) | !rowAnys(tbl)
  dim(fmMat) <- c(l, l)
  fm <- colAlls(fmMat)
  if (verbose){
    # number of reduced formulas and first red. formula
    nred <- colSums(!fmMat)
    red1 <- cond[apply(!fmMat & fm, 2, match, x =TRUE)]
  }
  names(fm) <- cond
  out <- list(fm = fm)
  if (verbose){
    out$red1 <- red1
    out$nred <- nred
  }
  out
}

# findSubets():
# In a list of vectors, find all strict subset-superset relationships
# Requirements: 
# * x is list of _unique_ and _sorted_ atomic vectors of identical type
# Returns a logical matrix with entries:
#'   TRUE  i is subset of j
#'   FALSE i and j are equal
#'   NA    none of the above
# findSubsets(list(1:3, 1:2, 2:4, 4, 5))
findSubsets <- function(x){
  ll <- lengths(x)
  # convert to int
  ux <- unlist(x, use.names = FALSE)
  mm <- match(ux, unique(ux))
  mm[] <- mm[order(rep(seq_along(x), ll), mm)]
  x[] <- C_relist_Int(mm, ll)
  
  # Find potential subset-superset relationships (considering lengths only):
  shorter <- outer(ll, ll, "<")
  # check candidates one by one:
  ss <- matrix(NA, length(x), length(x))
  rr <- row(shorter)
  cc <- col(shorter)
  for (i in which(shorter)){
    if (C_isSubsetOf(x[[rr[[i]]]], x[[cc[[i]]]], strict = TRUE))
      ss[[i]] <- TRUE
  }
  diag(ss) <- FALSE
  ss
}

# ------------------------------------------------------------------------------

# Auxiliary function lhsFactors(): extracts lhs factors from cond
#   Takes a character vector of conditions (cs or mv in std form)
#   Returns a list of char vectors, 
#   the enumeration of factors appearing on the lhs for each condition
lhsFactors <- function(cond){
  gr <- .createFactorGroups(cond, outcomes = TRUE)
  gr$nms[gr$indices]
}

# Auxiliary function: finds minimal subsets
#   Takes a list of char vectors, returns the minimal sets, 
#   i.e. those that do not contain a shorter vector from the list.
#   Second arg u is optional, introduced to avoid having to do twice 
#   the same calculation in funcRed().
minimalSets <- function(x, u = unname(unique(x[order(lengths(x))]))){
  allFacts <- unique(sort(unlist(u, use.names = FALSE)))
  # convert to integer in order to use fast cna::C_isSubsetOf() below:
  u <- lapply(u, match, table = allFacts, nomatch = 0L)
  minLen <- min(lengths(u))
  out <- u
  ll <- lengths(u)
  for (i in which(ll > minLen)){
    jj <- which(ll < ll[[i]])
    isSubset <- sapply(u[jj], C_isSubsetOf, y = u[[i]], strict = FALSE)
    hasSubset <- any(isSubset)
    if (hasSubset) out[i] <- list(NULL)
  }
  out <- out[lengths(out)>0]
  out <- unname(split(allFacts[unlist(out)], 
                      rep(seq_along(out), lengths(out))))
  
  out
}

