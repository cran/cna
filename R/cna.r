
cna <- function (x, type,
    ordering = NULL, strict = FALSE,
    con = 1, cov = 1, con.msc = con, 
    notcols = NULL, rm.const.factors = TRUE, rm.dup.factors = TRUE,  
    maxstep = c(3, 3, 9), inus.only = FALSE,
    only.minimal.msc = TRUE, only.minimal.asf = TRUE, maxSol = 1e6, 
    suff.only = FALSE, what = if (suff.only) "m" else "ac",
    cutoff = 0.5, border = c("down", "up", "drop"),
    details = FALSE
    )
{

  # call and type
  cl <- match.call()
  if (!is.null(attr(x, "type"))) type <- attr(x, "type")
  if (missing(type)) type <- "cs"
  
  # variable names  
  tt <- truthTab(x, type = type, rm.const.factors = rm.const.factors, 
                 rm.dup.factors = rm.dup.factors)
  if (any(!grepl("[[:alpha:]]", names(tt)))) 
    stop("All column names must contain a letter.")
  if (anyDuplicated(tolower(names(tt))) != 0L)
    stop("Inadmissable column names: names must be unique when case is ignored!")
  names(tt) <- toupper(names(tt))

  # more formal requirements  
  if (nrow(tt) <= 1 || ncol(tt) <= 1)
    stop("Truth table must have at least two rows and two columns.")
  ordering <- check.ordering(ordering, tt)
  details <- details0 <- details1 <- clarify_details(details)
  if (inus.only){
    details <- union(details, c("inus", "redundant"))
  }
  details1 <- setdiff(details0, c("coherence", "redundant"))

  # notcols and tt.out
  if (!is.null(notcols)){
    if (type == "mv") stop("\"notcols\" not applicable if type==\"mv\"")
    if (length(notcols) == 1L && notcols == "all"){
      notcols <- names(tt) 
      if ("ALL" %in% names(tt)) 
        warning("'notcols=\"all\"' is ambiguous if a variable has name \"ALL\". ",
                "notcols is applied to _all_ variables.", call. = FALSE)
    }
    if (!is.character(notcols)) notcols <- names(tt)[notcols]
    notcols <- toupper(notcols)
    if (!all(notcols %in% names(tt))) stop("Wrong specification of 'notcols'")
    tt.out <- tt
    notcols.nrs <- names(tt) %in% notcols
    names(tt.out)[notcols.nrs] <- tolower(names(tt.out)[notcols.nrs])
    tt.out.df <- as.data.frame(tt.out)
    tt.out.df[notcols.nrs] <- lapply(tt.out.df[notcols.nrs], function(x) 1-x)
    attributes(tt.out.df)[c("names", "row.names", "class", "n", "cases", "type")] <- 
      attributes(tt.out)[c("names", "row.names", "class", "n", "cases", "type")]
    tt.out <- tt.out.df
  } else {
    tt.out <- tt
  }

  # Check con and cov values and reduce them slightly to avoid failing 
  # to find conditions due to rounding issues
  if (length(con) != 1 || con < 0 || con > 1 || 
      length(cov) != 1 || cov < 0 || cov > 1){
    stop("Invalid input for 'con' or 'cov'")
  }
  d.eps <- nrow(tt) * .Machine$double.eps
  con <- max(con - d.eps, 0)
  cov <- max(cov - d.eps, 0)

  # maxstep
  stopifnot(length(maxstep) == 3, maxstep > 0)
  maxstep[1:2] <- pmin(maxstep[1:2], maxstep[3])

  # Define config, uniqueValues, resp_nms, factMat, nVal, valueId, scores
  tti <- tt.info(tt, cutoff = cutoff, border = match.arg(border))
  vnms <- colnames(tti$scores)
  freqs <- attr(tt, "n")
  
  # setup output object
  sol <- vector("list", length(tti$resp_nms))
  names(sol) <- tti$resp_nms
  
  for (zname in tti$resp_nms){
  
    # Identify minimal sufficient conditions
    # --------------------------------------
    # Initialize minSuff, list of minimal sufficient combinations
    .znm <- sub("(.+)=.+", "\\1", zname)
    poteff <- potential.effects(tt, .znm, ordering, strict)
    if (length(poteff) == 0L) next
    
    y <- tti$scores[, zname, drop = TRUE]
    if (zname %in% notcols){
      y[] <- 1-y
      names(sol) <- sub(paste0("^", zname, "$"), tolower(zname), names(sol))
      zname <- tolower(zname)
    }  
    
    minSuff <- findMinSuff(tti, y, poteff, freqs, con.msc, 
                           maxstep, only.minimal.msc)
    if (all(m_is.null(minSuff))) next
    
    msc <- lapply(minSuff, make.msc, outcome = zname, tti = tti, details = details1)
    msc <- do.call(rbind, msc)
    nstars <- gregexpr("*", msc$condition, fixed = TRUE)
    msc$complexity <- lengths(nstars) + 1L - (vapply(nstars, "[[", integer(1), 1L) == -1L)
    msc <- msc[order(msc$complexity, -msc$consistency * msc$coverage, msc$condition), , drop = FALSE]
    rownames(msc) <- NULL
    sol[[zname]] <- list(msc = msc)

    if (suff.only) next

    # Find asf's
    # ----------
    .conjList <- lapply(minSuff, "attr<-", "conCov", NULL)
    noMsc <- m_is.null(.conjList)
    .conjList[noMsc] <- lapply(which(noMsc), function(i) matrix(integer(0), 0, i))

    .sol <- findAsf(tti, y, freqs, con, cov, .conjList, maxSol, maxstep, only.minimal.asf)
    stopifnot(length(.sol) == 0 || any(vapply(.sol, is.intList, logical(1))))
      
    if (length(.sol)){
      asf <- make.asf(tti, zname, .sol, inus.only, details)
      sol[[c(zname, "asf")]] <- asf
    }
  }

  out <- structure(list(), class = "cna")
  out$call <- cl
  out$x <- x
  out$ordering <- ordering
  out$truthTab <- tt
  out$truthTab_out <- tt.out
  out$solution <- sol
  out$what <- what
  out$details <- details0

  return(out)
}


findMinSuff <- function(tti, y, poteff, freqs, con.msc, maxstep, only.minimal.msc){
  nsteps <- min(length(poteff), maxstep[1L])
  minSuff <- vector("list", nsteps)
  
  for (.step in seq_along(minSuff)){
    # Select .step-fold conditions occurring in the data
    if (.step == 1L){
      allkfoldConds <- matrix(unlist(tti$config[poteff], use.names = FALSE),
                              ncol = 1)
    } else {
       allkfoldConds <- findAllSubsets(tti$valueId, .step, match(poteff, colnames(tti$valueId)))
    }                          
    # Eliminate combinations that contain a simpler combination that has been selected in an earlier step
    if (only.minimal.msc && .step >= 2L){
      for (k_ in seq_len(.step-1L))
        if (!is.null(minSuff[[k_]]))
          allkfoldConds <- allkfoldConds[!hasSubsetInM(allkfoldConds, minSuff[[k_]]), , drop = FALSE]
      rm(k_)
    }
    if (nrow(allkfoldConds) == 0) break
    stopifnot(anyDuplicated(allkfoldConds) == 0L)
    # Select sufficient conditions and add them to output list
    cons <- conj_conCov(allkfoldConds, tti$scores, y, freqs)
  if (any(isSuff <- cons[1, ] >= con.msc, na.rm = TRUE)){
      isSuff[is.na(isSuff)] <- FALSE
      minSuff[[.step]] <- structure(allkfoldConds[isSuff, , drop = FALSE], 
                                    conCov = cons[, isSuff, drop = FALSE])
    }  
  }
  minSuff
}
make.msc <- function(x, outcome, tti, details){
  if (is.null(x)) return(NULL)
  vnms <- colnames(tti$scores)
  out <- data.frame(
    outcome,
    condition = C_mconcat(C_relist_Char(vnms[t(x)], 
                                        rep(ncol(x), nrow(x))), 
                          sep = "*"),
    setNames(data.frame(t(attr(x, "conCov"))), c("consistency", "coverage")),
    stringsAsFactors = FALSE)
  if (length(details))
    out <- cbind(out, 
                 .det.tti(tti, paste0(out$condition, "->", out$outcome), 
                          what = details))
  rownames(out) <- NULL
  out
}

findAsf <- function(tti, y, freqs, con, cov, .conjList, maxSol, maxstep, only.minimal.asf){
  .conSc <- lapply(seq_along(.conjList), function(i) C_conjScore(tti$scores, .conjList[[i]]))
  maxstep1 <- maxstep
  maxstep1[[1]] <- min(maxstep1[[1]], length(.conSc))
  .combs <- allStructs(maxstep1)
  
  # initial step  
  .sol <- list()
  i <- 0L
  # search for asf's
  nn <- vapply(.conSc, ncol, integer(1))
  for (i in seq_along(.combs)){
    .c <- .combs[[i]]
    if (prod(nn[.c]) == 0 || any(tabulate(.c, nbins = length(.conSc)) > nn)) next
    .s <- C_find_asf(.c, .conSc[.c], y, freqs, con, cov, maxSol)
    
    if (nrow(.s) > 0){
      .cands <- lapply(seq_along(.c), function(i){
        .part <- .conjList[[.c[i]]][.s[, i] + 1L, , drop = FALSE]
        unname(split.default(.part, row(.part)))
      })
      .newsol <- do.call(mapply, c(list(list), .cands, list(SIMPLIFY = FALSE)))
      stopifnot(is.recIntList(.newsol))
      if (only.minimal.asf && length(.sol)){ 
        .newsol <- .newsol[C_minimal_old(.newsol, .sol, ignore_equals = FALSE)]
      }

      .sol <- c(.sol, .newsol)
    }
  }
  .sol
}
make.asf <- function(tti, zname, .sol, inus.only, details){
  vnms <- colnames(tti$scores)
  .lhs <- hconcat(.sol, c("+", "*"), f = function(i) vnms[i])
  asf <- qcondTbl_asf(paste0(.lhs, "<->", zname), 
                      tti$scores, tti$freq)  
  asf$condition[] <- .lhs
  n_plus_stars <- gregexpr("[\\*\\+]", asf$condition)
  asf$complexity <- 
    lengths(n_plus_stars) + 1L - (vapply(n_plus_stars, "[[", integer(1), 1L) == -1L)
  asf <- asf[order(asf$complexity, -asf$consistency * asf$coverage), , drop = FALSE]
  if (length(details))
    asf <- cbind(asf, 
                 .det.tti(tti, paste0(asf$condition, "<->", asf$outcome), 
                          what = details))
  if (inus.only) asf <- asf[asf$inus, , drop = FALSE]
  rownames(asf) <- NULL
  asf
}

################################################################################

# versions of cna with fixed type
cscna <- function(...){
  cl <- match.call(cna, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(cna)
  cl$type <- "cs"
  eval.parent(cl)
}
mvcna <- function(...){
  cl <- match.call(cna, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(cna)
  cl$type <- "mv"
  eval.parent(cl)
}
fscna <- function(...){
  cl <- match.call(cna, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(cna)
  cl$type <- "fs"
  eval.parent(cl)
}
