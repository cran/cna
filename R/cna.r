
cna <- function (x, type,
    ordering = NULL, strict = FALSE,
    con = 1, cov = 1, con.msc = con, 
    notcols = NULL, rm.const.factors = TRUE, rm.dup.factors = TRUE,  
    maxstep = c(3, 3, 9),
    only.minimal.msc = TRUE, maxSol = 1e6, 
    suff.only = FALSE, what = "mac",
    cutoff = 0.5, border = c("down", "up", "drop")
    )
{

  # call and type
  cl <- match.call()
  if (!is.null(attr(x, "type"))) type <- attr(x, "type")
  if (missing(type)) type <- "cs"
  
  # variable names  
  tt <- truthTab(x, type = type, rm.const.factors = rm.const.factors, 
                 rm.dup.factors = rm.dup.factors)
  if (any(!grepl("[[:alpha:]]", names(tt)))) stop("All column names must contain a letter.")
  if (anyDuplicated(tolower(names(tt))) != 0L)
    stop("Inadmissable column names: names must be unique when case is ignored!")
  names(tt) <- toupper(names(tt))

  # more formal requirements  
  if (nrow(tt) <= 1)
    stop("Truth table must have at least two rows.")
  if (ncol(tt) < 2 || ncol(tt) > 26)
    stop("Truth table must have between 2 and 26 columns.")  # so belassen??
  ordering <- check.ordering(ordering, tt)

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

  # Slightly reduce con and cov values to avoid failing to find conditions due to rounding issues
  d.eps <- nrow(tt) * .Machine$double.eps
  con <- con - d.eps
  cov <- cov - d.eps

  # maxstep
  stopifnot(length(maxstep) == 3L)
  if (maxstep[[2L]] < 1) suff.only <- FALSE
  maxstep[1:2] <- pmin(maxstep[1:2], maxstep[3])

  # Define config, uniqueValues, resp_nms, factMat, nVal, valueId, scores
  tti <- tt.info(tt, cutoff = cutoff, border = match.arg(border))
  
  f <- attr(tt, "n")
  
  # setup output object
  sol <- vector("list", length(tti$resp_nms))
  names(sol) <- tti$resp_nms
  
  for (zname in tti$resp_nms){
  
    # Identify minimal sufficient conditions
    # --------------------------------------
    # Initialize minSuff, list of minimal sufficient combinations
    .znm <- if (grepl("=", zname)) strsplit(zname, "=")[[c(1L, 1L)]] else zname
    poteff <- potential.effects(tt, .znm, ordering, strict)
    if (length(poteff) == 0L) next
    
    nsteps <- min(length(poteff), maxstep[1L])
    minSuff <- vector("list", nsteps)
    y <- tti$scores[, zname, drop = TRUE]
    if (zname %in% notcols){
      y[] <- 1-y
      names(sol) <- sub(paste0("^", zname, "$"), tolower(zname), names(sol))
      zname <- tolower(zname)
    }  
    
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
      cons <- conj_conCov(allkfoldConds, tti$scores, y, f)
    if (any(isSuff <- cons[1, ] >= con.msc, na.rm = TRUE)){
        isSuff[is.na(isSuff)] <- FALSE
        minSuff[[.step]] <- structure(allkfoldConds[isSuff, , drop = FALSE], 
                                      conCov = cons[, isSuff, drop = FALSE])
      }  
    }
    if (all(vapply(minSuff, is.null, logical(1)))) next
    
    msc <- lapply(minSuff, function(x) if (!is.null(x)) 
      data.frame(condition = apply(x, 1, function(r) paste(colnames(tti$scores)[r], collapse = "*")),
                 setNames(data.frame(t(attr(x, "conCov"))), c("consistency", "coverage")),
                 stringsAsFactors = FALSE))
    msc <- do.call(rbind, msc)
    nstars <- gregexpr("\\*", msc$condition)
    msc$complexity <- vapply(nstars, length, integer(1)) + 1L - (vapply(nstars, "[[", integer(1), 1L) == -1L)
    msc <- msc[order(msc$complexity, -msc$consistency * msc$coverage, msc$condition), , drop = FALSE]
    rownames(msc) <- NULL
    sol[[zname]] <- list(msc = msc)

    if (suff.only) next

    # Find asf's
    # ----------
    .conjList <- lapply(minSuff, "attr<-", "conCov", NULL)
    noMsc <- vapply(.conjList, is.null, logical(1))
    .conjList[noMsc] <- lapply(which(noMsc), function(i) matrix(integer(0), 0, i))

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
      .s <- C_find_asf(.c, .conSc[.c], y, f, con, cov, maxSol)
      
      if (nrow(.s) > 0){
        .cands <- lapply(seq_along(.c), function(i){
          .part <- .conjList[[.c[i]]][.s[, i] + 1L, , drop = FALSE]
          unname(split.default(.part, row(.part)))
        })
        .newsol <- do.call(mapply, c(list(list), .cands, list(SIMPLIFY = FALSE)))
        stopifnot(is.recIntList(.newsol))
        if (length(.sol)){ 
          .newsol <- .newsol[C_minimal(.newsol, .sol)]
        }

        .sol <- c(.sol, .newsol)
      }
    }
    
    stopifnot(length(.sol) == 0 || vapply(.sol, is.intList, logical(1)))
    if (length(.sol)){
      asf <- quickCondTbl(intList2char(.sol, colnames(tti$scores)), zname, tt)
      n_plus_stars <- gregexpr("[\\*\\+]", asf$condition)
      asf$complexity <- 
        vapply(n_plus_stars, length, integer(1)) + 1L - (vapply(n_plus_stars, "[[", integer(1), 1L) == -1L)
      
      asf <- asf[order(asf$complexity, -asf$consistency * asf$coverage), , drop = FALSE]
      rownames(asf) <- NULL
  
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

  return(out)
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
