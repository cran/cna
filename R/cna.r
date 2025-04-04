
cna <- function (x, outcome = TRUE, con = 1, cov = 1, maxstep = c(3, 4, 10), 
    measures = c("standard consistency", "standard coverage"), 
    ordering = NULL, strict = FALSE, exclude = character(0), notcols = NULL, 
    what = if (suff.only) "m" else "ac", details = FALSE, 
    suff.only = FALSE, acyclic.only = FALSE, cycle.type = c("factor", "value"), 
    verbose = FALSE, control = NULL, ...){

  # update control  
  control <- resolveCnaControl(control, .con = con, ..., 
                               control_input_null = is.null(control))
  checkCnaParams()
  measures <- resolveMeasures(measures)
  cl <- match.call()

  # config table  
  ct <- configTable(x, type = control$type, 
                    rm.const.factors = control$rm.const.factors, 
                    rm.dup.factors = control$rm.dup.factors)
  # minimal size for cna
  if (nrow(ct) <= 1 || ncol(ct) <= 1)
    stop("Config table must have at least two rows and two columns.")
  control$type <- attr(ct, "type")
  
  # details
  details <- resolveDetails(details)
  if (!control$inus.only){
    details <- addDetailMeasure(details, "inus", "front")
  }
  # details for asf and msc
  details_asf <- removeDetailMeasure(details, c("coherence", "redundant", "cyclic"))
  details_msc <- removeDetailMeasure(details_asf, "inus")

  # Slightly reduce con, cov, con.msc to avoid failing to find conditions 
  # due to rounding issues
  d.eps <- nrow(ct) * .Machine$double.eps
  con <- max(con - d.eps, 0)
  cov <- max(cov - d.eps, 0)
  control$con.msc <- max(control$con.msc - d.eps, 0)
  
  # maxstep
  stopifnot(length(maxstep) == 3, maxstep > 0)
  maxstep[1:2] <- pmin(maxstep[1:2], maxstep[3])

  # ctInfo: representation of configTable in a convenient structure for implementation
  cti <- ctInfo(ct, cutoff = control$cutoff, border = control$border)
  # call below is only made to produce message where all values are on same side of cutoff:
  if (control$type == "fs"){
    invisible(fs2cs(ct, cutoff = control$cutoff, border = control$border, 
                    verbose = TRUE))
  }
  vnms <- colnames(cti$scores)
  freqs <- cti$freq
  
  # ordering, outcome, notcols, exclude
  ordering <- check.ordering(ordering, cti)
  if (isTRUE(outcome)){ 
    outcome <- cti$resp_nms
  } else {
    outcome <- checkOutcome(outcome, colnames(cti$scores))
  }
  if (any(neg.outcome <- outcome == tolower(outcome))){
    outcome <- toupper(outcome)
    notcols <- union(notcols, outcome[neg.outcome])
  }
  if (is.list(ordering) && (strict || length(ordering[[1]]) == 1)) 
    outcome <- setdiff(outcome, ordering[[1]])
  if (control$type == "mv" && any(outcome %in% names(ct))){
    mvnms <- intersect(outcome, names(ct))
    outcome <- union(setdiff(outcome, mvnms), 
                     cti$resp_nms[unlist(cti$config[mvnms], use.names = FALSE)])
  }
  outcome <- intersect(cti$resp_nms, outcome) # order!
  notcols <- resolveNotCols(notcols, names(ct), control$type)
  fVals <- factorValues(cti)
  exclude <- resolveExclude(exclude, outcome, notcols, fVals)

  # setup solutions list
  sol <- list(
    msc = emptyCondTbl(condClass = "stdBoolean", 
                       minimal = !control$only.minimal.msc,
                       details = getDetailCols(details_msc)),
    asf = emptyCondTbl(condClass = "stdBoolean", 
                       minimal = !control$only.minimal.asf,
                       details = getDetailCols(details_asf)))
  sol <- rep(list(sol), length(outcome))
  names(sol) <- outcome

  # main loop over outcomes  
  for (zname in outcome){

    # Identify minimal sufficient conditions
    # --------------------------------------
    # Initialize minSuff, list of minimal sufficient combinations
    poteff <- potential.effects(cti, zname, ordering, strict)

    if (length(poteff) == 0L) next
    
    y <- cti$scores[, zname, drop = TRUE]
    if (zname %in% notcols){
      y[] <- 1-y
      names(sol) <- sub(paste0("^", zname, "$"), tolower(zname), names(sol))
      zname <- tolower(zname)
    }
    
    if (zname %in% names(exclude)){
      excl_z <- match(exclude[[zname]], colnames(cti$scores), 0L)
      excl_z <- sort(excl_z[excl_z>0])
    } else {
      excl_z <- integer(0)
    }
    minSuff <- findMinSuff(cti, y, poteff, freqs, maxstep, control, 
                           exclValues = excl_z, conCovDef = measures$def)
    n_msc <- sum(vapply(minSuff, NROW, integer(1)))
    if (verbose){
      cat("Outcome ", zname, " (", match(zname, outcome), "/", length(outcome), "):\n  ", 
          n_msc, " msc found\n", sep = "")
    }
    if (n_msc == 0) next

    msc <- lapply(minSuff, make.msc, outcome = zname, cti = cti, 
                  details = details_msc)
    msc <- do.call(rbind, msc)
    if (!isTRUE(getOption("dontsort"))) 
      msc <- msc[order(msc$complexity, -msc$con * msc$cov, msc$condition), , drop = FALSE]
    rownames(msc) <- NULL
    sol[[zname]] <- list(msc = msc)

    if (suff.only) next

    # Find asf's
    # ----------
    .conjList <- lapply(minSuff, "attr<-", "conCov", NULL)
    noMsc <- m_is.null(.conjList)
    .conjList[noMsc] <- lapply(which(noMsc), function(i) matrix(integer(0), 0, i))

    .sol <- findAsf(cti, y, freqs, con, cov, .conjList, control, 
                    maxstep = maxstep, conCovDef = measures$def, 
                    verbose = verbose)
    n_asf <- length(.sol)
    if (n_asf > 0){
      asf <- make.asf(cti, zname, .sol, control, details = details_asf, 
                      conCovDef = measures$def, verbose = verbose)
      n_asf <- nrow(asf) # may be less than before due to inus.only
      sol[[zname]]$asf <- asf
    }
    if (verbose) cat("  ", n_asf, " asf found\n\n", sep = "")
  }

  out <- structure(list(), class = "cna")
  out$call <- cl
  if (!is.null(out$call$outcome)){ 
    out$call$outcome <- ifelse(outcome %in% notcols, tolower(outcome), outcome)
  }
  out$x <- x
  out$ordering <- ordering
  out$notcols <- notcols
  out$configTable <- ct
  if (attr(ct, "type") == "fs") out$fsInfo <- list(cutoff = control$cutoff, border = control$border)
  out$solution <- sol
  out$measures <- measures
  out$what <- resolveWhat(what)
  out$details <- details
  out[c("con", "cov", "acyclic.only", "cycle.type")] <- 
    list(con, cov, acyclic.only, match.arg(cycle.type))
  out$control <- control
  return(out)
}


findMinSuff <- function(cti, y, poteff, freqs, maxstep, control,
                        exclValues = integer(0), conCovDef = 1:2){
  
  if (cti$type == "mv") poteff <- unique(sub("(.+)=.+", "\\1", poteff))
  nsteps <- min(length(poteff), maxstep[1L])
  minSuff <- vector("list", nsteps)
  if (!control$only.minimal.msc) minimList <- vector("list", nsteps)
  
  for (.step in seq_along(minSuff)){
    # Select .step-fold conditions occurring in the data
    allkfoldConds <- findAllSubsets(cti$valueId, .step, match(poteff, colnames(cti$valueId)), 
                                    exclValues = exclValues)
    if (NROW(allkfoldConds) == 0) next
    if (!control$only.minimal.msc){
      minimList[[.step]] <- rep(TRUE, nrow(allkfoldConds))
    }
    # Eliminate combinations that contain a simpler combination that has been selected in an earlier step
    if (.step >= 2L){
      for (k_ in seq_len(.step-1L))
        if (!is.null(minSuff[[k_]])){
          is.minim <- !hasSubsetInM(allkfoldConds, minSuff[[k_]])
          if (control$only.minimal.msc){
            allkfoldConds <- allkfoldConds[is.minim, , drop = FALSE]
          } else if (!all(is.minim)){
            minimList[[.step]][!is.minim] <- FALSE
          }
      }
      rm(k_)
    }
    if (nrow(allkfoldConds) == 0) break
    stopifnot(anyDuplicated(allkfoldConds) == 0L)
    # Select sufficient conditions and add them to output list
    cons <- conj_conCov(allkfoldConds, cti$scores, y, freqs, def = as.integer(conCovDef))
    if (any(isSuff <- cons[1, ] >= control$con.msc, na.rm = TRUE)){
      isSuff[is.na(isSuff)] <- FALSE
      minSuff[[.step]] <- structure(allkfoldConds[isSuff, , drop = FALSE], 
                                    conCov = cons[, isSuff, drop = FALSE])
      if (!control$only.minimal.msc) 
        attr(minSuff[[.step]], "minimal") <- minimList[[.step]][isSuff]
    }  
  }
  minSuff
}

make.msc <- function(x, outcome, cti, details){
  if (is.null(x)) return(NULL)
  vnms <- colnames(cti$scores)
  out <- data.frame(
    outcome,
    condition = C_mconcat(C_relist_Char(vnms[t(x)], 
                                        rep(ncol(x), nrow(x))), 
                          sep = "*"),
    setNames(data.frame(t(attr(x, "conCov"))), c("con", "cov")),
    stringsAsFactors = FALSE)
  nstars <- gregexpr("*", out$condition, fixed = TRUE)
  out$complexity <- lengths(nstars) + 1L - (vapply(nstars, "[[", integer(1), 1L) == -1L)
  if (!is.null(attr(x, "minimal"))){
    out$minimal <- attr(x, "minimal")
  }
  if (anyDet(details)){
    out[getDetailCols(details)] <- .det(cti, paste0(out$condition, "->", out$outcome), 
                                        what = details$detailMeasures, 
                                        conCovMeasures = details$conCovMeasures)
  }
  rownames(out) <- NULL
  out
}

findAsf <- function(cti, y, freqs, con, cov, .conjList, maxstep, control, 
                    conCovDef = 1:2, verbose = FALSE){
  .conSc <- lapply(seq_along(.conjList), function(i) C_conjScore(cti$scores, .conjList[[i]]))
  maxstep1 <- maxstep
  maxstep1[[1]] <- min(maxstep1[[1]], length(.conSc))
  .combs <- allStructs(maxstep1)
  
  # initial step  
  .sol <- list()
  i <- 0L
  # search for asf's
  nn <- vapply(.conSc, ncol, integer(1))
  if (verbose){
    .complexity <- 0L # .complexity is never defined if !verbose
    .count_newsol <- 0L  # counts number of solutions of current complexity in loop below
  }
  for (i in seq_along(.combs)){
    .c <- .combs[[i]]
    if (verbose && sum(.c)>.complexity){
      .complexity <- sum(.c)
      cat("\r  ...searching for asf of complexity ", .complexity, "...", sep = "")
      flush.console()
    }
    .newsol <- list()
    if (prod(nn[.c]) > 0 && all(tabulate(.c, nbins = length(.conSc)) <= nn)){
      .s <- C_find_asf(.c, .conSc[.c], y, freqs, con, cov, 
                       maxSol = control$maxSol, def = conCovDef)
      if (nrow(.s) > 0){
        .cands <- lapply(seq_along(.c), function(i){
          .part <- .conjList[[.c[i]]][.s[, i] + 1L, , drop = FALSE]
          unname(split.default(.part, row(.part)))
        })
        .newsol <- do.call(mapply, c(list(list), .cands, list(SIMPLIFY = FALSE)))
        stopifnot(is.recIntList(.newsol))
        if (control$only.minimal.asf && length(.sol)){ 
          .newsol <- .newsol[C_minimal(.newsol, .sol, strict = TRUE)]
        }
        .sol <- c(.sol, .newsol)
      }
    }
    if (verbose){
      .count_newsol <- .count_newsol + length(.newsol)
      .last.compl <- (i == length(.combs)) || (sum(.combs[[i+1L]]) > .complexity) 
      if (.last.compl){ ## && .count_newsol>0L){
        cat("\r    complexity ", .complexity, ": ", .count_newsol, " potential asf",
            "                 \n", sep = "")
        .count_newsol <- 0L
      }
    }
  }
  if (verbose) cat("\r                                              \r")
  .sol
}
make.asf <- function(cti, zname, .sol, control, details, conCovDef = 1:2, 
                     verbose = FALSE){
  vnms <- colnames(cti$scores)
  .lhs <- hconcat(.sol, c("+", "*"), f = function(i) vnms[i])
  asf <- qcondTbl_asf(paste0(.lhs, "<->", zname), cti, 
                      asf.selection = control$asf.selection, conCovDef = conCovDef)
  attr(asf, "measures") <- NULL
  if (verbose && nrow(asf)<length(.lhs)){
    cat("    ", length(.lhs)-nrow(asf), " asf (of ", length(.lhs) ,
        ") are removed based on criterion asf.selection=\"", 
        control$asf.selection, "\"\n", sep = "")
  }
  if (nrow(asf) == 0) return(NULL)
  asf$condition[] <- as.character(lhs(asf$condition))
  n_plus_stars <- gregexpr("[\\*\\+]", asf$condition)
  asf$complexity <- 
    lengths(n_plus_stars) + 1L - (vapply(n_plus_stars, "[[", integer(1), 1L) == -1L)
  if (!isTRUE(getOption("dontsort"))) 
    asf <- asf[order(asf$complexity, -asf$con * asf$cov), , drop = FALSE]
  n0 <- nrow(asf)
  if (!control$only.minimal.asf){
    asf$minimal <- is.minimal(asf$condition)
  }
  .cond <- if (n0 == 0) character(0) else paste0(asf$condition, "<->", asf$outcome)
  if (useCtiList(cti)) 
    cti <- ctiList(cti, .cond)
  
  outCols <- union(names(asf), getDetailCols(details))
  if (control$inus.only) details <- addDetailMeasure(details, "inus")
  if (anyDet(details)){
    .detCols <- getDetailCols(details) # always include "inus"
    asf[getDetailCols(details)] <- 
      .det(cti, .cond, what = details$detailMeasures, 
           conCovMeasures = details$conCovMeasures)[.detCols]
  }
  if (control$inus.only){
    asf <- asf[asf$inus, outCols, drop = FALSE]
    if (verbose)
      cat("    ", n0 - nrow(asf), " non-INUS asf (of ", n0 ,") are removed, as inus.only=TRUE\n", 
          sep = "")
  } else if (verbose){
    cat("    Keeping all asf, as inus.only=FALSE\n")
  }
  rownames(asf) <- NULL
  asf
}

checkOutcome <- function(x, values){
  if (is.null(x)) return(x)
  ok <- x %in% values
  if (all(ok)) return(x)
  ok_up <- toupper(x) %in% values
  x[!ok & ok_up] <- toupper(x)[!ok & ok_up]
  ok <- x %in% values
  if (!all(ok)){
    stop("Invalid 'outcome' value", if (sum(!ok)>1) "s", 
         " specified: ", paste0(x[!ok], collapse = ", "), 
         call. = FALSE)
  }
  x  
}

