
cna <- function (x, type,
    ordering = NULL, strict = FALSE, outcome = TRUE, 
    con = 1, cov = 1, con.msc = con, 
    notcols = NULL, rm.const.factors = FALSE, rm.dup.factors = FALSE,  
    maxstep = c(3, 4, 10), inus.only = only.minimal.msc && only.minimal.asf,
    only.minimal.msc = TRUE, only.minimal.asf = TRUE, maxSol = 1e6, 
    suff.only = FALSE, what = if (suff.only) "m" else "ac",
    cutoff = 0.5, border = c("up", "down", "drop"),
    details = FALSE, acyclic.only = FALSE, cycle.type = c("factor", "value"), 
    asf.selection = c("cs", "fs", "none"), verbose = FALSE){
  # call and type
  cl <- match.call()
  if (!is.null(attr(x, "type"))) type <- attr(x, "type")
  if (missing(type)) type <- "auto"

  # resolve inus.only in case it contains an inus definition
  if (is.character(inus.only)){
    inus.def <- match.arg(inus.only, c("implication", "equivalence"))
    inus.only <- TRUE
  } else {
    stopifnot(is.logical(inus.only))
    inus.def <- "implication"
  }
  
  # Checking/enforcing compatibility of inus.only with only.minimal.msc and only.minimal.asf
  if (inus.only){
    if (!only.minimal.msc) 
      stop("Calling cna() with inus.only=TRUE and only.minimal.msc=FALSE is not meaningful.")
    if (!only.minimal.asf) 
      stop("Calling cna() with inus.only=TRUE and only.minimal.asf=FALSE is not meaningful.")
  }
  
  # Select asf.selection (choices: "none", "cs", "fs")
  asf.selection <- match.arg(asf.selection)
    
  # variable names  
  ct <- configTable(x, type = type, rm.const.factors = rm.const.factors, 
                    rm.dup.factors = rm.dup.factors)
  if (any(!grepl("[[:alpha:]]", names(ct)))) 
    stop("All column names must contain a letter.")
  if (anyDuplicated(tolower(names(ct))) != 0L)
    stop("Inadmissable column names: names must be unique when case is ignored!")
  names(ct) <- toupper(names(ct))

  # more formal requirements  
  if (nrow(ct) <= 1 || ncol(ct) <= 1)
    stop("Config table must have at least two rows and two columns.")
  
  # details
  details <- clarify_details(details)
  if (!suff.only) details <- union("inus", details)
  details1 <- setdiff(details, c("coherence", "redundant", "cyclic"))
  if (acyclic.only){
    details <- union(details, c("cyclic"))
  }

  # Check con and cov values and reduce them slightly to avoid failing 
  # to find conditions due to rounding issues
  if (length(con) != 1 || con < 0 || con > 1 || 
      length(cov) != 1 || cov < 0 || cov > 1){
    stop("Invalid input for 'con' or 'cov'")
  }
  d.eps <- nrow(ct) * .Machine$double.eps
  con <- max(con - d.eps, 0)
  cov <- max(cov - d.eps, 0)

  # maxstep
  stopifnot(length(maxstep) == 3, maxstep > 0)
  maxstep[1:2] <- pmin(maxstep[1:2], maxstep[3])

  # Define config, uniqueValues, resp_nms, factMat, nVal, valueId, scores
  border <- match.arg(border)
  cti <- ctInfo(ct, cutoff = cutoff, border = border)
  vnms <- colnames(cti$scores)
  freqs <- cti$freq
  
  # setup output object
  sol <- vector("list", length(cti$resp_nms))
  names(sol) <- cti$resp_nms
  
  # ordering and outcome
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
  if (type == "mv" && any(outcome %in% names(ct))){
    mvnms <- intersect(outcome, names(ct))
    outcome <- union(setdiff(outcome, mvnms), 
                     cti$resp_nms[unlist(cti$config[mvnms], use.names = FALSE)])
  }
  outcome <- intersect(cti$resp_nms, outcome) # order!

  # notcols
  if (!is.null(notcols)){
    if (type == "mv") stop("\"notcols\" not applicable if type==\"mv\"")
    if (length(notcols) == 1L && notcols == "all"){
      notcols <- names(ct) 
      if ("ALL" %in% names(ct)) 
        warning("'notcols=\"all\"' is ambiguous if a variable has name \"ALL\". ",
                "notcols is applied to _all_ variables.", call. = FALSE)
    }
    if (!is.character(notcols)) notcols <- names(ct)[notcols]
    notcols <- toupper(notcols)
    if (!all(notcols %in% names(ct))) stop("Wrong specification of 'notcols'")
  }

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
    minSuff <- findMinSuff(cti, y, poteff, freqs, con.msc, 
                           maxstep, only.minimal.msc)
    if (all(m_is.null(minSuff))) next
    
    msc <- lapply(minSuff, make.msc, outcome = zname, cti = cti, 
                  details = details1, suff.only = suff.only)
    msc <- do.call(rbind, msc)
    if (verbose){
      cat("Outcome ", zname, " (", match(zname, outcome), "/", length(outcome), "):\n  ", 
          nrow(msc), " msc found\n", sep = "")
    }
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

    .sol <- findAsf(cti, y, freqs, con, cov, .conjList, maxSol, maxstep, only.minimal.asf, 
                    verbose = verbose)
    stopifnot(length(.sol) == 0 || any(vapply(.sol, is.intList, logical(1))))
      
    if (length(.sol)){
      asf <- make.asf(cti, zname, .sol, inus.only, details1, 
                      asf.selection = asf.selection, verbose = verbose)
      sol[[c(zname, "asf")]] <- asf
    }
    if (verbose)
      cat("  ", if (length(.sol)) NROW(asf) else 0, " asf found\n\n", sep = "")
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
  if (attr(ct, "type") == "fs") out$fsInfo <- list(cutoff = cutoff, border = border)
  out$solution <- sol
  out$what <- what
  out$details <- details
  out[c("con", "cov", "con.msc", "inus.only", "acyclic.only", "cycle.type")] <- 
    list(con, cov, con.msc, inus.only, acyclic.only, cycle.type)
  if (out$inus.only) out$inus.only <- inus.def

  return(out)
}


findMinSuff <- function(cti, y, poteff, freqs, con.msc, maxstep, only.minimal.msc){
  if (cti$type == "mv") poteff <- unique(sub("(.+)=.+", "\\1", poteff))
  nsteps <- min(length(poteff), maxstep[1L])
  minSuff <- vector("list", nsteps)
  if (!only.minimal.msc) minimList <- vector("list", nsteps)
  
  for (.step in seq_along(minSuff)){
    # Select .step-fold conditions occurring in the data
    allkfoldConds <- findAllSubsets(cti$valueId, .step, match(poteff, colnames(cti$valueId)))
    if (!only.minimal.msc){
      minimList[[.step]] <- rep(TRUE, nrow(allkfoldConds))
    }
    # Eliminate combinations that contain a simpler combination that has been selected in an earlier step
    if (.step >= 2L){
      for (k_ in seq_len(.step-1L))
        if (!is.null(minSuff[[k_]])){
          is.minim <- !hasSubsetInM(allkfoldConds, minSuff[[k_]])
          if (only.minimal.msc){
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
    cons <- conj_conCov(allkfoldConds, cti$scores, y, freqs)
  if (any(isSuff <- cons[1, ] >= con.msc, na.rm = TRUE)){
      isSuff[is.na(isSuff)] <- FALSE
      minSuff[[.step]] <- structure(allkfoldConds[isSuff, , drop = FALSE], 
                                    conCov = cons[, isSuff, drop = FALSE])
      if (!only.minimal.msc) attr(minSuff[[.step]], "minimal") <- minimList[[.step]][isSuff]
    }  
  }
  minSuff
}
make.msc <- function(x, outcome, cti, details, suff.only){
  if (is.null(x)) return(NULL)
  vnms <- colnames(cti$scores)
  out <- data.frame(
    outcome,
    condition = C_mconcat(C_relist_Char(vnms[t(x)], 
                                        rep(ncol(x), nrow(x))), 
                          sep = "*"),
    setNames(data.frame(t(attr(x, "conCov"))), c("consistency", "coverage")),
    stringsAsFactors = FALSE)
  # if (cti$type == "mv"){
  #   out <- out[out$outcome != out$condition, , drop = FALSE]
  #   if (nrow(out) == 0) return(NULL)
  # }
  if (!is.null(attr(x, "minimal"))){
    out$minimal <- attr(x, "minimal")
  } else {
    out$minimal <- TRUE
  }
  details <- setdiff(details, "inus")
  if (length(details)){
    out[details] <- .det.cti(cti, paste0(out$condition, "->", out$outcome), 
                             what = details)
  }
  rownames(out) <- NULL
  out
}

findAsf <- function(cti, y, freqs, con, cov, .conjList, maxSol, maxstep, only.minimal.asf, 
                    verbose = FALSE){
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
make.asf <- function(cti, zname, .sol, inus.only, details, 
                     asf.selection, verbose = FALSE){
  vnms <- colnames(cti$scores)
  .lhs <- hconcat(.sol, c("+", "*"), f = function(i) vnms[i])
  asf <- qcondTbl_asf(paste0(.lhs, "<->", zname), cti$scores, cti$freq, 
                      asf.selection = asf.selection)
  if (verbose && nrow(asf)<length(.lhs)){
    cat("    ", length(.lhs)-nrow(asf), " asf (of ", length(.lhs) ,
        ") are removed based on criterion asf.selection=\"", asf.selection, "\"\n", 
        sep = "")
  }
  if (nrow(asf) == 0) return(NULL)
  asf$condition[] <- as.character(lhs(asf$condition))
  n_plus_stars <- gregexpr("[\\*\\+]", asf$condition)
  asf$complexity <- 
    lengths(n_plus_stars) + 1L - (vapply(n_plus_stars, "[[", integer(1), 1L) == -1L)
  asf <- asf[order(asf$complexity, -asf$consistency * asf$coverage), , drop = FALSE]
  n0 <- nrow(asf)
  if (length(details)){
    .cond <- if (n0 == 0) character(0) else paste0(asf$condition, "<->", asf$outcome)
    if (useCtiList(cti)) 
      cti <- ctiList(cti, .cond)
    asf <- cbind(asf, .det(cti, .cond, what = details, cycle.type = NULL))
  }
  if (inus.only){
    asf <- asf[asf$inus, , drop = FALSE]
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
