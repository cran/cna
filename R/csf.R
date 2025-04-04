
# Extract csf from cna object
csf <- function (x, n.init = 1000, details = x$details, asfx = NULL, 
                 inus.only = x$control$inus.only, inus.def = x$control$inus.def,  
                 minimalizeCsf = inus.only,
                 acyclic.only = x$acyclic.only, cycle.type = x$cycle.type, 
                 verbose = FALSE){

  # resolve inus.only in case it contains an inus definition
  if (is.character(inus.only)){
    inus.def <- match.arg(inus.only, c("implication", "equivalence"))
    warning("Setting inus.only=\"implication\" or inus.only=\"equivalence\" in cna() is deprecated;",
            "\nuse the parameter inus.def instead. See ?cnaControl for help.",
            call. = FALSE)
    inus.only <- TRUE
  } else {
    stopifnot(is.logical(inus.only))
    inus.def <- "implication"
  }
  
  # details
  details <- resolveDetails(details)
  if (inus.only && isFALSE(x$control$inus.only)){
    stop("Calling csf() with inus.only=TRUE on a cna-object generated as cna(..., inus.only = FALSE) is not meaningful.")
  }
  # Checking/enforcing compatibility of inus.only and minimalizeCsf
  if (inus.only && !minimalizeCsf){
    minimalizeCsf <- TRUE
    message("Calling csf() with inus.only=TRUE and minimalizeCsf=FALSE is not meaningful - setting minimalizeCsf=TRUE")
  }
  if (inus.only){ 
    details_out <- details
    details <- addDetailMeasure(details, c("inus", if (acyclic.only) "cyclic"))
  } else {
    details <- details_out <- addDetailMeasure(details, "inus")
    if (acyclic.only) details <- addDetailMeasure(details_out, "cyclic")
  }
  
  if ("cyclic" %in% details$detailMeasures) cycle.type <- match.arg(cycle.type)

  # Output if no asf
  if (is.null(asfx)){
    asfx <- asf(x, details = addDetailMeasure(details, "inus"))
  }
  if (nrow(asfx) == 0 || n.init <= 0) {
    out <- emptyCondTbl("stdComplex", measures = x$measures, 
                        details = getDetailCols(details_out))
    return(out)
  }
  cti <- ctInfo(x$configTable)
  
  # ---- Get csf's as combinations of asf's ---- 
  splitasf <- split(asfx, asfx$outcome)
  n.asf <- vapply(splitasf, nrow, integer(1))
  n.out <- prod(n.asf)
  if (verbose){
    cat("no. of asf", if (inus.only) " (inus.only)", ": ", 
        paste0(names(n.asf), ":", n.asf, collapse = "; "),
        "\nCombine these to ", paste(n.asf, collapse = "*"),
        " = ", n.out, " initial csfs.\n", sep = ""
            )
  }
  splitasf <- lapply(splitasf, 
    function(x) x[head_with_ties(x$complexity, n.init), , drop = FALSE])
  out <- combineAsf(splitasf, n.init)
  initialCsfs <- out$condition
  if (n.init < n.out){
    out <- head(out, n.init)
    warning("Not all csf solutions have been recorded. csf() with a higher value of n.init might find more solutions.")
    if (verbose){
      cat("Selection of the n.init=", n.init, " best csfs: no. of csf ", 
          n.out, "->", n.out <- n.init, "\n", sep = "")
    }
  }

  # ---- Identify structural redundancies from list solutions and reshape result ----
  if (minimalizeCsf){
    out <- minimalizeCsf(cti, out$condition, conCovDef = x$measures$def, 
                         removeSubCsf = FALSE)
    if (verbose){
      cat("Reduction of csf with structural redundancies (as minimalizeCsf=TRUE): no. of csf: ", 
          n.out, "->", n.out <- nrow(out), "\n", sep = "")
    }
  }
  
  # ---- Calculate required details if not done in the previous step ----
  details_without_cyclic <- removeDetailMeasure(details, "cyclic")
  # Note: inus is always part of details_without_cyclic
  
  inus_from_asfs <- if ("inus" %in% names(out)) out$inus else TRUE
  if (useCtiList(cti)){
    cti1 <- ctiList(cti, out$condition)
  } else cti1 <- cti
  detTable <- .det(cti1, out$condition, what = details_without_cyclic$detailMeasures, 
                   conCovMeasures = getDetailCols(details, "conCov"),
                   cycle.type = cycle.type, in.csf = TRUE, inus.def = inus.def)
  out[names(detTable)] <- detTable
  out$inus <- out$inus & inus_from_asfs

  # ---- Remove partial structural redundancies (if inus.only=TRUE) ---
  if (inus.only){
    out <- out[out$inus, , drop = FALSE]
    if (verbose){
      cat("Elimination of csf that are constant or have constant factors or partial structural redundancies (as inus.only=TRUE): no. of csf: ",
          n.out, "->", n.out <- nrow(out), "\n", sep = "")
    }
  }
  
  # ---- Remove cyclic solutions if acyclic.only=TRUE ----
  if (nrow(out) && "cyclic" %in% details$detailMeasures){
    out$cyclic <- cyclic(out$condition, cycle.type = cycle.type, use.names = FALSE)
    if (acyclic.only){
      out <- out[!out$cyclic, , drop = FALSE]
      if (verbose){
        cat("Elimination of cyclic csf (as acyclic.only=TRUE): no. of csf: ", n.out, "->", n.out <- nrow(out),
            "\n", sep = "")
      }
    }
  }

  # ---- Remove solutions that have a (irreducible) supermodel in the other solutions ----
  if (nrow(out) && minimalizeCsf){
    rmSol <- findSubCsf(out$condition)
    out <- out[!rmSol, , drop = FALSE]
    if (verbose){
      cat("Elimination of solutions that are a submodel of another (as removeSubCsf=TRUE): no. of csf: ", 
          n.out, "->", n.out <- nrow(out), "\n", sep = "")
    }
  }
  
  # --- Adjustments in table of csf's, if required --- 
  # if new csfs have been introduced in minimalizeCsf-step:
  if (nrow(out) && minimalizeCsf){
    newCsfs <- !(out$condition %in% initialCsfs)
    if (any(newCsfs)){
      # Compute complexity and inus for csf that resulted from removing struct redundancies
      out$complexity[newCsfs] <- getComplexity(out$condition[newCsfs])
      if (any(calc_inus <- is.na(out$inus))){
        asfs <- extract_asf(out$condition[calc_inus])
        out$inus[calc_inus] <- m_all(C_relist_Log(asfx$inus[match(unlist(asfs), asfx$condition)], lengths(asfs)))
      }
      # check if con/cov-thresholds are still attained
      d.eps <- nrow(x$configTable) * .Machine$double.eps
      con <- max(x$con - d.eps, 0)
      cov <- max(x$cov - d.eps, 0)
      ok <- out$con >= con & out$cov >= cov
      if (!all(ok)){
        out <- out[ok, ]
        if (verbose){
          cat("Recalculating con&cov - ", sum(!ok), 
              " csf not reaching the thresholds removed: no. of csf: ", 
              n.out, "->", n.out <- nrow(out), "\n", sep = "")
        }
      }
    }
  }
  
  # --- re-order csf ---
  if (nrow(out)){
    ord <- with(out, order(complexity, -con*cov, 
                           outcomeOrder(outcome, x$resp_nms)))
    out <- out[ord, , drop = FALSE]
  }

  if (nrow(out) == 0 && nrow(asfx)>0){
    out <- asfx
    detTable <- .det(cti, out$condition, what = details$detailMeasures, 
                     conCovMeasures = getDetailCols(details, "conCov"),
                     cycle.type = cycle.type, inus.def = inus.def)
    out[names(detTable)] <- detTable
    message("The asfs could not be combined to csfs. They are returned separately.")
  }
  
  # --- Remove paranthesis from csf consisting of a simple asf:
  csf_length1 <- !grepl(")*(", out$condition, fixed = TRUE)
  out$condition[csf_length1] <- sub("^\\((.+)\\)$", "\\1", out$condition[csf_length1])
  out <- out[c("outcome", "condition", "con", "cov", "complexity", 
               getDetailCols(details_out))]
  rownames(out) <- NULL
  as.condTbl(out, measures = x$measures, condClass = "stdComplex")
}

# Combine asf into csf's
combineAsf <- function(csflist, n){  
  n.asf <- vapply(csflist, nrow, integer(1))
  n.asfCombs <- prod(n.asf)
  l <- length(csflist)
  use.inus <- "inus" %in% names(csflist[[1]])
  colnms <- c("complexity", "con", "cov", if (use.inus) "inus")
  a <- csflist[[1]][colnms]
  a$id1 <- seq_len(nrow(a))
  if (nrow(a) > n){
    ord <- order(a$complexity)
    a <- a[ord, ]
    a <- a[head_with_ties(a$complexity, n), , drop = FALSE]
  }
  if (l >= 2) for (i in seq(2, l)){
    b <- csflist[[i]][colnms]
    names(b) <- paste0(names(b), ".",  1)
    b[[paste0("id", i)]] <- seq_len(nrow(b))
    ab <- expand.frames(a, b)
    ab$complexity <- ab$complexity + ab$complexity.1
    con.smaller <- ab$con.1 < ab$con
    ab$con[con.smaller] <- ab$con.1[con.smaller]
    cov.smaller <- ab$cov.1 < ab$cov
    ab$cov[cov.smaller] <- ab$cov.1[cov.smaller]
    if (use.inus) ab$inus <- ab$inus & ab$inus.1
    ord <- order(ab$complexity)
    ab <- ab[ord, , drop = FALSE]
    a <- ab[head_with_ties(ab$complexity, n), , drop = FALSE]
    a$complexity.1 <- a$con.1 <- a$cov.1 <- NULL
  }
  a <- a[with(a, order(complexity, -con * cov)), , drop = FALSE]
  selectedRows <- head_with_ties(cbind(a$complexity, with(a, -con*cov)), n)
  a <- a[selectedRows, , drop = FALSE]
  id <- a[grepl("^id", names(a))]
  allAsfs <- mapply(function(x, i) x$condition[i], csflist, id,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  allAsfs <- C_relist_Char(do.call(rbind, allAsfs), rep(l, nrow(a)))
  out <- data.frame(
    outcome = C_concat(names(csflist), sep = ","),
    condition = C_mconcat(happly(allAsfs, function(x) paste0("(", x, ")")), "*"),
    con = a$con, cov = a$cov, complexity = a$complexity, 
    stringsAsFactors = FALSE)
  if (use.inus) out$inus <- a$inus
  out
}

# Auxiliary function expand.frames
expand.frames <- function(x, y){
  nx <- nrow(x)
  ny <- nrow(y)
  cbind(x[rep(seq_len(nx), each = ny), , drop = FALSE],
        y[rep(seq_len(ny), nx), , drop = FALSE])
}

# Auxiliary function head_with_ties
head_with_ties <- function(x, n){
  x <- as.matrix(x)
  if (nrow(x) <= n) {
    n <- nrow(x)
  }
  else {
    notDup <- which(!duplicated(x))
    if (all(notDup <= n)) {
      n <- nrow(x)
    }
    else {
      n <- min(notDup[notDup > n]) - 1L
    }
  }
  seq_len(n)
}

# Aux fun to read complexity of csf (read from character string)
getComplexity <- function(cond){
  if (length(cond) == 0) return(integer(0))
  lhsides <- lapply(extract_asf(cond), lhs)
  ll <- lengths(strsplit(unlist(lhsides), "[\\+\\*]"))
  vapply(C_relist_Int(ll, lengths(lhsides)), sum, integer(1))
}
