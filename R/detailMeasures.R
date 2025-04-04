
# ==== details ====
detailMeasures <- function(cond, x, 
  what = c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence"),
  cycle.type = c("factor", "value"), ...){
  what <- resolveDetails(what) #, available = available)
  out <- .det(x, noblanks(cond), what = what$detailMeasures, cycle.type = cycle.type, 
              conCovMeasures = what$conCovMeasures, ...)
  out$outcome <- structure(
    C_mconcat(happly(extract_asf(rownames(out)), rhs), sep = ","),
    class = c("outcomeString", "character"))
  out$condition <- structure(rownames(out), class = "condString")
  as.condTbl(out[union(c("outcome", "condition"), getDetailCols(what))])
}


# ==== .det() ====
# Switch order of first 2 args to provide dispatching on x
# Generic function
.det <- function(x, cond, ...) UseMethod(".det")

# ==== Default method for data.frame or configTable ====
#  Converts x to a configTable and then calls the .det.cti
.det.default <- function(x, cond, 
    what = c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence"),
    type, cycle.type = "factor", inus.def = "implication", ...){
  if (!inherits(x, "configTable")){
    if (missing(type)) type <- "auto"
    x <- configTable(x, type = type, rm.dup.factors = FALSE, rm.const.factors = FALSE, 
                     verbose = FALSE)
  }
  cti <- ctInfo(x)
  qtypes <- .qcondType(cond, colnames(cti$scores), cti$type,
                       stdComplex.multiple.only = FALSE) 
  ok <- qtypes %in% c("stdAtomic", "stdComplex")
  if (any(!ok)){
    stop("Invalid condition(s):\n", 
         paste0("  ", cond[!ok], collapse = "\n"),
         "\ndetails() expects valid conditions in standard form.",
         call. = FALSE)
  }
  if (useCtiList(cti)) cti <- ctiList(cti, cond)
  .det(cti, cond = cond, what = what, available = x$details, cycle.type = cycle.type, 
       inus.def = inus.def, ...)
}

# ==== Method for class 'cti' ====
#   x        cti
#   cond     character vector with the csf
#   what     char-vector: list of required detailMeasures
#   available, cycle.type
#            Passed to detailMeasures/resolveDetails
#   in.csf   special shortcut for the case where .det.cti is called from within csf()
# value: A data.frame with columns according to what
.det.cti <- function(x, cond, 
    what = c("inus", "cyclic", "exhaustiveness", "faithfulness", "coherence"),
    available = what, cycle.type = "factor", in.csf = FALSE, is.ctiList = FALSE, 
    inus.def = "implication", conCovMeasures = character(0), ...){

  if (any(c("redundant", "inus", "exhaustiveness", "faithfulness") %in% what)){
    x_full <- full.ct(x, cond = cond)
    qc_full <- qcond_csf(cond, x_full$scores, flat = TRUE)
  }
  out <- data.frame(matrix(numeric(0), length(cond)), row.names = cond)
  if ("redundant" %in% what || (in.csf && "inus" %in% what)){
    red <- m_any(.redund.cti(x_full, cond, simplify = FALSE, full = TRUE, 
                             qc_full = qc_full, names = FALSE))
  }
  if ("inus" %in% what){
    if (in.csf){
      # no need to screen asf for non-inus structures
      out$inus <- !red
      const <- rep(FALSE, length(cond))
      const[out$inus] <- constCols(qcond_csf(cond[out$inus], x_full$scores, 
                                             force.bool = TRUE))
      if (any(const)) out$inus[const] <- FALSE
      constFact <- constFact(cond[out$inus], x_full)
      if (any(constFact)) out$inus[out$inus][constFact] <- FALSE
      partStrRed <- partiallyRedundant(extract_asf(cond[out$inus]), x_full, 
                                       inusDef = inus.def)
      if (any(partStrRed)) out$inus[out$inus][partStrRed] <- FALSE
    } else {
      out$inus <- .inus.cti(x_full, cond, full = TRUE, def = inus.def)
    }
  }
  if ("cyclic" %in% what){
    out$cyclic <- cyclic(cond, cycle.type = cycle.type, use.names = FALSE)
  }
  ex_ff <- c("exhaustiveness", "faithfulness") %in% what
  if (any(ex_ff)){
    if (is.ctiList){
      exf <- calcExff(cti = x, cti.full = x_full, cond, names = FALSE)
    } else {
      ctiL <- ctiList(x, cond)
      exf <- .exff.ctiList(ctiL, cond, cti.full = full.ct(ctiL),
                          names = FALSE)[, ex_ff, drop = FALSE]
    }
    out <- cbind(out, exf)
  }
  if ("coherence" %in% what)
    out$coherence <- coherence.cti(x, cond, names = FALSE)
  if ("redundant" %in% what){
    out$redundant <- red
  }
  if (length(conCovMeasures)){
    extr <- getConCovMeasures(
      cond, x, measures = 
        resolveMeasures(conCovMeasures, imposeLength2 = FALSE))
    out[names(extr)] <- extr
  }
  out
}

getConCovMeasures <- function(cond, cti, measures){
  # measures <- resolveMeasures(measures, imposeLength2 = FALSE)
  dupl <- duplicated(measures$def)
  if (any(dupl)){
    measures[] <- lapply(measures, "[", !dupl)
  }
  hasCsf <- any(lengths(asfs <- extract_asf(cond)) > 1)
  if (hasCsf){
    qcondTbl_csf(cond, sc = cti$scores, freqs = cti$freq, 
                 conCovDef = measures$def, imposeLength2 = FALSE)[-(1:2)]
  } else {
    cond <- unlist(asfs)
    qcondTbl_asf(cond, cti, conCovDef = measures$def, 
                 imposeLength2 = FALSE)[-(1:2)]
  }
}

.detailMeasures <- c("inus", "cyclic", "exhaustiveness", "faithfulness", 
                     "coherence", "redundant")

showDetailMeasures <- function(){
  writeLines(.detailMeasures)
}

# Aux function resolveDetails()
#   what           required measures
#   warn           Logical: warn if some required measure is not available
resolveDetails <- function(what, warn = TRUE){
  if (inherits(what, "resolvedDetails")) return(what)
  if (isFALSE(what) || is.null(what)) what <- character(0)
  if (isTRUE(what)) what <- setdiff(.detailMeasures, "redundant")
  what_in <- what
  what <- tolower(what)
  isConCovMeasure <- pmatch(
    what, 
    tolower(c(measuresList$full_names, unlist(measuresList$aliases, use.names = FALSE))), 
    nomatch = 0L, duplicates.ok = FALSE) > 0L
  conCovMeasures <- what[isConCovMeasure]
  posConCov <- which(isConCovMeasure)
  conCovMeasures <- resolveMeasures(conCovMeasures, imposeLength2 = FALSE)
  what <- what[!isConCovMeasure]
  ii <- pmatch(tolower(what), .detailMeasures, nomatch = 0, duplicates.ok = FALSE)
  posDet <- setdiff(seq_along(what_in), posConCov)[ii>0]
  invalidInput <- what_in[!isConCovMeasure][ii == 0]
  posInv <- setdiff(seq_along(what_in), posConCov)[ii == 0]
  what <- .detailMeasures[ii]
  if (warn && length(invalidInput)){
    warning("Input string", if (length(invalidInput) == 1) " is" else "s are", 
            " invalid or ambiguous as an identifier of a measure:\n  ", 
            paste0("\"", invalidInput, "\"", collapse = "\n  "))
  }
  structure(
    list(
      detailMeasures = intersect(what, .detailMeasures), 
      conCovMeasures = conCovMeasures, 
      invalid = invalidInput, 
      positions = list(details = posDet, 
                       conCov = posConCov, 
                       invalid = posInv)),
    class = c("resolvedDetails", "list")
  )
}
addDetailMeasure <- function(det, what, where = c("back", "front")){
  stopifnot(inherits(det, "resolvedDetails"), what %in% .detailMeasures)
  what <- setdiff(what, det$detailMeasures)
  if (!length(what)) return(det)
  where <- match.arg(where)
  l <- length(what)
  pos <- det$positions
  if (where == "back"){
    det$detailMeasures <- c(det$detailMeasures, what)
    m <- max(unlist(pos, use.names = FALSE), 0L)
    pos$details <- c(pos$details, m + seq_len(l))
  } else if (where == "front"){
    det$detailMeasures <- c(what, det$detailMeasures)
    pos <- list(
      details = c(seq_len(l), pos$details + 1L), 
      conCov = pos$conCov + 1L, 
      invalid = pos$invalid + 1L)
  }
  det$positions <- pos
  det
}
removeDetailMeasure <- function(det, what){
  stopifnot(inherits(det, "resolvedDetails"), what %in% .detailMeasures)
  i <- match(what, det$detailMeasures, nomatch = 0L)
  if (all(i == 0)) return(det)
  det$detailMeasures <- det$detailMeasures[-i]
  det$positions$details <- det$positions$details[-i]
  det
}

getDetailCols <- function(details, which = c("details", "conCov")){
  stopifnot(inherits(details, "resolvedDetails"))
  out <- as.character(c(if ("details" %in% which) details$detailMeasures, 
                        if ("conCov" %in% which) details$conCovMeasures$shortNames))
  if (length(which) == 2
      && all(c("details", "conCov") %in% names(details$positions)) # [remove this row later -- useful in dev phase]
      ){
    out <- out[order(unlist(details$positions[c("details", "conCov")]))]
  }
  out
}

anyDet <- function(d){
  stopifnot(inherits(d, "resolvedDetails"))
  length(d$detailMeasures) + length(d$conCovMeasures$def) > 0
}

