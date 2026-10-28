
# ==== details ====
detailMeasures <- function(cond, x, what = TRUE, 
  inus.def = "implication", inus.func.minim = FALSE,
  cycle.type = c("factor", "value"), ...){
  if (!is.character(cond)) stop("cond must be a character vector.")
  if (detailsTRUE <- isTRUE(what)){
    # provisional value of 'what'
    # which measures are selected is decided in .det.cti() depending on 
    # condition types (all asf or csf/mixed)
    what <- list(detailMeasures = TRUE, conCovMeasures = character(0))
  } else {
    what <- resolveDetails(what)
  }
  out <- .det(x, noblanks(cond), what = what$detailMeasures, cycle.type = cycle.type, 
              conCovMeasures = what$conCovMeasures, 
              inus.def = inus.def, inus.func.minim = inus.func.minim, 
              ...)
  if (detailsTRUE) what <- resolveDetails(names(out)) 
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
.det.default <- function(x, cond, what = TRUE,
    type = "auto", cycle.type = "factor", 
    inus.def = "implication", inus.func.minim = FALSE, ...){
  if (inherits(x, "configTable") && (type == "auto" || type == attr(x, "type"))){
    type <- attr(x, "type")
  } else {
    x <- configTable(x, type = type, verbose = FALSE)
  }
  cti <- ctInfo(x)
  ctypes <- checkStdCondTypes(cond, cti)
  if (useCtiList(cti)) cti <- ctiList(cti, cond)
  .det(cti, cond = cond, what = what, cycle.type = cycle.type, 
       inus.def = inus.def, inus.func.minim = inus.func.minim, ..., 
       condType = ctypes)
}

checkStdCondTypes <- function(cond, cti, functionName = "detailMeasures"){
  out <- .qcondType(cond, colnames(cti$scores), cti$type, 
                    stdComplex.multiple.only = FALSE) 
  ok <- out %in% c("stdAtomic", "stdComplex")
  if (any(!ok)){
    stop("Invalid condition(s):\n", 
         paste0("  ", cond[!ok], collapse = "\n"),
         "\n", functionName, "() expects valid conditions in standard form (asf or csf)",
         call. = FALSE)
  }
  out
}

# ==== Method for class 'cti' ====
#   x        cti
#   cond     character vector with the csf
#   what     char-vector: list of required detailMeasures
#   cycle.type Passed to cyclic()
#   in.csf   special shortcut for the case where .det.cti is called from within csf()
# value: A data.frame with columns according to what
.det.cti <- function(x, cond, what = TRUE,
    cycle.type = "factor", in.csf = FALSE, is.ctiList = FALSE, 
    inus.def = "implication", inus.func.minim = FALSE, 
    conCovMeasures = character(0), ..., 
    condType = NULL){
  if(is.null(condType)){
    condType <- checkStdCondTypes(cond, x)
  }
  isComplex <- rep(condType == "stdComplex", length.out = length(cond))
  anyComplex <- any(isComplex)
  what <- getDetailCols(resolveDetails(what, asfOnly = !anyComplex))
  usesFull <- c("inus", "exhaustiveness", "faithfulness", 
                if (anyComplex) c("redundant", "fm_structural"))
  if (any(usesFull %in% what)){
    x_full <- full.ct(x, cond = cond)
    qc_full <- qcond_csf(cond, x_full$scores, flat = TRUE)
  }
  out <- data.frame(matrix(numeric(0), length(cond)), row.names = cond)
  if ((anyComplex && "redundant" %in% what) || 
      (in.csf && "inus" %in% what)){
    red <- m_any(.redund.cti(x_full, cond[isComplex], 
                             simplify = FALSE, full = TRUE, 
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
      out$inus <- .inus.cti(x_full, cond, full = TRUE, def = inus.def, 
                            func.minim = inus.func.minim)
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
  if ("coherence" %in% what){
    out$coherence <- rep(1, nrow(out))  # value for asf
    if (anyComplex){
      out$coherence[isComplex] <- 
        coherence.cti(x, cond[isComplex], names = FALSE)
    }
  }
  if ("redundant" %in% what){
    out$redundant <- rep(FALSE, nrow(out)) # value for asf
    if (anyComplex) out$redundant[isComplex] <- red
  }
  if ("fm_structural" %in% what){
    out$fm_structural <- rep(NA, nrow(out)) # value for asf
    if (anyComplex){
      out$fm_structural[isComplex] <- 
        .fm_struct.cti(x, cond[isComplex], names = FALSE)
    }
  }
  if ("fm_empirical" %in% what){
    out$fm_empirical <- rep(NA, nrow(out)) # value for asf
    if (anyComplex) out$fm_empirical[isComplex] <- .fm_emp(cond[isComplex])
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

.detailMeasures <- c("inus", "cyclic", "fm_empirical", "fm_structural", 
                     "exhaustiveness", "faithfulness", "coherence", "redundant")

showDetailMeasures <- function(){
  writeLines(.detailMeasures)
}

# Aux function resolveDetails()
#   what           required measures
#   warn           Logical: warn if some required measure is not available
resolveDetails <- function(what, warn = TRUE, asfOnly = FALSE){
  if (inherits(what, "resolvedDetails")){
    if (asfOnly){
      what <- removeDetailMeasure(
        what, 
        c("redundant", "coherence", "fm_structural", "fm_empirical"))
    }
    return(what)
  }
  if (isFALSE(what) || is.null(what)) what <- character(0)
  if (isTRUE(what)){
    what <- setdiff(.detailMeasures, "redundant")
    if (asfOnly){
      what <- setdiff(.detailMeasures, 
                      c("redundant", "coherence", "fm_structural", "fm_empirical"))
    }
  }
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
      details = c(seq_len(l), pos$details + l), 
      conCov = pos$conCov + l, 
      invalid = pos$invalid + l)
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
  det$positions[] <- 
    lapply(det$positions, match, sort.default(unlist(det$positions)))
  det
}

getDetailCols <- function(details, which = c("details", "conCov")){
  stopifnot(inherits(details, "resolvedDetails"))
  out <- as.character(c(if ("details" %in% which) details$detailMeasures, 
                        if ("conCov" %in% which) details$conCovMeasures$shortNames))
  if (length(which) == 2){
    out <- out[order(unlist(details$positions[c("details", "conCov")]))]
  }
  out
}

anyDet <- function(d){
  stopifnot(inherits(d, "resolvedDetails"))
  length(d$detailMeasures) + length(d$conCovMeasures$def) > 0
}
