
# Evaluate an 'disjuntion of conjunctions'
#   condstr  string in 'visible' syntax with 'standard boolean' format, i.e. a disjunction of conjunctions
#   sc       'scores' matrix
qcond_bool <- function(condstr, sc){
  if (inherits(sc, "cti")) sc <- sc$scores
  spl <- strsplit(condstr, "+", fixed = TRUE)
  conjs <- as.character(unique(unlist(spl, use.names = FALSE, recursive = FALSE)))
  splc <- strsplit(conjs, "*", fixed = TRUE)
  # Check for presence of the factors in the data
  usplc <- unlist(splc, recursive = FALSE, use.names = FALSE)
  nms <- colnames(sc)
  if (!all(usplc %in% nms)) 
    stop("Invalid factor value(s) in condition: ", paste0(setdiff(usplc, nms), collapse = ","), 
         call. = FALSE)
  factors <- unique(toupper(usplc))
  factor_ok <- !is.na(match(factors, nms))
  if (any(!factor_ok)) stop("Invalid condition specified.", call. = FALSE)
  n <- NROW(sc)
  ms_conjs <- matrix(vapply(splc, function(x) rowMins(sc[, x, drop = FALSE]),
                            numeric(n)),
                     nrow = n, ncol = length(splc))
  out <- matrix(vapply(spl, 
                       function(x) rowMaxs(ms_conjs[, match(x, table = conjs), drop = FALSE]),
                       numeric(n)),
                nrow = n, ncol = length(condstr))
  colnames(out) <- condstr
  out
}

# ===== Handling of 'asf' strings =====

# Evaluate an 'atomic condition' seperated with <-> or ->
#   condstr  string in 'visible' syntax with 'asf' format
#   sc       'scores' matrix
# value: array of dimension nrow(sc) x 2 x length(condstr),
#   with attributes 'condition' and 'response'
qcond_asf <- function(condstr, sc, force.bool = FALSE){
  if (inherits(sc, "cti")) sc <- sc$scores
  l <- length(condstr)
  n <- nrow(sc)
  lr <- strsplit(condstr, "<*->")
  out <- qcond_bool(as.character(unlist(lr, recursive = FALSE, use.names = FALSE)), sc)
  if (force.bool){
    dim(out) <- c(n, 2, l)
    equiv <- grepl("<->", condstr, fixed = TRUE)
    out1 <- numeric(n*l)
    dim(out1) <- c(n, l)
    if (any(equiv)) out1[, equiv] <- out[, 1, equiv] == out[, 2, equiv]
    if (any(!equiv)) out1[, !equiv] <- out[, 1, !equiv] <= out[, 2, !equiv]
    colnames(out1) <- condstr
    return(out1)
  }
  response <- colnames(out)[1:l * 2]
  dim(out) <- c(n, 2, l)
  structure(out, 
            condition = condstr, 
            response = response)
}
# quick version of condTbl for 'atomic condition'
# parametes: as above
# Value: condTbl with columns outcome, condition, con, cov
qcondTbl_asf <- function(condstr, cti, asf.selection = "none", conCovDef = 1:2, ...){
  stopifnot(asf.selection %in% c("none", "cs", "fs"))
  if (cti$type == "fs"){
    cutoff <- cti$fsInfo$cutoff
    border <- cti$fsInfo$border
  } else {
    cutoff <- 0.5
    border <- "up"
  }
  cond <- qcond_asf(condstr, cti$scores)
  if (asf.selection != "none"){
    # select condition where outcome varies within concordant cases:
    varying <- apply(cond, 3, function(tbl){
        C_varies(tbl[, 1], tbl[, 2], asfSelection = asf.selection, 
                 cutoff = cutoff, border = border)
      }, simplify = TRUE)
    if (!any(varying)) return(emptyCondTbl("stdAtomic", measures = conCovDef))
    cond <- structure(cond[, , varying, drop = FALSE], 
                      condition = attr(cond, "condition")[varying],
                      response = attr(cond, "response")[varying])
  }
  cond2condTbl(cond, cti$freq, conCovDef = conCovDef, ...)
}

# Take an expanded 'qcond_asf' array and return the corresponding condTbl
# cond   output from qcond_asf or qcond_csf
cond2condTbl <- function(cond, freqs, conCovDef = 1:2, imposeLength2 = TRUE){
  coco <- conCovFromArray(cond, freqs, def = conCovDef, 
                          imposeLength2 = imposeLength2)  # !detailed
  if (imposeLength2){
    out <- structure(
      data.frame(outcome = structure(attr(cond, "response"), 
                                     class = c("outcomeString", "character")),
                 condition = structure(attr(cond, "condition"),
                                       class = c("stdAtomic", "character")),
                 con = coco[1, ], cov = coco[2, ], 
                 row.names = NULL, stringsAsFactors = FALSE),
      measures = if (imposeLength2) resolveMeasures(conCovDef), 
      class = c("condTbl", "data.frame"))
  } else {
    meas <- resolveMeasures(conCovDef, imposeLength2 = FALSE)
    colnames(coco) <- make.names(meas$shortNames) 
    out <- structure(
      data.frame(outcome = structure(attr(cond, "response"), 
                                     class = c("outcomeString", "character")),
                 condition = structure(attr(cond, "condition"),
                                       class = c("stdAtomic", "character")),
                 coco, row.names = NULL, stringsAsFactors = FALSE),
      class = c("condTbl", "data.frame"))
  }
  out
}

# ===== Handling of 'csf' strings =====

# Evaluate an 'complex condition' 
#   condstr  string in 'visible' syntax with 'csf' format
#   sc       'scores' matrix
#   force.bool
#   freqs    if (force.bool & !flat) and freqs is supplied, an attribute conCov is attached
# Value:
# - if flat = TRUE: Returns an array resulting from applying qcond_asf() to the (flattended) 
# vector of all asf present in 'condstr', with additional attribute csflengths
# - if flat = FALSE: Returns a list with a separate object as returned by qcond_asf() for each csf
qcond_csf <- function(condstr, sc, conCovDef = NULL, freqs = NULL, 
                      flat = FALSE, force.bool = FALSE){
  if (inherits(sc, "cti")) sc <- sc$scores
  n <- nrow(sc)
  asfs <- extract_asf(condstr)
  lengths <- lengths(asfs)
  unlasfs <- as.character(unlist(asfs, use.names = FALSE, recursive = FALSE))
  varray <- qcond_asf(unlasfs, sc, force.bool = force.bool)
  if (force.bool){
    relisted <- if (length(varray)){
      relist1(as.vector(varray), lengths*n)
    } else {
      list()
    }
    out <- matrix(vapply(
      relisted, 
      function(x){ rowMins(matrix(x, nrow = n)) },
      numeric(n)), nrow = nrow(sc))
    colnames(out) <- condstr
    return(out)
  } else if (!flat & !is.null(freqs) & !is.null(conCovDef)){
    ctbl <- cond2condTbl(varray, freqs, conCovDef = conCovDef)
    asfCons <- C_relist_Num(ctbl$con, lengths)
    asfCovs<- C_relist_Num(ctbl$cov, lengths)
    conCov <- data.frame(
      con = vapply(asfCons, min, numeric(1)),
      cov = vapply(asfCovs, min, numeric(1)))
    conCov$asfCons <- asfCons
    conCov$asfCovs <- asfCovs
    conCov$outcome <- C_mconcat(C_relist_Char(ctbl$outcome, lengths), sep = ",")
  }
  if (flat){
    return(structure(varray, csflengths = lengths))
  }
  gr <- rep(seq_along(lengths), lengths*2*n)
  out <- lapply(split(as.vector(varray), gr),
                function(x){
                  array(x, c(n, 2, length(x)/(2*n)))
                  })
  names(out) <- condstr
  if (exists("conCov", inherits = FALSE))
    attr(out, "conCov") <- conCov
  out
}
# Quick version of condTbl for 'complex condition'
# parametes: as above
# Value: condTbl with columns outcome, condition, con, cov
qcondTbl_csf <- function(condstr, sc, freqs, conCovDef, imposeLength2 = TRUE){
  qc <- qcond_csf(condstr, sc, flat = TRUE)
  ctbl <- cond2condTbl(qc, freqs, conCovDef = conCovDef, imposeLength2 = imposeLength2)
  lengths <- attr(qc, "csflengths")
  gr <- rep(seq_along(lengths), lengths)
  conCov <- vapply(ctbl[-(1:2)], gmins, gr, FUN.VALUE = numeric(gr[[length(gr)]]))
  if (!is.matrix(conCov)) 
    conCov <- matrix(conCov, nrow = 1, dimnames = list(NULL, names(conCov)))
  data.frame(
    outcome = C_mconcat(split.default(ctbl$outcome, gr), sep = ","),
    condition = condstr,
    conCov,
    stringsAsFactors = FALSE
  )
}

# # Grouped version of condTbl for csf-strings
# groupedCondTbl_csf <- function(condstr, sc, freqs, conCovDef = 1:2){
#   qc <- qcond_csf(condstr, sc, flat = TRUE)
#   ctbl <- cond2condTbl(qc, freqs, conCovDef)
#   lengths <- attr(qc, "csflengths")
#   out <- split(ctbl, 
#                rep(seq_along(lengths), lengths))
#   names(out) <- condstr
#   out <- lapply(out, "rownames<-", NULL)
#   out
# }
