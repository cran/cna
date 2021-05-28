
#' auxConfigTable: Generate a configTable from a cond vector
#' Creates a 'minimal/incomplete full.ct', i.e. a configTable
#' containing all factor values (such as A, b - or in mv case: A=1, B=0) 
#' appearing in cond, but (unlike full.ct)  not all possible row configurations.
#' This makes it possible to generate a configTable from a cond where full.ct would fail
#' due to memory limitations.
#' cond must be either DNF, msc/asf, csf or constant 0/1.
#' 
#' If check = TRUE only valid names are returned (thus eliminating 
#' entries as '(A' that can result in case on invalid cond structure).
#' erwartet cond in dnf/msc/asf/csf Form
auxConfigTable  <- function(cond, x = NULL, check = TRUE){
  if (is.null(x)){
    type <- if (any(grepl("=[0-9]", cond))) "mv" else "cs"
    extr_asf <- extract_asf(cond)
    factors <- extractFactors(unlist(extr_asf, use.names = FALSE), 
                              type = type, check = check)
    if (length(factors) == 0) return(configTable(data.frame()))
    if (type == "cs"){
      uvals <- setNames(rep(list(0:1), length(factors)), factors)
    } else {
      uvals <- attr(factors, "values")
      uvals[] <- lapply(uvals, rep, length.out = max(lengths(uvals)))
    }
  } else {
    if (is.matrix(x)) x <- as.data.frame(x)
    if (inherits(x, "configTable")){
      type <- attr(x, "type")
    } else {
      type <- ctType(x)
    }
    factors <- names(x)
    if (type %in% c("cs", "fs")){
      uvals <- setNames(rep(list(0:1), length(factors)), factors)
    } else {
      uvals <- lapply(x, unique.default)
      uvals[] <- lapply(uvals, rep, length.out = max(lengths(uvals)))
    }
  }
  ct <- configTable(data.frame(uvals), type = type, 
                    rm.dup.factors = FALSE, rm.const.factors = FALSE, 
                    verbose = FALSE)
  return(ct)
}

#' extractFactors
#' Extracts factor names from a cond-Vekcor (globally, not individually for conds).
#' Asuming a cond in some standard form (dnf/msc/asf, not csf!)
#' Return char vector with factor names, 
#' - if type is "mv": adds attr. 'values' (obsolete in cs-case)
#' - if check=TRUE only returns valid factor names:
#'     cna:::extractFactors("(A+B)*C", "cs")
#'     cna:::extractFactors("(A+B)*C", "cs", check = TRUE)
#'   -> check=T eliminates the wrong entries resulting from invalid cond structure)
extractFactors <- function(cond, type, check = FALSE,
                           split = c("<*->", "+", "*"), fixed = c(FALSE, TRUE, TRUE), 
                           relist = FALSE, ...){
  cond <- setdiff(cond, as.character(0:1))
  if (!length(cond)) return(character(0))
  hstrspl <- hstrsplit(cond, split = split, fixed = fixed, relist = relist, ...)
  out <- toupper(as.vector(hstrspl))
  if (!is.null(type) && type == "mv"){
    vals <- out
    out <- sub("=[0-9]+", "", out)
    fctrs <- out
  }
  if (type == "mv"){
    strspl <- strsplit(vals, "=", fixed = TRUE)
    ok <- lengths(strspl) == 2
    valsbyfctr <- vapply(strspl[ok], "[", 2, FUN.VALUE = character(1))
    valList <- lapply(split(valsbyfctr, fctrs[ok]), 
                      function(x) suppressWarnings(as.integer(unique.default(x))))
    valList <- valList[!m_all(happly(valList, is.na))]
    out <- names(valList)
  } else {
      out <- unique(out)
  }
  if (check){
    factor_ok <- checkFactorNames(out, warn = FALSE)
    out <- out[factor_ok]
  }
  if (type == "mv") attr(out, "values") <- valList[out]
  out
}

ctType <- function(x){
  x <- unlist(x, recursive = FALSE, use.names = FALSE)
  if (all(x %in% 0:1)) return("cs")
  if (all(x<=1) && all(x>=0)) return("fs")
  if (all(x>=0) && all(x%%1 == 0)) return("mv")
  stop("Invalid data for a configTable")
}
