
# ==== full.ct() ====
# Generic function
full.ct <- function(x, ...) UseMethod("full.ct")


.expandUniqvals <- function(valueList, nmax = NULL){
  if (is.null(nmax) || 
      (n <- prod(lengths(valueList))) <= nmax){
    out <- do.call(expand.grid, 
                   c(valueList, list(KEEP.OUT.ATTRS = FALSE)))
    return(out)
  }
  nval <- lengths(valueList)
  log_ntot <- sum(log(nval))
  nmax <- round(exp(min(log(nmax), log_ntot)))
  complete <- log(nmax) == log_ntot
  if (log_ntot > log(.Machine$integer.max)){
    out <- matrix(unlist(lapply(valueList, sample, size = nmax, replace = TRUE), 
                         recursive = FALSE, use.names = FALSE), 
                  nrow = nmax, ncol = length(valueList))
    repeat {
      out <- unique(out)
      if (nrow(out) == nmax) break
      out <- rbind(
        out, 
        sapply(valueList, sample, size = nmax-nrow(out), replace = TRUE))
    }
  } else {
    ntot <- prod(lengths(valueList))
    moduli <- c(1, cumprod(nval[-length(nval)]))
    samp <- if (complete) seq_len(ntot) else sort(sample(ntot, nmax))
    ind <- .expandRowIDs(samp-1, moduli)
    ind[] <- lapply(ind, "+", 1)
    out <- mapply("[", valueList, ind, 
                  SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }
  out <- data.frame(out)
  names(out) <- names(valueList)
  out
}
.expandRowIDs <- function(x, mod){
  if (length(mod) == 1) return(list(x))
  c(.expandRowIDs(x %% mod[[length(mod)]], mod[-length(mod)]), 
    list(x %/% mod[[length(mod)]]))
}


# Aux function rowID
# generates a unique integer identifier of the rows in the ct
rowID <- function(cti){
  # if (cti$type == "fs") warning("Be careful when using rowID() with 'fs' data.")
  v <- matrix(unlist(cti$uniqueValues, use.names = FALSE)[cti$valueId],
              nrow = nrow(cti$valueId),
              dimnames = list(NULL, names(cti$nVal)))
  maxVal <- vapply(cti$uniqueValues, max, 1L) + 1L
  m <- c(1L, cumprod(maxVal[-length(maxVal)]))
  drop(v %*% m)
}


# ==== Method for class 'cti' ====
# builds 
#   x       output of ctInfo configTable
#   cond    used for selection of factors
# value:    cti of configTable, mv if original is mv, cs else
full.ct.cti <- function(x, cond = NULL, nmax = NULL, ...){
  if (!is.null(cond)){
    varsInCond <- extractFactors(unlist(extract_asf(cond)), x$type)
    x <- selectFactors(x, varsInCond)
  }
  expanded <- .expandUniqvals(x$uniqueValues, nmax = nmax)
  if (x$type == "fs") x$type <- "cs"
  scoresColnms <- colnames(x$score)
  x$scores <- do.call(cbind, 
                      mapply(outer, expanded, x$uniqueValues, 
                             MoreArgs = list(FUN = "=="), 
                             SIMPLIFY = FALSE))
  mode(x$scores) <- "numeric"
  colnames(x$scores) <- scoresColnms
  vId <- switch(x$type,
    cs = 2L - as.matrix(expanded),
    mv = mapply(match, expanded, x$uniqueValues, SIMPLIFY = TRUE))
  if (is.null(dim(vId))){
    dim(vId) <- dim(expanded)
    colnames(vId) <- names(x$nVal)
  }
  nVal <- lengths(x$uniqueValues)
  vId[] <- sweep(vId, 
               2, 
               c(if (length(nVal)) 0L, cumsum(nVal[-length(nVal)])), 
               "+")
  x$valueId <- vId
  x$freq <- rep(1L, nrow(expanded))
  x
}


# ==== Method for class 'configTable' ====
# builds full configTable
#   x       configTable
#   value:  configTable, mv if original is mv, cs else
full.ct.configTable <- function(x, cond = NULL, nmax = NULL, ...){
  if (!is.null(cond)){
    cond <- noblanks(cond)
    varsInCond <- extractFactors(unlist(extract_asf(cond)), attr(x, "type"), 
                                 check = TRUE)
    varsInCond <- intersect(names(x), varsInCond)
    if (length(varsInCond) == 0) 
      stop("Invalid input to full.ct:\n", paste0("  ", cond, collapse = "\n"),
           call. = FALSE)
    x <- full.ct(x[varsInCond], nmax = nmax)
  }
  cti <- ctInfo(x)
  expanded <- .expandUniqvals(cti$uniqueValues, nmax = nmax)
  configTable(expanded, type = if (cti$type == "mv") "mv" else "cs",
              rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)
}

# ==== Default Method (for matrix or data.frame) ====
# builds full.ct
# cases: character, list, matrix/data.frame
full.ct.default  <- function(x, type = "auto", cond = NULL, nmax = NULL, ...){
  if (is.numeric(x) && length(x) == 1 && x %% 1 == 0 && x >= 1L){
    if (x<=26){
      x <- rep(LETTERS, length.out = x)
    } else {
      x <- paste0("X", seq_len(x))
    }
  }
  if (is.character(x)){
    type <- if (any(grepl("=", x))) "mv" else "cs"
    px <- lapply(x, tryparse)
    if (!all(ok <- !vapply(px, is.null, logical(1))))
      stop("Invalid input to full.ct:\n", paste0("  ", x[!ok], collapse = "\n"),
           call. = FALSE)
    if (type == "cs"){
      nms <- unique(toupper(unlist(lapply(px, all.vars))))
      x <- setNames(rep(list(0:1), length(nms)), nms)
    } else {
      vals <- rapply(px, .call2list, how = "unlist",
                     stopOps = c("==", "<", ">", "<=", ">="), 
                     validOps = c("<-", "<<-", "=", "&", "|", "(", "-"))
      vals <- unique(vals)
      symbols <- vapply(vals, is.symbol, FUN.VALUE = TRUE)
      if (!all(sapply(vals[symbols], as.character) %in% 
               c("<-", "<<-", "=", "&", "|", "(", "-"))){
        stop("Improper condition(s) of type ", dQuote("mv"), " specified.", 
             call. = FALSE)
      }
      vals <- vals[!symbols]
      if (!all(lengths(vals) == 3)) stop("Check the condition")
      var <- vapply(vals, function(x) as.character(x[[2]]), character(1))
      val <- vapply(vals, "[[", 3, FUN.VALUE = numeric(1))
      x <- lapply(split(val, var), sort)
    }
  }
  if (!is.data.frame(x) && is.list(x)){
    if (is.null(names(x))){
      if (length(x)<=26){
        names(x) <- rep(LETTERS, length.out = length(x))
      } else {
        names(x) <- paste0("X", seq_along(x))
      }
    }
    names(x) <- make.unique(names(x))
    x <- .expandUniqvals(x, nmax = nmax)
    if (!all(unlist(x, use.names = FALSE) %in% 0:1))
      type <- "mv"
  }
  if (is.data.frame(x) || is.matrix(x)){
    return(full.ct.configTable(
      configTable(x, type = type, 
                  rm.dup.factors = FALSE, 
                  rm.const.factors = FALSE, 
                  verbose = FALSE, ...), 
      cond = cond, nmax = nmax))
  }
  stop("Don't know how to apply 'full.ct' to this input.")
}
  
