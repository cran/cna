
# ==== full.ct() ====
# Generic function
full.ct <- function(x, ...) UseMethod("full.ct")

.expandUniqvals <- function(valueList){
  do.call(expand.grid, 
          c(valueList, list(KEEP.OUT.ATTRS = FALSE)))
}

# ==== Method for class 'cti' ====
# builds 
#   x       output of ctInfo configTable
#   cond    used for selection of factors
# value:    cti of configTable, mv if original is mv, cs else
full.ct.cti <- function(x, cond = NULL, ...){
  if (!is.null(cond)){
    varsInCond <- extractFactors(unlist(extract_asf(cond)), x$type)
    x <- selectFactors(x, varsInCond)
  }
  expanded <- .expandUniqvals(x$uniqueValues)
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
full.ct.configTable <- function(x, cond = NULL, ...){
  if (!is.null(cond)){
    cond <- noblanks(cond)
    varsInCond <- extractFactors(unlist(extract_asf(cond)), attr(x, "type"), 
                                 check = TRUE)
    varsInCond <- intersect(names(x), varsInCond)
    if (length(varsInCond) == 0) 
      stop("Invalid input to full.ct:\n", paste0("  ", cond, collapse = "\n"),
           call. = FALSE)
    x <- full.ct(x[varsInCond])
  }
  cti <- ctInfo(x)
  expanded <- .expandUniqvals(cti$uniqueValues)
  configTable(expanded, type = if (cti$type == "mv") "mv" else "cs",
              rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)
}

# ==== Default Method (for matrix or data.frame) ====
# builds full.ct
# cases: character, list, matrix/data.frame
full.ct.default  <- function(x, type = "auto", cond = NULL, ...){
  if (is.numeric(x) && length(x) == 1 && x %in% 1:length(LETTERS)){
    x <- LETTERS[seq_len(x)]
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
  if (!is.data.frame(x) && is.list(x) && !is.null(names(x))){
    x <- .expandUniqvals(x)
    if (!all(unlist(x, use.names = FALSE) %in% 0:1))
      type <- "mv"
  }
  if (is.data.frame(x) || is.matrix(x)){
    return(full.ct.configTable(
      configTable(x, type = type, 
                  rm.dup.factors = FALSE, 
                  rm.const.factors = FALSE, 
                  verbose = FALSE, ...), 
      cond = cond))
  }
  stop("Don't know how to apply 'full.ct' to this input.")
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
