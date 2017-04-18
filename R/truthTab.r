
# New version of cna:truthTab
# ===========================
# Modifications:
#   - types "cs", "mv", und "fs"
#   - Behaviour if x already is a truthTab
truthTab <- function(x, type = c("cs", "mv", "fs"), frequency = NULL,
                     case.cutoff = 0, rm.dup.factors = TRUE, rm.const.factors = TRUE,
                     .cases = NULL){
  nm.x <- deparse(substitute(x))
  stopifnot(is.data.frame(x) || is.matrix(x), length(case.cutoff) == 1)
  if (is.matrix(x)) {
    if (is.null(colnames(x)))
        colnames(x) <- LETTERS[seq_len(ncol(x))]
    x <- as.data.frame(x)
  } else if (inherits(x, "truthTab")){
    if (!is.null(frequency))
      stop(nm.x, " already is a truthTab - argument frequency is not admissable in this case!")
    frequency <- attr(x, "n")
    .cases <- attr(x, "cases")
    type <- attr(x, "type")
    x <- as.data.frame(x)
  }
  type <- tolower(type)
  type <- match.arg(type)
  if (is.null(.cases))
    .cases <- as.list(rownames(x))
  # check if data entries are correct
  datatype <- switch(type, cs = "integer",
                           mv = "integer",
                           fs = "numeric",
                           mv2cs = "integer")
  xvalues <- unlist(x, use.names = FALSE, recursive = FALSE)
  if (type == "cs" & !all(xvalues %in% 0:1))
    stop("Only values 0 and 1 are allowed in x with type=\"cs\".")
  if (type == "mv" & !all(xvalues %% 1 == 0))
    stop("Only integer values are allowed in x with type=\"mv\".")
  if (type == "fs" & !all(xvalues >= 0 & xvalues <= 1))
    stop("Only values >=0 and <=1 are allowed in x with type=\"fs\".")
  for (i in seq_along(x)) mode(x[[i]]) <- datatype
  if (nrow(x) > 0){
    # split rows into groups of identical rows and eliminate duplicates rows
    cx <- combine(x, sep = "\r")
    splitInput <- unname(split.data.frame(as.data.frame(x), cx))
    tt <- do.call(rbind, lapply(splitInput, "[", 1, , drop = FALSE))
    f <- if (is.null(frequency)){
      vapply(splitInput, nrow, integer(1))
    }  else {
      as.vector(tapply(frequency, cx, sum))
    }  
  } else {
    cx <- factor(character(0))
    splitInput <- list()
    tt <- x
    f <- integer(0)
  }
  rownames(tt) <- NULL
  # frequencies
  if (length(f) != nrow(tt) || any(f < 0) || any(is.na(f)) || 
      !isTRUE(all.equal(f, as.integer(f), check.attributes = FALSE)))
    stop("Inadmissible frequency argument")
  f <- as.integer(f)  
  .cases <- lapply(split(.cases, as.integer(cx)), unlist, recursive = FALSE, use.names = FALSE)
  ll <- vapply(.cases, length, integer(1))
  if (!isTRUE(all.equal(f, ll, check.attributes = FALSE)) && length(ll) > 0){
    .cases <- mapply(rep_len, .cases, f)
    .cases <-
      split.default(make.unique(unlist(.cases, use.names = FALSE)),
                    rep(seq_along(.cases), f))
  }
  # case.cutoff
  del.cases <- f < case.cutoff
  if (any(del.cases)){
    message("Note: ", sum(f[del.cases]), " of ", sum(f),
        " cases are removed due to case.cutoff = ", case.cutoff,
        ".")
    tt <- tt[!del.cases, , drop = FALSE]
    f <- f[!del.cases]
    .cases <- .cases[!del.cases]
  }
  # remove rows with frequency 0
  if (any(rm.rows <- (f == 0L))) {
    tt <- tt[!rm.rows, , drop = FALSE]
    f <- f[!rm.rows]
  }
  # constant columns
  constcols <- vapply(tt, function(col) length(unique(col)) == 1, logical(1))
  if (any(constcols)){
    nms.constcols <- names(tt)[constcols]
    if (rm.const.factors){
      tt <- tt[, !constcols, drop = FALSE]
      message("Note: The following factors are constant and therefore eliminated: ",
        paste(nms.constcols, collapse = ", "))
    } else {
    message("Note: The following factors are constant: ",
            paste(nms.constcols, collapse = ", "))
    }        
  }          
  # eliminate duplicated columns
  dupcols <- duplicated(t(tt))
  if (any(dupcols)){
    nms.dupcols <- names(tt)[dupcols]
    if (rm.dup.factors){
      tt <- tt[, !dupcols, drop = FALSE]
      message("Note: The following factors are duplicated and therefore eliminated: ",
        paste(nms.dupcols, collapse = ", "))
    } else {
    message("Note: The following factors are duplicated: ",
            paste(nms.dupcols, collapse = ", "))
    } 
  }
  # Warn if names are not syntactically valid
  nms <- names(tt)
  if (!identical(nms, make.names(nms, unique = TRUE))){
    warning("truthTab has syntactically invalid names. condition(), cna() and other functions may not work.",
            call. = FALSE)
  }
  # output
  class(tt) <- c("truthTab", "data.frame")
  attr(tt, "n") <- as.vector(f)
  attr(tt, "cases") <- lapply(.cases, function(x) sort(unlist(x, use.names = FALSE, recursive = FALSE)))
  attr(tt, "type") <- type
  tt
}


# Amended print-method for truthTab
# =================================
# New: Type is displayed
# cases now shown as rownames
#   Default show.cases: TRUE, if all rownames shorter than 20 characters
#   ...  digits, quote, right passed to print.data.frame
print.truthTab <- function(x, show.cases = NULL, ...){ 
  if (is.null(attr(x, "n")))
      warning("Attribute \"n\" is missing")
  if (is.null(attr(x, "cases")))
      warning("Attribute \"cases\" is missing")
  df.args <- list(as.data.frame(x), n.obs = attr(x, "n"), check.names = FALSE)
  prntx <- do.call(data.frame, df.args)
  rownames(prntx) <- vapply(lapply(attr(x, "cases"), sort), paste0, character(1), collapse = ",")
  cat(paste0("truthTab of type \"", attr(x, "type"), "\"\n"))
  if (is.null(show.cases) && nrow(x) > 0){
    show.cases <- max(nchar(rownames(prntx))) <= 20
    labels.suppressed <- !show.cases
  }  
  print(prntx, row.names = show.cases, ...)
  cat("Total no.of.cases:", sum(attr(x, "n")), "\n")
  if (exists("labels.suppressed", inherits = FALSE) && labels.suppressed)
    message("Printing of case labels is suppressed because some labels are longer than 20 characters.\n",
            "Use print with show.cases=TRUE to display them.")
  invisible(x)
}

# "["-method for truthTab:
# ========================
#  Note that default of argument 'drop' differs from that in "[.data.frame"
`[.truthTab` <- function(x, i, j, drop = FALSE, rm.dup.factors = TRUE, rm.const.factors = FALSE){
  cl <- sys.call()
  cl$rm.dup.factors <- cl$rm.const.factors <- NULL
  len <- length(cl)
  nms <- names(cl)
  if ("drop" %in% nms){
    warning("'drop' argument will be ignored")
    cl$drop <- NULL
    len <- len - 1L
  }
  noRowPos <- len == 3
  cl1 <- quote(`[.data.frame`(x, , , drop = FALSE))
  cl1[[2]] <- call("as.data.frame", cl[[2]])
  cl1[[3]] <- if (noRowPos || identical(cl[[3]], quote(expr = ))) TRUE else cl[[3]]
  cl1[[3]] <- i <- eval.parent(cl1[[3]])
  cl1[[4]] <- if (noRowPos) cl[[3]] else cl[[4]]
  xx <- eval.parent(cl1)
  truthTab(xx,
           type = attr(x, "type"),
           frequency = attr(x, "n")[i],
           .cases = attr(x, "cases")[i],
           rm.dup.factors = rm.dup.factors,
           rm.const.factors = rm.const.factors)
}


# More methods for class "truthTab"
# =================================
as.data.frame.truthTab <- function(x, ...){
  class(x) <- "data.frame"
  attributes(x)[c("type", "cases", "n")] <- NULL
  x
}
`[<-.truthTab` <- function(x, i, j, value){
  out <- as.data.frame(NextMethod("[<-", x))
  row.sel <- nargs() == 5 && !missing(i)
  rows <- if (row.sel) i else TRUE
  out <- truthTab(out,
           type = attr(x, "type"),
           frequency = attr(x, "n"),
           .cases = attr(x, "cases")[rows])
}

`$<-.truthTab` <- function(x, name, value){
  out <- as.data.frame(NextMethod("$<-", x))
  out <- truthTab(out,
           type = attr(x, "type"),
           frequency = attr(x, "n"),
           .cases = attr(x, "cases"))
}

`[[<-.truthTab` <- function(x, i, j, value){
  out <- as.data.frame(NextMethod("[[<-", x))
  out <- truthTab(out,
           type = attr(x, "type"),
           frequency = attr(x, "n"),
           .cases = attr(x, "cases"))
}

subset.truthTab <- function(x, subset, ...){
  r <- if (missing(subset))
    rep_len(TRUE, nrow(x))
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'subset' must be logical")
    r & !is.na(r)
  }
  x[r, , ...]
}

head.truthTab <- function (x, n = 6L, ...) 
{
  stopifnot(length(n) == 1L)
  n <- if (n < 0L) 
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  x[seq_len(n), , ...]
}

tail.truthTab <- function (x, n = 6L, ...) 
{
  stopifnot(length(n) == 1L)
  nrx <- nrow(x)
  n <- if (n < 0L) 
    max(nrx + n, 0L)
  else min(n, nrx)
  x[seq.int(to = nrx, length.out = n), , ...]
}

# combine:
# ========
# Neu ist der Parameter sep
combine <- function (x, rows = rownames(x), sep = ""){
  rows <- do.call(paste, c(x, sep = sep))
  factor(rows, levels = unique(rows))
}

################################################################################

# versions of truthTab with fixed type
cstt <- function(...){
  cl <- match.call(truthTab, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(truthTab)
  cl$type <- "cs"
  eval.parent(cl)
}
mvtt <- function(...){
  cl <- match.call(truthTab, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(truthTab)
  cl$type <- "mv"
  eval.parent(cl)
}
fstt <- function(...){
  cl <- match.call(truthTab, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(truthTab)
  cl$type <- "fs"
  eval.parent(cl)
}
