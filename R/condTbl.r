
# extract msf from cna-object
msc <- function(x, details = x$details, cases = FALSE){
  stopifnot(inherits(x, "cna"))
  msc.list <- lapply(x$solution, "[[", "msc")
  all.msc <- do.call(rbind, msc.list)
  basicCols <- intersect(c("outcome", "condition", "consistency", "coverage", "complexity", "minimal"),
                         names(all.msc))
  detailCols <- clarify_details(details, 
                                measures = c("exhaustiveness", "faithfulness"),
                                available = x$details)
  if (all(m_is.null(msc.list)))
    return(emptyCondTbl("stdAtomic", details = detailCols, msc = TRUE))
  all.msc$condition <- paste0(all.msc[["condition"]], "->", all.msc[["outcome"]])
  out <- data.frame(all.msc[c(basicCols, detailCols)],
                    row.names = NULL, stringsAsFactors = FALSE)
  if (cases){
    conds <- paste0(lhs(out$condition), "*", rhs(out$condition))
    casesList <- attr(x$configTable, "cases")
    tbl <- as.data.frame(condition(lhs(conds), x$configTable), 
                         nobs = FALSE)
    if ("fsInfo" %in% names(x)){
      instantiated <- fs2cs(tbl, cutoff = x$fsInfo$cutoff, border = x$fsInfo$border)
    } else {
      instantiated <- tbl
    }
    out$cases <- lapply(instantiated, function(v) unlist(casesList[v == 1]))
    class(out$cases) <- "casesList"
    names(out$cases) <- NULL
  }
  as.condTbl(out, condClass = "stdAtomic")
}

# extract msf from cna-object
asf <- function(x, details = x$details, warn_details = TRUE){
  stopifnot(inherits(x, "cna"))
  asf.list <- lapply(x$solution, "[[", "asf")
  basicCols <- c("outcome", "condition", "consistency", "coverage", "complexity")
  detailCols <- clarify_details(details, 
                                measures = c("inus", "exhaustiveness", "faithfulness"),
                                available = x$details, warn = warn_details)
  detailCols <- union("inus", detailCols)
  if (all(vapply(asf.list, NROW, integer(1)) == 0L))
    return(emptyCondTbl("stdAtomic", details = detailCols))
  all.asf <- do.call(rbind, asf.list)
  all.asf$condition <- paste0(all.asf[["condition"]], "<->", all.asf[["outcome"]])
  out <- data.frame(all.asf[c(basicCols, detailCols)],
                    row.names = NULL, stringsAsFactors = FALSE)
  as.condTbl(out, condClass = "stdAtomic")
}


# Add blanks before and after given strings (default: <->, -> and +)
addblanks <- function(x, spaces = getOption("spaces")){
  x <- noblanks(x)
  if (length(spaces) == 0) return(x)
  if ("+" %in% spaces) x <- gsub("+", " + ", x, fixed = TRUE)
  if ("*" %in% spaces) x <- gsub("*", " * ", x, fixed = TRUE)
  if ("<->" %in% spaces) x <- gsub("<->", " <-> ", x, fixed = TRUE)
  if ("->" %in% spaces) 
    x <- gsub("([^<])->", "\\1 -> ", x, fixed = FALSE)
  x
}
#addblanks(c("a->b", "a<-b", "a<->b", "a-b"))

# Specific methods for special types of strings
# (classes stdBoolean, stdAtomic, stdComplex)
print.stdBoolean <- print.stdAtomic <- print.stdComplex <-
  print.condString <- print.outcomeString <-
  function(x, ...) print(unclass(x), ...)
format.condString <- format.stdBoolean <- format.stdAtomic <- 
  format.stdComplex <- 
  function(x, ...){
  x <- addblanks(x)
  format.default(x, justify = "left", width = max(8, nchar(x)))
}
format.outcomeString <- function(x, ...){
  if (anyNA(x)) x[is.na(x)] <- "NA"
  x[x != "(No outcome)"] <- addblanks(x[x != "(No outcome)"])
  format.default(x, justify = "left", width = max(7, nchar(x)))
}

`[.stdBoolean` <- `[.stdAtomic` <- `[.stdComplex` <- 
  `[.condString` <- `[.outcomeString` <- 
  function(x, ...){
    structure(NextMethod(), class = class(x)) 
  }

# as.condTbl: builds a "condTbl"-object from a list of "cond"-objects 
as.condTbl <- function (x, ...) UseMethod("as.condTbl")

as.condTbl.condList <- function (x, ...){
  info <- attr(x, "info")
  stopifnot(is.list(x), vapply(x, inherits, logical(1), "cond"))
  outcome <- character(length(x))
  out <- info[c("outcome", "condition", "consistency", "coverage", "complexity", "freq")]
  ctypes <- info$condTypes
  if (any(bool <- ctypes %in% c("boolean", "stdBoolean")))
    out$outcome[bool] <- "(No outcome)"
  if (all(ctypes == "stdBoolean")){
    class(out$condition) <- c("stdBoolean", "character")
  } else if (all(ctypes == "stdAtomic")){
    class(out$condition) <- c("stdAtomic", "character")
  } else if (all(ctypes == "stdComplex")){
    class(out$condition) <- c("stdComplex", "character")
  } else {
    class(out$condition) <- c("condString", "character")
  }
  if (!is.null(out$outcome)) class(out$outcome) <- c("outcomeString", "character")
  additionalCols <- setdiff(names(out), c("outcome", "condition", "consistency", "coverage", "complexity"))
  rmCols <- subset(additionalCols, colAlls(is.na(out[additionalCols])))
  out[rmCols] <- NULL
  as.condTbl(out)
}
as.condTbl.data.frame <- function(x, condClass = "condString", ...){
  if (any(fac <- vapply(x, is.factor, logical(1))))
    x[fac] <- lapply(x[fac], as.character)
  if (!is.null(x$condition) && identical(class(x$condition), "character")) 
    class(x$condition) <- c(condClass, "character")
  if (!is.null(x$outcome) && identical(class(x$outcome), "character"))
    class(x$outcome) <- c("outcomeString", "character")
  structure(x, class = c("condTbl", "data.frame"))
}
emptyCondTbl <- function(condClass = "condString", details = FALSE, msc = FALSE){
  out <- data.frame(outcome = character(0),
                    condition = character(0),
                    consistency = numeric(0),
                    coverage = numeric(0),
                    complexity = numeric(0))
  if (msc){
    out$minimal <- logical(0)
  }
  for (d in clarify_details(details)){
    out[[d]] <- if (d %in% c("inus", "redundant", "cyclic")){
      logical(0)
    } else {
      numeric(0)
    }
  }
  as.condTbl(out)
}

# print method for class condTbl
# Code taken from print.data.frame, except for leftAlignedColnames 
print.condTbl <- function(x, n = 20, digits = 3, quote = FALSE, 
  row.names = TRUE, ...){
  leftAlignedColnames <- intersect(c("outcome", "condition", "solution", "cases"), 
                                   names(x))
  n.total <- nrow(x)
  n.print <- min(n, n.total)
  if (length(x) == 0L || n.print == 0L) {
    print.data.frame(x)
  } else {
    short <- n.print < n.total
    if (short) x <- x[seq_len(n.print), , drop = FALSE]
    m <- as.matrix(format.data.frame(x, digits = digits, quote = quote, 
      na.encode = FALSE))
    if (!isTRUE(row.names)){
      dimnames(m)[[1L]] <- if (identical(row.names, FALSE)){
        rep.int("", n.print)
      } else {
        row.names
      }
    }
    for (nm in leftAlignedColnames){
      m <- leftAlignColname(m, nm)
    }
    print(m, ..., quote = quote, right = TRUE)
    if (short){
      cat(" ... (total no. of formulas: ", n.total, ")\n", 
        sep = "")
    }
  }
  invisible(x)
}

leftAlignColname <- function(m, nm){
  if (!is.na(pos <- match(nm, colnames(m)))){
    w <- max(nchar(m[, pos]), na.rm = TRUE)
    colnames(m)[pos] <- format(nm, justify = "left", width = w)
  }
  m
}

`[.casesList` <- function(x, ...){
  structure(NextMethod(), class = class(x)) 
}
format.casesList <- function(x, maxlen = 40, align = TRUE, ...){
  fmtd <- vapply(x, formatCases, maxlen = maxlen, FUN.VALUE = character(1))
  if (align) fmtd <- format(fmtd, justify = "left")
  fmtd
}
formatCases <- function(x, maxlen){
  if (length(x) == 0) return("")
  stringLengths <- cumsum(nchar(x)) + seq_along(x) - 1
  if (stringLengths[length(stringLengths)] <= maxlen)
    return(C_concat(x, sep = ","))
  l <- length(x)
  suffix <- paste0(",... (", l, " cases)")
  i <- findInterval(maxlen - nchar(suffix), stringLengths)
  if (i == 0) return(paste0("(", l, " cases)"))
  paste0(C_concat(head(x, i), ","), suffix)
}

# condTbl
condTbl <- function(...){
  cl <- match.call()
  cl[[1]] <- as.name("condition")
  as.condTbl(eval.parent(cl))
}

# condition method for class condTbl
condition.condTbl <- function(x, ct = full.ct(x[["condition"]]), ...)
  condition.default(x[["condition"]], ct, ...)

# as.data.frame method for class condTbl - removes all class attributes and 
# transforms list of cases (if present) to (','-separated) enumerations in a 
# character vector.
as.data.frame.condTbl <- function(x, ...){
  if (inherits(x$cases, "casesList")){
    x$cases <- format(x$cases, maxlen = Inf, align = FALSE)
  }
  x[] <- lapply(x, unclass)
  class(x) <- "data.frame"
  x
}

