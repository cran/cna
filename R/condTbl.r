
# extract msf from cna-object
msc <- function(x, details = x$details, cases = FALSE){
  stopifnot(inherits(x, "cna"))
  msc.list <- lapply(x$solution, "[[", "msc")
  all.msc <- do.call(rbind, msc.list)
  basicCols <- intersect(
    c("outcome", "condition", "con", "cov", "complexity", "minimal"),
    names(all.msc))
  details <- resolveDetails(details)
  details <- removeDetailMeasure(details, c("inus", "coherence", "redundant", "cyclic"))
  detailCols <- getDetailCols(details)
  if (NROW(all.msc) == 0L) 
    return(emptyCondTbl("stdAtomic", measures = x$measures, 
                        minimal = "minimal" %in% basicCols,
                        details = detailCols))
  all.msc$condition <- paste0(all.msc[["condition"]], "->", all.msc[["outcome"]])
  availableCols <- c(basicCols, intersect(detailCols, names(all.msc)))
  out <- data.frame(all.msc[availableCols], 
                    row.names = NULL, stringsAsFactors = FALSE)
  computeCols <- setdiff(detailCols, availableCols)
  if (length(computeCols)){
    add_det <- .det(ctInfo(x$configTable), out$condition, # cutoff / border  
                    what = intersect(getDetailCols(details, "details"), computeCols),
                    conCovMeasures = 
                      intersect(getDetailCols(details, "conCov"), computeCols))
    out[names(add_det)] <- add_det
  }
  if (cases){
    conds <- paste0(lhs(out$condition), "*", rhs(out$condition))
    casesList <- attr(x$configTable, "cases")
    tbl <- as.data.frame(condList(lhs(conds), x$configTable), 
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
  as.condTbl(out[union(c(basicCols, detailCols), names(out))], 
             measures = x$measures, condClass = "stdAtomic")
}

# extract msf from cna-object
asf <- function(x, details = x$details){
  stopifnot(inherits(x, "cna"))
  asf.list <- lapply(x$solution, "[[", "asf")
  all.asf <- do.call(rbind, asf.list)
  basicCols <- intersect(
    c("outcome", "condition", "con", "cov", "complexity", "minimal"),
    names(all.asf))
  details <- resolveDetails(details) 
  details <- removeDetailMeasure(details, c("coherence", "redundant", "cyclic"))
  detailCols <- getDetailCols(details)
  if (NROW(all.asf) == 0L) 
    return(emptyCondTbl("stdAtomic", measures = x$measures, 
                        minimal = "minimal" %in% basicCols,
                        details = detailCols))
  
  all.asf$condition <- paste0(all.asf[["condition"]], "<->", all.asf[["outcome"]])
  availableCols <- c(basicCols, intersect(detailCols, names(all.asf)))
  out <- data.frame(all.asf[availableCols], 
                    row.names = NULL, stringsAsFactors = FALSE)
  computeCols <- setdiff(detailCols, availableCols)
  if (length(computeCols)){
    add_det <- .det(ctInfo(x$configTable), out$condition, # cutoff / border  
                    what = intersect(getDetailCols(details, "details"), computeCols),
                    conCovMeasures = 
                      intersect(getDetailCols(details, "conCov"), computeCols))
    out[names(add_det)] <- add_det
  }
  as.condTbl(out[c(basicCols, detailCols)], 
             measures = x$measures, condClass = "stdAtomic")
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

as.condTbl.condList <- function (x, measures = attr(x, "measures"), ...){
  info <- attr(x, "info")
  stopifnot(is.list(x), vapply(x, inherits, logical(1), "cond"))
  outcome <- character(length(x))
  out <- info[c("outcome", "condition", "con", "cov", "complexity", "freq")]
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
  additionalCols <- setdiff(names(out), c("outcome", "condition", "con", "cov", "complexity"))
  rmCols <- subset(additionalCols, colAlls(is.na(out[additionalCols])))
  out[rmCols] <- NULL
  as.condTbl(out, measures = measures)
}
as.condTbl.data.frame <- function(x, condClass = "condString", measures = NULL, 
                                  ...){
  if (any(fac <- vapply(x, is.factor, logical(1))))
    x[fac] <- lapply(x[fac], as.character)
  if (!is.null(x$condition) && identical(class(x$condition), "character")) 
    class(x$condition) <- c(condClass, "character")
  if (!is.null(x$outcome) && identical(class(x$outcome), "character"))
    class(x$outcome) <- c("outcomeString", "character")
  structure(x, measures = measures, row.names = seq_len(nrow(x)), 
            class = c("condTbl", "data.frame"))
}
emptyCondTbl <- function(condClass = "condString", details = character(0), 
                         minimal = FALSE, ...){
  stopifnot(is.character(details))   # 'details' must be either default/empty or output of getDetailCols!!
  out <- data.frame(outcome = structure(character(0), 
                                        class = c("outcomeString", "character")),
                    condition = structure(character(0), 
                                          class = c("condString", "character")),
                    con = numeric(0),
                    cov = numeric(0),
                    complexity = integer(0))
  if (minimal){
    out$minimal <- logical(0)
  }
  for (d in details){
    out[[d]] <- if (d %in% c("inus", "redundant", "cyclic")){
      logical(0)
    } else {
      numeric(0)
    }
  }
  as.condTbl(out, ...)
}

# print method for class condTbl
# Code taken from print.data.frame, except for leftAlignedColnames 
print.condTbl <- function(x, n = 20, digits = 3, quote = FALSE, 
  row.names = TRUE, printMeasures = TRUE, ...){
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
  if (printMeasures) prntMeasures(attr(x, "measures"))
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
condTbl <- function(x, ...){
  cl <- match.call()
  cl[[1]] <- as.name("condition")
  as.condTbl(eval.parent(cl))
}
# as.data.frame method for class condTbl - removes all class attributes and 
# transforms list of cases (if present) to (','-separated) enumerations in a 
# character vector.
as.data.frame.condTbl <- function(x, ...){
  if (inherits(x$cases, "casesList")){
    x$cases <- format(x$cases, maxlen = Inf, align = FALSE)
  }
  cnaClasses <- c("condString", "outcomeString", "stdAtomic", "stdBoolean", "stdComplex")
  unclassCols <- sapply(x, inherits, cnaClasses)
  x[unclassCols] <- lapply(x[unclassCols], unclass)
  class(x) <- "data.frame"
  attributes(x) <- attributes(x)[c("names", "class", "row.names")]
  x
}

# `[` method for class condTbl - keeps measures attributes if 'condition', 
# 'con' and  'cov' are still present as columns in the output
`[.condTbl` <- function(x, ...){
   out <- NextMethod()
   nms <- colnames(out)
   cnaClasses <- c("condString", "outcomeString", "stdAtomic", "stdBoolean", "stdComplex")
   keepClass <- any(vapply(out, inherits, cnaClasses, FUN.VALUE = logical(1)))
   conCovRelevant <- if (keepClass){
     all(c("con", "cov") %in% colnames(out)) &&
       any(rowAlls(!is.na(out[c("con", "cov")])))
     } else FALSE
   if (keepClass){
     attr(out, "measures") <- if (conCovRelevant){
       attr(x, "measures")
     } else NULL
   } else {
     out[] <- lapply(out, as.vector)
     class(out) <- setdiff(class(out), "condTbl")
     attr(out, "measures") <- NULL
   }
   out
}

