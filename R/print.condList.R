
print.condList <- function (x, ...){
  nn <- names(x)
  ll <- length(x)
  if (length(nn) != ll)
    nn <- paste("Component", seq.int(ll))
  for (i in seq_len(ll)){
    x1 <- x[[i]]
    info <- attr(x1, "info")
    cat(addblanks(nn[i]), 
        if (info$reshaped) paste0(" [input: ", info$input, "]"),
        ":\n")
    print(x1, ...)
    cat("\n")
  }
  invisible(x)
}

`[.condList` <- function(x, ...){
  out <- NextMethod()
  attributes(out) <- c(
    attributes(out),
    attributes(x)[c("class", "type", "n", "cases", "ct")]
  )
  i <- eval.parent(sys.call()[[3]])
  if (is.character(i)) i <- match(i, names(x), 0L)
  attr(out, "info") <- attr(x, "info")[i, ]
  out
}
`[[.condList` <- function(x, ...){
  out <- NextMethod()
  if (identical(out, "Invalid condition")) return(out)
  attributes(out) <- c(
    attributes(out),
    attributes(x)[c("type", "n", "cases", "ct")])
  i <- eval.parent(sys.call()[[3]])
  if (is.character(i)) i <- match(i, names(x), 0L)
  attr(out, "info") <- attr(x, "info")[i, ]
  out
}
`$.condList` <- function(x, ...){
  out <- NextMethod()
  if (is.null(out) || identical(out, "Invalid condition")) return(out)
  attributes(out) <- c(
    attributes(out),
    attributes(x)[c("type", "n", "cases", "ct")])
  i <- sys.call()[[3]]
  i <- match(as.character(i), names(x), 0L)
  attr(out, "info") <- attr(x, "info")[i, ]
  out
}


# summary.condList: print.condList with print.table = FALSE
summary.condList <- function(object, ...){
  print.condList(object, print.table = FALSE, ...)
}

# ------------------------------------------------------------------------------

# as.data.frame-method for class "condList", the output of condition():
# Splits all conditions in x into rhs and lhs of <-> or ->,  
# boolean conditions are kept as they are. The corresponding columns are joined into 
# one data.frame, each (part of a) condition appears once only.
as.data.frame.condList <- function(x, row.names = attr(x, "cases"), optional = TRUE, ...){
  x <- x[!sapply(x, inherits, "invalidCond")]
  n.obs <- attr(x, "n")
  if (length(x) == 0){
    out <- data.frame(n.obs)
  } else {
    compl <- vapply(x, inherits, "complexCond", 
                    FUN.VALUE = logical(1), USE.NAMES = FALSE)
    x[compl] <- lapply(x[compl], flattenComplexCond)
    dfList <- unname(lapply(x, data.frame, check.names = !optional))
    keepCol <- split(!duplicated(unlist(lapply(dfList, names))), 
                     rep(seq_along(dfList), lengths(dfList)))
    out <- mapply("[", dfList, keepCol, 
                  SIMPLIFY = FALSE, USE.NAMES = TRUE)
    out <- do.call(cbind, out)
    out$n.obs <- n.obs
  }
  if (missing(row.names) && is.list(row.names)){
    rownames(out) <- C_mconcat(row.names, sep = ",")
  } else {
    rownames(out) <- row.names
  }
  out
}
# auxiliary function
flattenComplexCond <- function(complexCond){
  out <- as.data.frame(unlist(complexCond, recursive = F))
  names(out) <- unlist(unname(lapply(complexCond, names)))
  out
}


