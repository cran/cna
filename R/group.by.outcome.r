
# function group.by.outcome
#   group a list of atomicCond's by their outcome
group.by.outcome <- function(condlst, cases = TRUE){
  stopifnot(is.list(condlst))
  if (!all(isAtomic <- vapply(condlst, inherits, logical(1), "atomicCond"))){
    condlst <- condlst[isAtomic]
    message("Note: group.by.outcomes only considers conditions of type 'atomic' - other are ignored.")
  }
  outc <- vapply(strsplit(names(condlst), "-> "), "[", character(1), 2)
  outc[is.na(outc)] <- "(No outcome)"
  out <- lapply(split(condlst, outc), grbyout1, cases = cases)
  structure(out, class = c("groupedConds", "listof"), cases = cases)
}
grbyout1 <- function(x, cases){
  secondCols <- do.call(cbind, lapply(x, "[", 2))
  outName <- names(secondCols)[1]
  if (!all(apply(secondCols, 1, function(col) length(unique(col)) == 1)))
    stop("Response ", outName, "is not identical in all condition tables.")
  ncases <- do.call(cbind, lapply(x, "attr", "n"))
  if (!all(apply(ncases, 1, function(col) length(unique(col)) == 1)))
    stop("n is not identical in all condition tables.")
  ncases <- ncases[, 1, drop = TRUE]
  if (cases){
    Cases <- do.call(cbind, lapply(x, "attr", "cases"))
    if (!all(apply(Cases, 1, function(col) length(unique(col)) == 1)))
      stop("Cases are not identical in all condition tables.")
    Cases <- sapply(Cases[, 1], paste, collapse = ",")
  }
  out <- do.call(data.frame, c(lapply(x, function(a) as.data.frame(a)[1]), list(check.names = FALSE)))
  out[[outName]] <- x[[1]][[outName]]
  out$n.obs <- ncases
  if (cases) rownames(out) <- Cases
  out
}

print.groupedConds <- function(x, cases, ...)
  print.listof(x, row.names = attr(x, "cases"), ...)
