
# Extract csf from cna object
csf <- function (x, n = 20, tt = x$truthTab, details = x$details,
                 asfx = asf(x, details, warn_details = FALSE) 
                 ){
  
  if (nrow(asfx) == 0 || n <= 0) {
    out <- emptyCondTbl("stdComplex", details = x$details)
    # if (coh) out$coherence <- numeric(0)
    return(out)
  }
  splitasf <- split(asfx, asfx$outcome)
  n.csf <- prod(vapply(splitasf, nrow, integer(1)))
  splitasf <- lapply(splitasf, 
    function(x) x[head_with_ties(x$complexity, n), , drop = FALSE])
  l <- length(splitasf)
  a <- splitasf[[1]][c("complexity", "consistency", "coverage")]
  a$id1 <- seq_len(nrow(a))
  if (nrow(a) > n){
    ord <- order(a$complexity)
    a <- a[ord, ]
    a <- a[head_with_ties(a$complexity, n), , drop = FALSE]
  }
  if (l >= 2) for (i in seq(2, l)){
    b <- splitasf[[i]][c("complexity", "consistency", "coverage")]
    names(b) <- paste0(names(b), ".",  1)
    b[[paste0("id", i)]] <- seq_len(nrow(b))
    ab <- expand.frames(a, b)
    ab$complexity <- ab$complexity + ab$complexity.1
    con.smaller <- ab$consistency.1 < ab$consistency
    ab$consistency[con.smaller] <- ab$consistency.1[con.smaller]
    cov.smaller <- ab$coverage.1 < ab$coverage
    ab$coverage[cov.smaller] <- ab$coverage.1[cov.smaller]
    ord <- order(ab$complexity)
    ab <- ab[ord, , drop = FALSE]
    a <- ab[head_with_ties(ab$complexity, n), , drop = FALSE]
    a$complexity.1 <- a$consistency.1 <- a$coverage.1 <- NULL
  }
  a <- a[with(a, order(complexity, -consistency * coverage)), , drop = FALSE]
  selectedRows <- head_with_ties(cbind(a$complexity, 
                                       with(a, -consistency*coverage)), 
                                 n)
  a <- a[selectedRows, , drop = FALSE]
  n.out <- nrow(a)
  
  id <- a[grepl("^id", names(a))]
  allAsfs <- mapply(function(x, i) x$condition[i], splitasf, id,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  allAsfs <- C_relist_Char(do.call(rbind, allAsfs), rep(l, n.out))
  nms <- csf_representation(allAsfs)
  out <- data.frame(
    outcome = C_concat(names(splitasf), sep = ","),
    condition = nms,
    consistency = a$consistency, coverage = a$coverage, complexity = a$complexity,
    stringsAsFactors = FALSE)
  out <- cbind(out, .det.tti(tt.info(x$truthTab), out$condition, 
                             what = details, available = x$details))
  # insert treatment of csf with redundancies here...
  rownames(out) <- NULL
  if (n.out < n.csf) {
    message(n.out, " of the ", 
            n.csf, " complex solutions (csf) have been calculated.")
  }
  as.condTbl(out, condClass = "stdComplex")
}

# Auxiliary function expand.frames
expand.frames <- function(x, y){
  nx <- nrow(x)
  ny <- nrow(y)
  cbind(x[rep(seq_len(nx), each = ny), , drop = FALSE],
        y[rep(seq_len(ny), nx), , drop = FALSE])
}

# Auxiliary function head_with_ties
head_with_ties <- function(x, n){
  x <- as.matrix(x)
  if (nrow(x) <= n) {
    n <- nrow(x)
  }
  else {
    notDup <- which(!duplicated(x))
    if (all(notDup <= n)) {
      n <- nrow(x)
    }
    else {
      n <- min(notDup[notDup > n]) - 1L
    }
  }
  seq_len(n)
}
