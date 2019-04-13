
# build dnf 
#  Note that tt is supposed to be a "full" tt
make.dnf <- function(expr, tt, disj = "+"){
  if (expr %in% c("0", "1")) return(expr)
  selC <- selectCases(expr, tt)
  if (nrow(selC) == 0L) return("0")
  if (nrow(selC) == nrow(tt)) return("1")
  sc <- tt.info(selC)$scores
  terms <- split(rep(colnames(sc), each = nrow(sc))[sc == 1],
                 row(sc)[sc == 1])
  conj <- C_mconcat(terms, "*")
  conj <- conj[is.minimal(conj)]
  C_concat(conj, disj)
}
# Minimize a single condition
#  Note that x is supposed to be a "full" tt
.minim1 <- function(cond, x, maxstep = c(4, 4, 12)){
  cond <- make.dnf(cond, x)
  if (cond %in% c("0", "1")) return(cond)
  y <- as.vector(qcond_bool(cond, tt.info(truthTab(x))$scores))
  if (isConstant(y)) return(as.character(y[[1]]))
  x$..RESP.. <- y
  suppressMessages({
    .cna <- cna(x, ordering = list("..RESP.."), strict = TRUE, 
                maxstep = maxstep, rm.const.factors = FALSE, rm.dup.factors = FALSE)
  })
  .asf <- asf(.cna)
  if (attr(x, "type") == "mv"){
    .asf <- subset(.asf, .asf$outcome == "..RESP..=1")
  }
  lhs(.asf$condition)
}
# Minimize multiple conditions
minimalize <- function(cond, x = NULL, maxstep = c(4, 4, 12)){
  cond <- noblanks(cond)
  if (is.null(x)){
    x <- full.tt(cond)
  } else {
    x <- full.tt(x)
  }
  tti <- tt.info(x)
  out <- vector("list", length(cond))
  names(out) <- cond
  # check for disjunctive normal form
  dnf <- checkValues(cond, c("+", "*"), colnames(tti$scores))
  cond1 <- cond
  cond1[!dnf] <- vapply(cond[!dnf], make.dnf, x,
                        FUN.VALUE = character(1), USE.NAMES = FALSE)
  out[] <- lapply(cond1, .minim1, x = x, maxstep = maxstep)
  if (any(noOutput <- lengths(out) == 0))
    warning("No minimal solution found for condition(s):\n",
            paste0("  ", cond[noOutput], "\n"),
            "You may try to increase maxstep.", call. = FALSE)
  out
}
