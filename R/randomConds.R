
# randAsf: Determine a random asf
randomAsf <- function(x, outcome = NULL, maxVarNum = if (type == "mv") 8 else 16,
                      compl = NULL, how = c("inus", "minimal")){
  how <- match.arg(how)
  if (!is.data.frame(x) && !is.matrix(x)) x <- full.ct(x)
  if (!inherits(x, "configTable")){
    x <- configTable(x, rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)
  }
  type <- attr(x, "type")
  if (length(x) > maxVarNum){
    keep <- union(outcome, sample(names(x), maxVarNum - length(outcome)))
    x <- x[keep]
  }
  stopifnot(length(x) >= 3)
  if (!is.null(outcome)) stopifnot(length(outcome) == 1, outcome %in% names(x))
  
  if (is.null(compl)){
    compl <- pmin(ncol(x)-1, 4)
    if (compl>2) compl <- 2:compl
  } else {
    compl <- compl[compl <= length(x)-1]
    compl <- compl[compl >= 1]
  }
  if(length(compl) < 1) stop("Invalid specification of compl")

  # number and lengths of msc's
  if (length(compl)>1){  
    n.msc <- sample(compl, 1)
    len.msc <- sample(compl, n.msc, replace = TRUE)
  } else {
    n.msc <- compl
    len.msc <- rep(compl, n.msc)
  }
  
  # Formal preparations
  p <- ncol(x)
  cti <- ctInfo(x)
  if (type %in% c("cs", "fs")){
    vals <- mapply(c, cti$resp_nms, tolower(cti$resp_nms), 
                   USE.NAMES = TRUE, SIMPLIFY = FALSE)
  } else {
    nvals <- lengths(cti$uniqueValues)
    vals <- split(cti$resp_nms, rep(seq_len(p), nvals))
    names(vals) <- names(nvals)
  }
  
  # outcome (rhs)
  rhsFactor <- if (is.null(outcome)){
    sample(names(vals), 1)
  } else {
    outcome
  }
  rhs <- if (type %in% c("cs", "fs")){
    rhsFactor
  } else {
    sample(vals[[rhsFactor]], 1)  
  }
  
  # condition (lhs)
  vals1 <- vals[-match(rhsFactor, names(vals))]

  repeat {
    lhs.ini <- lapply(len.msc, msamp, vals1)
    lhs <- hconcat(list(lhs.ini), c("+", "*"))
    keepvars <- unlist(lhs.ini)
    keepvars <- if (type == "mv") sub("=.+$", "", keepvars) else toupper(keepvars)
    x4rreduce <- if (how == "inus") full.ct(x[unique(keepvars)]) else x
    # minimalize if no-inus
    lhs <- rreduce(lhs, x4rreduce, full = how == "inus")
    if (!lhs %in% c("0", "1")) break     # if resulting lhs-condition is a tautology -> repeat
  }
  structure(paste0(lhs, "<->", rhs), class = c("stdAtomic", "character"))
}

# Aux func
msamp <- function(i, v) vapply(sample(v, i), sample, 1, FUN.VALUE = character(1), USE.NAMES = FALSE)



# === randomCsf === 
# randCsf: Determine a random csf
randomCsf <- function(x, outcome = NULL, n.asf = NULL, compl = NULL, maxVarNum = if (type == "mv") 8 else 16){
  if (!is.data.frame(x) && !is.matrix(x)) x <- full.ct(x)
  if (!inherits(x, "configTable")){
    x <- configTable(x, type = "auto", rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)
  }
  type <- attr(x, "type")
  stopifnot(length(x) >= 4, length(maxVarNum) == 1)
  maxVarNum <- min(maxVarNum, length(x))

  # outcome, number and complexity of asf's
  if (is.null(outcome)){
    if (is.null(n.asf)) n.asf <- 2:pmin(ncol(x)-2, 4)
    if (length(n.asf) > 1) n.asf <- sample(n.asf, 1)
    outcome <- sample(names(x), n.asf)
  } else {
    if (length(outcome) > ncol(x)-2) 
      stop("The number of outcomes is limited to number of factors minus 2.")
    n.asf <- length(outcome)
  }
  if (is.null(compl)) compl <- 2:pmin(ncol(x)-1, 4)

  lhs_factors <- setdiff(names(x), outcome)
  outCsf <- ""
  for (i in seq_along(outcome)){
    if (i > 1) lhs_factors <- c(lhs_factors, outc)
    outc <- outcome[[i]]
    repeat{
      if (length(lhs_factors) <= maxVarNum){
        xx <- full.ct(x[c(lhs_factors, outc)])
      } else {
        # Case wide data: preselect a subset of factors for this asf before applying full.ct()
        used_factors <- outc
        if(nzchar(outCsf)){
          used_factors <- unique(
            c(used_factors, 
              extractFactors(extract_asf(outCsf)[[1]], type, split = c("<->", "+", "*"),
                             fixed = TRUE)))
        }
        if (length(used_factors) < maxVarNum){
          lhs_candidates <- setdiff(lhs_factors, used_factors)
          selected_factors <- if (length(lhs_candidates) <= maxVarNum - length(used_factors)){
            lhs_candidates
          } else {
            sample(lhs_candidates, maxVarNum - length(used_factors))
          }
        } else {
          selected_factors <- character(0)
        }
        xx <- full.ct(x[c(used_factors, selected_factors)])
      }
      if (nzchar(outCsf)){
        xx <- selectCases(outCsf, xx)
      }
      rasf <- randomAsf(xx, outcome = outc, compl = compl, how = "minimal")
      if (!(lhs(rasf) %in% c("0", "1"))) break
    }
    outCsf <- paste0(outCsf, if (nzchar(outCsf)) "*", "(", rasf, ")")
  }
  structure(outCsf, class = c("stdComplex", "character"))
}
