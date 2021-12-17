
# randAsf: Determine a random asf
randomAsf <- function(x, outcome = NULL, positive = TRUE, 
                      maxVarNum = if (type == "mv") 8 else 16,
                      compl = NULL, how = c("inus", "minimal")){
  how <- match.arg(how)
  if (!is.data.frame(x) && !is.matrix(x)) x <- full.ct(x)
  if (!inherits(x, "configTable")){
    x <- configTable(x, rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)
  }
  type <- attr(x, "type")
  if (!(length(outcome) %in% 0:1)){
    stop(sQuote("outcome"), " must have length 1")
  }
  if (type == "mv" && !missing(positive))
    message("Parameter 'positive' has no effect in case of mv data.")
  if (type %in% c("cs", "fs") && !is.null(outcome) && !missing(positive)){
    warning('positive=', positive, " is ignored, as outcome value has been specified.")
  }
  
  # select a subset of outcome factors (if necessary due to maxVarNum)
  if (length(x) > maxVarNum){
    outcomeFactors <- if (type %in% c("cs", "fs")){
      toupper(outcome)
    } else {
      sub("=.+$", "", outcome) 
    }
    keep <- union(outcomeFactors, sample(names(x), maxVarNum - length(outcome)))
    x <- x[keep]
  }
  if(length(x) < 3) stop("randomAsf() expects an 'x' with at least 3 factors.")
  cti <- ctInfo(x)

  # outcome, factors and values
  outcome <- if (type == "mv"){
    checkOutcome(outcome, c(colnames(cti$scores), names(x)))
  } else {
    checkOutcome(outcome, colnames(cti$scores))
  }

  vals <- getvals(cti)
  outcome <- determine_outcomes(outcome, vals, type = type, positive = positive)
  outcomeFactor <- if (type %in% c("cs", "fs")){
    toupper(outcome)
  } else {
    sub("=.+$", "", outcome) 
  }

  # compl (complexity of condition)
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
  vals <- vals[-match(outcomeFactor, names(vals))]
  repeat {
    lhs.ini <- lapply(len.msc, msamp, x = vals)
    lhs <- hconcat(list(lhs.ini), c("+", "*"))
    keepvars <- unlist(lhs.ini)
    keepvars <- if (type == "mv") sub("=.+$", "", keepvars) else toupper(keepvars)
    x4rreduce <- if (how == "inus") full.ct(x[unique(keepvars)]) else x
    # minimalize if no-inus
    lhs <- rreduce(lhs, x4rreduce, full = how == "inus")
    if (!lhs %in% c("0", "1")) break     # if resulting lhs-condition is a tautology -> repeat
  }
  structure(paste0(lhs, "<->", outcome), class = c("stdAtomic", "character"))
}


# Aux funcs
getvals <- function(cti){
  if (cti$type %in% c("cs", "fs")){
    vals <- mapply(c, cti$resp_nms, tolower(cti$resp_nms), 
                   USE.NAMES = TRUE, SIMPLIFY = FALSE)
  } else {
    vals <- mapply(paste, names(cti$uniqueValues), cti$uniqueValues, sep = "=", 
                   SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }
  vals
}
determine_outcomes <- function(outcome, vals, type, positive, n = 1){
  outcome_miss <- length(outcome) == 0
  if (outcome_miss) outcome <- msamp(vals, n)
  if (type == "mv" && any(fnm <- (toupper(outcome) %in% names(vals))))
    outcome[fnm] <- msamp(vals[toupper(outcome[fnm])], sum(fnm))
  if (type %in% c("cs", "fs") && outcome_miss && positive) 
    outcome <- toupper(outcome)
  outcome
}

# Take a sample of size n from list x and sample one element from each selected list elnement
msamp <- function(x, n) vapply(sample(x, n), sample, 1, FUN.VALUE = character(1), USE.NAMES = FALSE)
# (x <- split(LETTERS, sample(1:5, 26, T)))
# msamp(x, 3)



# === randomCsf === 
# randCsf: Determine a random csf
randomCsf <- function(x, outcome = NULL, positive = TRUE, 
                      n.asf = NULL, compl = NULL, maxVarNum = if (type == "mv") 8 else 16){
  if (!is.data.frame(x) && !is.matrix(x)) x <- full.ct(x)
  if (!inherits(x, "configTable")){
    x <- configTable(x, type = "auto", rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)
  }
  type <- attr(x, "type")
  if (type == "mv" && !missing(positive))
    message("Parameter 'positive' has no effect in case of mv data.")
  if (type %in% c("cs", "fs") && !is.null(outcome) && !missing(positive)){
    warning('positive=', positive, " is ignored, as outcome value", 
            if (length(outcome)>1) "s have" else " has", 
            " been specified.")
  }
  
  if(length(x) < 4) stop("randomCsf() expects 'x' with at least 4 factors.")
  maxVarNum <- min(maxVarNum, length(x))

  # outcome, number and complexity of asf's
  if (is.null(outcome)){
    if (is.null(n.asf)) n.asf <- 2:pmin(ncol(x)-2, 4)
    if (length(n.asf) > 1) n.asf <- sample(n.asf, 1)
  } else {
    if (length(outcome) > ncol(x)-2) 
      stop("The number of outcomes is limited to number of factors minus 2.")
    n.asf <- length(outcome)
  }
  cti <- ctInfo(x)
  
  # check 'outcome' input
  outcome <- if (type == "mv"){
    checkOutcome(outcome, c(colnames(cti$scores), names(x)))
  } else {
    checkOutcome(outcome, colnames(cti$scores))
  }
  outcome <- determine_outcomes(outcome, vals = getvals(cti), type = cti$type, 
                                positive = positive, n = n.asf)

  outcomeFactors <- if (type %in% c("cs", "fs")){
    toupper(outcome)
  } else {
    sub("=.+$", "", outcome) 
  }
  if (anyDuplicated(outcomeFactors)){
    stop("Invalid ", sQuote("outcome"), ": factors are not allowed to appear repeatedly.")
  }
  if (is.null(compl)) compl <- 2:pmin(ncol(x)-1, 4)

  lhs_factors <- setdiff(names(x), outcomeFactors)
  outCsf <- ""
  for (i in seq_along(outcome)){
    if (i > 1) lhs_factors <- c(lhs_factors, outcomeFactor)
    outc <- outcome[[i]]
    outcomeFactor <- outcomeFactors[[i]]

    repeat{
      if (length(lhs_factors) <= maxVarNum){
        xx <- full.ct(x[c(lhs_factors, outcomeFactor)])
      } else {
        # Case wide data: preselect a subset of factors for this asf before applying full.ct()
        used_factors <- outcomeFactor
        if(nzchar(outCsf)){
          used_factors <- unique(
            c(used_factors, 
              extractFactors(extract_asf(outCsf)[[1]], type, split = c("<->", "+", "*"),
                             fixed = TRUE)))
        }
        if (length(used_factors) < maxVarNum){
          lhs_candidates <- setdiff(lhs_factors, used_factors)
          if (length(lhs_candidates) <= maxVarNum - length(used_factors)){
            selected_factors <- lhs_candidates
          } else {
            selected_factors <- sample(lhs_candidates, maxVarNum - length(used_factors))
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
