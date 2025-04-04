
# print method for class cna
print.cna <- function(x, what = x$what, digits = 3, nsolutions = 5, 
                      printMeasures = TRUE, details = x$details, 
                      show.cases = NULL, verbose = FALSE, ...){
  cat("--- Coincidence Analysis (CNA) ---\n")
 # cat("\nFunction call:\n", deparse(x$call), "\n", sep = "")
  whatl <- vapply(c("t", "m", "a", "c"), grepl, what, fixed = TRUE, 
                  FUN.VALUE = logical(1), USE.NAMES = TRUE)
  if (nsolutions == "all") nsolutions <- Inf
  
  if (whatl["t"]){
    cat("\nConfiguration table:\n")
    printPseudoConfigTable(x, show.cases = show.cases)
  }
  if (!is.null(x$ordering))
    cat("\nCausal ordering",
        if(!is.null(x$call$strict) && eval(x$call$strict)) " (strict)", ":\n",
        C_concat(C_mconcat(x$ordering, sep = ", "), sep = " < "),
        "\n", sep = "")
  else cat("\nFactors:", C_concat(names(x$configTable), sep = ", "), "\n")

  if (!is.null(outc <- eval(x$call$outcome))){
    cat("Outcome", if (length(outc)>1) "s", ": ", paste0(outc, collapse = ", "), 
        "\n", sep = "")
  }
  
  if (printMeasures) prntMeasures(x$measures)
  details <- resolveDetails(details)
  
  if (whatl["m"]){
    msc.df <- msc(x, details)
    cat("\nMinimally sufficient conditions:\n",
        "--------------------------------", sep = "")
    if (nrow(msc.df) == 0){
      cat("\n*none*\n")
    } else for (msc1 in split(msc.df, msc.df$outcome)){
      cat("\nOutcome ", msc1$outcome[1], ":\n", sep = "")
      names(msc1)[names(msc1) == "condition"] <- "solution"
      has_minimal <- "minimal" %in% names(msc1)
      prntCols <- c(
        "solution", "con", "cov", "complexity", if (has_minimal) "minimal",
        intersect(getDetailCols(details), names(msc.df))) 
      print(msc1[prntCols], n = nsolutions, digits = digits, 
            printMeasures = FALSE, row.names = FALSE, ...)
    }
  }

  if (any(whatl[c("a", "c")]))
    asf.df <- asf(x, details = addDetailMeasure(details, "inus"))

  if (whatl["a"]){
    cat("\nAtomic solution formulas:\n",
        "-------------------------", sep = "")
    if (nrow(asf.df) == 0){
      cat("\n*none*\n")
    } else for (asf1 in split(asf.df, asf.df$outcome)){
      cat("\nOutcome ", asf1$outcome[1], ":\n", sep = "")
      names(asf1)[names(asf1) == "condition"] <- "solution"
      has_minimal <- "minimal" %in% names(asf1)
      prntCols <- c(
        "solution", "con", "cov", "complexity", if (has_minimal) "minimal",
        intersect(getDetailCols(details), names(asf1))) 
      print(asf1[prntCols], n = nsolutions, digits = digits, 
            printMeasures = FALSE, row.names = FALSE, ...)
    }
  }

  if (whatl["c"]){
    cat("\nComplex solution formulas:\n",
        "--------------------------\n", sep = "")
    if (nrow(asf.df) == 0){
      n.csf <- 0L
      cat("*none*\n")
    } else if (length(unique(asf.df$outcome)) == 1L && whatl["a"]){
      cat("Same as asf\n")
    } else {
      csf1 <- csf(x, asfx = asf.df, details = details,
                  inus.only = x$control$inus.only, inus.def = x$control$inus.def, 
                  acyclic.only = x$acyclic.only, cycle.type = x$cycle.type,
                  verbose = verbose)
      n.csf <- nrow(csf1)
      if (n.csf == 0){
        cat("*none*\n")
      } else {
        names(csf1)[names(csf1) == "condition"] <- "solution"
        print(csf1, n = nsolutions, digits = digits, 
              printMeasures = FALSE, row.names = FALSE, ...)
      }
    }  
  }
  invisible(x)
}


# Note: The 'configTable'  that is printed if 'what' contains an "t" is not a proper cna object
# if there are some 'notcols'.
printPseudoConfigTable <- function(x, show.cases = TRUE){
  if (length(x$notcols)){
    ct0 <- x$configTable
    notcols.nrs <- names(ct0) %in% x$notcols
    names(ct0)[notcols.nrs] <- tolower(names(ct0)[notcols.nrs])
    ctout <- as.data.frame(ct0, warn = FALSE)
    ctout[notcols.nrs] <- lapply(ctout[notcols.nrs], function(x) 1-x)
    attributes(ctout)[c("names", "row.names", "class", "n", "cases", "type")] <-
      attributes(ct0)[c("names", "row.names", "class", "n", "cases", "type")]
  } else {
    ctout <- x$configTable
  }
  print(ctout, show.cases = show.cases)
}
