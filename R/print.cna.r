
# print method for class cna
print.cna <- function(x, what = x$what, digits = 3, nsolutions = 5, 
                      details = x$details, show.cases = NULL, ...){
  cat("--- Coincidence Analysis (CNA) ---\n")
 # cat("\nFunction call:\n", deparse(x$call), "\n", sep = "")

  what <- tolower(what)
  if (what == "all"){
    whatl <- rep(TRUE, 4)
  } else {
    whatl <- vapply(c("t", "m", "a", "c"), grepl, logical(1), what, fixed = TRUE)
  }
  names(whatl) <- c("t", "m", "a", "c")

  if (nsolutions == "all") nsolutions <- Inf
  
  if (whatl["t"]){
    cat("\nTruth table:\n")
    print(x$truthTab_out, show.cases = show.cases)
  }
  if (!is.null(x$ordering))
    cat("\nCausal ordering",
        if(!is.null(x$call$strict) && eval(x$call$strict)) " (strict)", ":\n",
        C_concat(C_mconcat(x$ordering, sep = ", "), sep = " < "),
        "\n", sep = "")
  else cat("\nFactors:", C_concat(names(x$truthTab), sep = ", "), "\n")

  if (whatl["m"]){
    msc.df <- msc(x)
    cat("\nMinimally sufficient conditions:\n",
        "--------------------------------", sep = "")
    if (nrow(msc.df) == 0){
      cat("\n*none*\n")
    } else for (msc1 in split(msc.df, msc.df$outcome)){
      cat("\nOutcome ", msc1$outcome[1], ":\n", sep = "")
      if (short <- ((nsol <- nrow(msc1)) > nsolutions))
        msc1 <- msc1[seq_len(nsolutions), , drop = FALSE]
      names(msc1)[names(msc1) == "condition"] <- "solution"
      print(msc1[-1], digits = digits, row.names = FALSE, ...)
      if (short)
        cat(" ... (total no. of conditions: ", nsol, ")\n", sep = "")
    }
  }

  if (any(whatl[c("a", "c")]))
    asf.df <- asf(x, details)

  if (whatl["a"]){
    cat("\nAtomic solution formulas:\n",
        "-------------------------", sep = "")
    if (nrow(asf.df) == 0) cat("\n*none*\n")
    else for (asf1 in split(asf.df, asf.df$outcome)){
      cat("\nOutcome ", asf1$outcome[1], ":\n", sep = "")
      if (short <- ((nsol <- nrow(asf1)) > nsolutions))
        asf1 <- asf1[seq_len(nsolutions), , drop = FALSE]
      names(asf1)[names(asf1) == "condition"] <- "solution"
      print(asf1[-1], digits = digits, row.names = FALSE, ...)
      if (short)
        cat(" ... (total no. of formulas: ", nsol, ")\n", sep = "")
    }
  }

  if (whatl["c"]){
    cat("\nComplex solution formulas:\n",
        "--------------------------\n", sep = "")
    if (nrow(asf.df) == 0){
      n.csf <- 0L
    } else {
      n.csf.by.outcome <- vapply(split(asf.df, asf.df$outcome), nrow, integer(1L))
      n.csf <- prod(vapply(split(asf.df, asf.df$outcome), nrow, integer(1L)))
    }  
    if (n.csf == 0){
      cat("*none*\n")
    } else if (length(n.csf.by.outcome) == 1L && whatl["a"]){
      cat("Same as asf\n")
    } else {
      csf1 <- csf(x, n = nsolutions, asfx = asf.df, details = details)
      if (short <- ((nsol <- nrow(csf1)) > nsolutions))
        csf1 <- csf1[seq_len(nsolutions), , drop = FALSE]
      names(csf1)[names(csf1) == "condition"] <- "solution"
      print(csf1, digits = digits, row.names = FALSE, ...)
      if (short)
        cat(" ... (total no. of formulas: ", nsol, ")\n", sep = "")
    }
  }

  invisible(x)
}
