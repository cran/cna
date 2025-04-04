
measuresList <- structure(
  list(
    full_names = c("standard consistency", "standard coverage",
                   "contrapositive consistency", "contrapositive coverage", 
                   "prevalence-adjusted consistency", "antecedent-adjusted coverage",
                   "antecedent-adjusted contrapositive consistency", 
                   "prevalence-adjusted contrapositive coverage"),
    aliases = list(
      c("scon", "s-con"), c("scov", "s-cov"),
      c("ccon", "c-con"), c("ccov", "c-cov"), 
      c("PAcon", "PA-con"), c("AAcov", "AA-cov"),
      c("AACcon", "AAC-con"), c("PACcov", "PAC-cov"))),
  class = "measuresDefinition"
)

print.measuresDefinition <- function(x, indent = "", ...){
  nms <- x$full_names
  aliases <- lapply(x$aliases, function(a) paste0("\"", a, "\""))
  aliases <- C_mconcat(aliases, sep = ", ")
  writeLines(paste0(indent, nms, "\n", 
                    indent, "    alias", ifelse(lengths(x$aliases)!=1, "es", ""), 
                    ": ", aliases))
}

resolveMeasures <- function(m, imposeLength2 = TRUE){
  if (is.list(m) && 
      isTRUE(all.equal(names(m), c("measuresNames", "shortNames", "def")))){
    return(m)
  }
  m_in <- m
  m <- tolower(m)  # converts int to char in case of integer input
  defstrings <- mapply(c, measuresList$full_names, measuresList$aliases,
                       seq_along(measuresList$full_names), 
                       SIMPLIFY = FALSE, USE.NAMES = FALSE)
  ii <- pmatch(m, tolower(unlist(defstrings)), nomatch = 0, duplicates.ok = TRUE)
  if (imposeLength2){
    resolved <- FALSE
    if (length(m) == 2 && all(ii>0)){
      def <- rep(seq_along(defstrings), lengths(defstrings))[ii]
      resolved <- TRUE
    }
    if (!resolved){
      if (is.character(m_in)) m_in <- paste0("\"", m_in, "\"")
      stop("Invalid input of measures: c(", paste0(m_in, collapse = ","), ")", 
           call. = FALSE)
    }
    out <- list(measuresNames = measuresList$full_names[def], 
                shortNames = vapply(measuresList$aliases[def], "[[", 1, FUN.VALUE = character(1)),
                def = as.integer(def))
  } else {
    if (any(dupl <- duplicated(ii))){
      ii <- ii[!dupl]
      m <- m[!dupl]
      m_in <- m_in[!dupl]
    }
    l <- length(m)
    def <- rep(seq_along(defstrings), lengths(defstrings))[ii]
    out <- list(measuresNames = measuresList$full_names[def], 
                shortNames = vapply(measuresList$aliases[def], "[[", 1, FUN.VALUE = character(1)),
                def = as.integer(def))
    if (any(ii == 0)){
      if (is.character(m_in)) m_in <- paste0("\"", m_in, "\"")
      out$invalidInputs <- m_in[ii == 0]
      # => warning is issued elsewhere!!
    }
  }
  out
}
#' cna:::resolveMeasures(c("ccon", "ccov"))
#' cna:::resolveMeasures(c("standard con", "standard cov")) # incomplete matching
#' cna:::resolveMeasures(c(7, 8))
#' cna:::resolveMeasures(c("a", "b")); resolveMeasures(0:1)
#' cna:::resolveMeasures(1:8, imposeLength2 = FALSE)
#' cna:::resolveMeasures(1:9, imposeLength2 = FALSE)

prntMeasures <- function(measures){
  measNms <- format(measures$measuresNames)
  if (is.list(measures)){
    cat("Measures:\n", 
        "  con: ", measNms[1], "\n", 
        "  cov: ", measNms[2], "\n", 
        sep = "")
  }
}

# ------------------------------------------------------------------------------

showConCovMeasures <- function(){
  measuresList
}

showMeasures <- function(conCov = TRUE, details = TRUE){
  indent = "  "
  if (conCov){
    cat("Con/Cov measures:\n")
    print(measuresList, indent = indent)
    cat("\n")
  }
  if (details){
    cat("Detail measures:\n")
    writeLines(paste0(indent, .detailMeasures))
  }
}

