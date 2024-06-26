
# remove blanks
noblanks <- function(x){
  # avoids loss of class attributes stdBoolean/stdAtomic/stdComplex
  if (inherits(x, c("stdBoolean", "stdAtomic", "stdComplex"))) return(x)
  gsub("[[:space:]]", "", as.character(x))
}
# extract lhs from formula
lhs <- function(x) as.vector(sub("([^<]+)<*->.+", "\\1", x))

# extract rhs from formula
rhs <- function(x){
  out <- rep("", length(x))
  twosided <- grepl("->", x, fixed = TRUE)
  out[twosided] <- sub(".+<*->(.+)", "\\1", x[twosided])
  as.vector(out)
}

# Extract asf strings from csf strings
#   condstr   string in 'visible' syntax with 'csf' format
# Returns a list of character vectors, where each list element corresponds to a csf
# and the elements therein are the asf
extract_asf <- function(x){
  noPar <- !grepl("^\\(", x)
  if (any(noPar)){
    x[noPar] <- paste0("(", x[noPar], ")")
  }
  hstrsplit(gsub("^\\(|\\)$", "", x), "\\)[\\*,]\\(", fixed = FALSE,
            split.attr = FALSE)
}
