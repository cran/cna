# ==== auxiliary functions used in condition() ====
# and other stuff used for switching between expressions and strings



# ==== Operators in 'visible' and 'parsed' notation and 
#      function to switch between notations ====

# Operators in conditions as they are visible to the user in character strings
.opsVisible <- c("<->", "->", "<-",
                 "+", "*",
                 "<=", ">=", "<", ">", "!=", "=",
                 "!", "-")
# Operators in conditions as they are parsed by the R parser
.opsParsed <- c("=", "->", "<<-",
                "|", "&",
                "<=", ">=", "<", ">", "!=", "==",
                "-", "-")
# Operator lists used to manipulate strings expressing conditions
.opsNames <- c(":rlarr:", ":rarr:", ":larr:",
               ":or:", ":and:",
               ":le:", ":ge:", ":lt:", ":gt:", ":ne:", ":eq:",
               ":not1:", ":not2:")

# Function attributed to the ".opsParsed"-operators:
# -> Stored in the R-environment logicalOperators
opsNames <- c(
  "(",                                # parenthesis
  "<-", "=", "<<-",                   # conditions (->> for left-to-right assignment
  "|", "&", "-",                      # logical  ***NOTE: "-" used for negation (because its high priority in complex expresions)***
  ">", ">=", "<", "<=", "!=", "==")   # (un-)equality
opsList <- setNames(lapply(opsNames, match.fun), opsNames)
logicalOperators <- as.environment(opsList)  # unveraenderte Operatoren
parent.env(logicalOperators) <- emptyenv()
assign("<-", `>=`, envir = logicalOperators)
assign("<<-", `>=`, envir = logicalOperators)
assign("|", function(x, y){x[x<y] <- y[x<y]; x}, envir = logicalOperators)  # pmax
assign("&", function(x, y){x[x>y] <- y[x>y]; x}, envir = logicalOperators)  # pmin
assign("-", function(x) 1-x, envir = logicalOperators)
assign("=", `==`, envir = logicalOperators)
rm(opsNames, opsList)

# ------------------------------------------------------------------------------

# Utility function for translations between the "condition"-syntaxes
modifyStrings <- function(from, to, x){
  stopifnot(length(from) == length(to),
            is.character(from), is.character(to), is.character(x),
            anyDuplicated(from) == 0)
  ord <- order(nchar(from), decreasing = TRUE)
  from <- from[ord]
  to <- to[ord]
  for (i in seq_along(from))
    x <- gsub(from[[i]], .opsNames[[i]], x, fixed = TRUE)
  for (i in seq_along(from))
    x <- gsub(.opsNames[[i]], to[[i]], x, fixed = TRUE)
  x
}

# Translate between the two syntaxes
# visible (= vector of character string) to parsed (= list of quoted expressions)
visible2parsed <- function(x){
  x <- modifyStrings(.opsVisible, .opsParsed, x)
  as.list(parse(text = x, keep.source = FALSE))
}

# Auxiliary function tryparse(): parse while catching errors
tryparse <- function(x){
  x <- sub("**", "^", x, fixed = TRUE)
  tryCatch(visible2parsed(x)[[1]], error = function(e) NULL)
}

# parsed to visible
parsed2visible <- function(px){
  x <- C_concat(noblanks(deparse(invertArrow(px))), sep = "")
  x <- modifyStrings(.opsParsed[1:11], .opsVisible[1:11], x)
  gsub("<-([^>])", "->\\1", x)
}

# invert arguments around '->'
invertArrow <- function (x, inverted = FALSE){
  if (is.call(x)) {
    fn.name <- as.character(x[[1]])
    # invert "<-"
    if ((fn.name == "<-" || fn.name == "<<-") && !inverted){
      x <- x[c(1, 3, 2)]
      return(invertArrow(x, TRUE))
    }
    # recurse
    x[-1] <- lapply(x[-1], invertArrow)
    x
  } else {
    x
  }
}

# remove parantheses from a parsed expression
rm.parentheses <- function(px){
  if (is.call(px) && as.character(px[[1]]) == "("){
    rm.parentheses(px[[2]])
  } else {
    px
  }
}

# unify and switch case
up <- intToUtf8(65:90)
lo <- intToUtf8(97:122)
unifyCase <- function(x){
  hasUpper <- grepl("[[:upper:]]", x)
  x[hasUpper] <- chartr(lo, up, x[hasUpper])
  x
}
switchCase <- function(x){
  hasUpper <- grepl("[[:upper:]]", x)
  x[hasUpper] <- chartr(up, lo, x[hasUpper])
  x[!hasUpper] <- chartr(lo, up, x[!hasUpper])
  x
}


# reshaping of parsed calls 
reshapeCall <- function(x, type = "cs"){
  if (is.call(x)) {
    fn.name <- as.character(x[[1]])
    # remove unnecessary parentheses
    if (fn.name =="("){
      x <- rm.parentheses(x)
      return(reshapeCall(x))
    }
    # switch "case negation" to negation with operator "-"
    if (type != "mv" && fn.name == "-" && length(x) == 2 && is.name(x[[2]])){
      return(as.name(switchCase(as.character(x[[2]]))))
    }
    # recurse
    x[-1] <- lapply(x[-1], reshapeCall, type = type)
    x
  } else if (is.name(x)){
    # 'unify' case
    as.name(unifyCase(as.character(x)))
  } else if (is.atomic(x)){
    x
  } else {  # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}


# ==============================================================================

# getInfo functions
getInfo_bool <- function(x, freqs){
  if (is.list(x)){
    if (missing(freqs)) freqs <- attr(x, "n")
    sumf <- sum(freqs)
    x <- array(unlist(x, recursive = TRUE, use.names = TRUE),
               c(length(freqs), length(x)))
  } else {
    sumf <- sum(freqs)
  }
  sy <- colSums(x * freqs)
  data.frame(sy, freq = sy/sumf, sumf, row.names = NULL)
}
getInfo_asf <- function(x, freqs){
  resp <- attr(x, "response")
  if (is.list(x)){
    if (missing(freqs)) freqs <- attr(x, "n")
    sumf <- sum(freqs)
    x <- array(unlist(x, recursive = TRUE, use.names = TRUE),
               c(length(freqs), 2, length(x)))
  } else {
    sumf <- sum(freqs)
  }
  left <- matrix(x[, 1, , drop = TRUE], dim(x)[[1]])
  right <- matrix(x[, 2, , drop = TRUE], dim(x)[[1]])
  lrmin <- left
  lrmin[right<left] <- right[right<left]
  sx <- colSums(left * freqs)
  sy <- colSums(right * freqs)
  sxy <- colSums(lrmin * freqs)
  if (nrow(x) == 0){
    consistency <- coverage <- NA_real_
  } else {
    consistency <- sxy/sx
    coverage <- sxy/sy
  }
  data.frame(sx, sy, sxy, consistency, coverage, sumf, 
             outcome = resp, 
             row.names = NULL,
             stringsAsFactors = FALSE)
}
getInfo_csf <- function(x){
  attr(x, "conCov")
}
getInfo_customCsf <- function(x, lengths, freqs){
  resp <- happly(lapply(x, "names"), rhs, relist = FALSE)
  l <- length(x)
  structs <- lapply(x, attr, "complStruct")
  x <- array(unlist(x, recursive = TRUE, use.names = FALSE),
             c(length(freqs), 2, sum(lengths(x))))
  attr(x, "response") <- resp
  asfInf <- getInfo_asf(x, freqs)
  asfCons <- C_relist_Num(asfInf$consistency, lengths)
  asfCovs<- C_relist_Num(asfInf$coverage, lengths)

  conCov <- data.frame(matrix(NA_real_, l, 2))
  colnames(conCov) <- c("consistency", "coverage")
  for (i in seq_along(structs)){
    asfConCov <- data.frame(rbind(asfCons[[i]], asfCovs[[i]]))
    rownames(asfConCov) <- c("consistency", "coverage")
    names(asfConCov) <- paste0("atomicCond..", seq_along(asfConCov))
    conCov[i, ] <- eval(structs[[i]], asfConCov, enclos = logicalOperators)
  }
  conCov$asfCons <- asfCons
  conCov$asfCovs <- asfCovs
  conCov$outcome <- C_mconcat(C_relist_Char(asfInf$outcome, lengths), sep = ",")
  conCov
}


initializeInfo <- function(...){
  cl <- match.call()
  cl[[1]] <- quote(data.frame)
  cl$stringsAsFactors <- FALSE
  x <- eval.parent(cl)
  n <- nrow(x)
  x$sumf <- x$sxy <- x$sy <- x$sx <- x$complexity <- x$freq <- 
    x$coverage <- x$consistency <- emptyVector("double", n)
  x$outcome <- emptyVector("character", n)
  x$asfCovs <- x$asfCons <- emptyVector("list", n)
  x
}


# create an 'empty' vector of given type and length
emptyVector <- function(type, length){
  rep(switch(type, logical = NA, integer = NA_integer_, double = NA_real_, 
             complex = NA_complex_, character = NA_character_,
             list = list(NULL)),
      length)
}

updateInfo <- function(info, which, x){
  if (!any(which)) return(info)
  which <- which(which) # logigal index -> integer index
  stopifnot(length(which) == nrow(x), names(x) %in% names(info))
  info[which, names(x)] <- x
  info
}
