
# ==== Generic function condition ====
condition <- function(x, ...)
  UseMethod("condition")

# ------------------------------------------------------------------------------

# ==== condition.default ====
# entirely new
# now able to handle the types "cs", "mv", "fs"
condition.default <- function(x, tt, type, force.bool = FALSE, rm.parentheses = FALSE, ...){
  if (inherits(tt, "truthTab")){
    type <- attr(tt, "type")
  } else {
    if (missing(type)) type <- "cs"    # "hidden" Default value!
    tt <- truthTab(tt, type = type)
  }
  # use lower case negation in conditions' syntax?
  negateCase <- type %in% c("cs", "fs")
  # remove blanks
  # x <- gsub(" ", "", x, fixed = TRUE)
  # parse expresions
  px <- visible2parsed(x)
  if (rm.parentheses) px[] <- lapply(px, rm.parentheses)
  # determining types of conditions
  condTypes <- .condType(px, force.bool)
  # split conditions into one or two parts accordingly
  out <- vector("list", length(px))
  if (any(bool <- (condTypes == "boolean"))){
    out[bool] <- booleanCond(px[bool], tt = tt, negateCase = negateCase)
  }
  if (any(atomic <- (condTypes == "atomic"))){
    out[atomic] <- atomicCond(px[atomic], tt = tt, negateCase = negateCase)
  }
  if (any(complx <- (condTypes == "complex"))){
    for (i in which(complx)){
      out[[i]] <- complexCond(px[[i]], tt = tt, negateCase = negateCase)
    }
  }
  out <- lapply(out, function(x){ 
    attributes(x) <- c(attributes(x), getInfo(x))
    x
  })
  class(out) <- "listof"
  names(out) <- vapply(out, attr, character(1), "condName")
  out
}

# ==== auxiliary functions  ====
# and other stuff used for switching between expresions and strings

# condtype
# determines the type of a condition
# px   quoted string
# value  a character string: "boolean", "atomic" or "complex"
.condType <- function(px, force.bool, ops = c("<-", "=", "<<-")){
  if (force.bool) return(rep("boolean", length(px)))
  outerOps <- vapply(px, function(x) if (is.call(x)) as.character(x[[1L]]) else "", character(1)) 
  cntAssOp <- vapply(px, .containsAssignOp, logical(1), ops)
  condType <- character(length(px))
  condType[outerOps %in% ops] <- "atomic"
  condType[condType != "atomic" & cntAssOp & outerOps != "("] <- "complex"
  condType[condType != "atomic" & condType != "complex"] <- "boolean"
  condType
}


# .containsAssignOp
# determinesw whether an expression contains an assignment operator
# x   quoted string
# value: logical
# Used in .condType
.containsAssignOp <- function(x, ops = c("<-", "<<-", "=")){
  if (is.atomic(x) || is.name(x)) {
    FALSE
  } else if (is.call(x)) {
    fn.name <- as.character(x[[1]])
    if (fn.name %in% ops){
      TRUE
    } else {
      any(vapply(x[-1], .containsAssignOp, logical(1)))
    }
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}


# Operator lists used to manipulate strings expressing conditions
.opsNames <- c(":rlarr:", ":rarr:", ":larr:",
               ":or:", ":and:",
               ":le:", ":ge:", ":lt:", ":gt:", ":ne:", ":eq:",
               ":not1:", ":not2:")
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
# parsed to visible
parsed2visible <- function(x){
  x <- paste0(deparse(x), collapse = "")
  x <- gsub(" ", "", x, fixed = TRUE)
  x <- modifyStrings(.opsParsed[1:11], .opsVisible[1:11], x)
  addblanks(x)
}

# remove parantheses from a parsed expression
rm.parentheses <- function(px){
  if (is.call(px) && as.character(px[[1]]) == "("){
    rm.parentheses(px[[2]])
  } else {
    px
  }
}

# Add blanks before and after given strings (default: <->, -> and <-)
# Args:
#   x       character vector to be modified
#   string  character/regular expression: strings
#   ...     passed to gsub
# value: character vector
#addblanks <- function(x, string = "([\\<]{0,1}\\-[\\>]{0,1})", ...)
#  gsub(string, " \\1 ", x, ...)
addblanks <- function(x, ...){
  x <- gsub("(<->)", " \\1 ", x, ...)
  x <- gsub("([^<])(->)", "\\1 \\2 ", x, ...)
  gsub("(<-)([^>])", " \\1 \\2", x, ...)
}
#addblanks(c("a->b", "a<-b", "a<->b", "a-b"))



# ------------------------------------------------------------------------------

# ==== ...Cond functions: handling the 3 types of conds (boolean, atomic, complex) ====
# take a quoted string x and a truthTable tt
# returns a cond object

# cond, "boolean" case
booleanCond <- function(x, tt, negateCase){
  nms <- vapply(x, parsed2visible, character(1))
  if (negateCase) {
    x[] <- lapply(x, applyCaseNegation, names(tt))
    nms <- removeCaseNegation(nms)
  } else {
    x[] <- lapply(x, setVarNms, names(tt))
  }
  # evaluate
  x <- lapply(x, eval, env = tt, enclos = logicalOperators)
  datatype <- switch(attr(tt, "type"),
                     cs = "integer",
                     mv = "integer",
                     fs = "numeric")
  x <- lapply(x, "mode<-", datatype)
  x <- lapply(x, as.data.frame, optional = TRUE)
  x <- mapply(`names<-`, x, nms, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  x <- lapply(x, structure, 
              class = c("booleanCond", "cond", "data.frame"),
              cases = attr(tt, "cases"),
              n = attr(tt, "n"))
  x <- mapply(`attr<-`, x, "condName", nms, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  names(x) <- nms
  x
}

# cond, "atomic" case
atomicCond <- function(x, tt, negateCase){
  rel <- vapply(x, function(cond) as.character(cond[[1]]),
                character(1))
  x <- rapply(x, function(cond) as.list(cond)[2:3], how = "replace")
  pnms <- rapply(x, parsed2visible, how = "replace")
  rel[rel %in% c("<-", "<<-")] <- " -> "
  rel[rel == "="] <- " <-> "
  if (any(rel == " -> ")){
    x[rel == " -> "] <- lapply(x[rel == " -> "], rev.default)
    pnms[rel == " -> "] <- lapply(pnms[rel == " -> "], rev.default)
  }
  if (negateCase){
    x[] <- rapply(x, applyCaseNegation, vnms = names(tt), how = "replace")
    pnms <- rapply(pnms, removeCaseNegation, how = "replace")
  } else {
    x[] <- rapply(x, setVarNms, names(tt), how = "replace")
  }
  # evaluate
  x <- rapply(x, eval, env = tt, enclos = logicalOperators,
              how = "replace")
  datatype <- switch(attr(tt, "type"), 
                     cs = "integer",
                     mv = "integer",
                     fs = "numeric")
  x <- rapply(x, `mode<-`, how = "replace", value = datatype)
  x <- mapply(`names<-`, x, pnms, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  x <- lapply(x, as.data.frame, optional = TRUE)
  x <- lapply(x, structure, 
              class = c("atomicCond", "cond", "data.frame"),
              cases = attr(tt, "cases"),
              n = attr(tt, "n"))
  pnms <- paste0(sapply(pnms, "[[", 1), rel, sapply(pnms, "[[", 2))
  x <- mapply(`attr<-`, x, "condName", pnms, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  names(x) <- pnms
  x
}

# cond, "complex" case
complexCond <- function(x, tt, negateCase){
  strct <- .call2list(x)
  atConds <- rapply(strct, function(x) x, classes = c("call", "<-", "="), how = "unlist")
  cntr <- .counter()
  cntr$init()
  strct <- rapply(strct, function(x) as.name(paste0("atomicCond..", cntr$increment())), 
                  classes = c("call", "<-", "="), how = "replace")
  restoredStruct <- .list2call(strct)
  cc <- atomicCond(atConds, tt, negateCase = negateCase)
  attr(cc, "complStruct") <- restoredStruct
  class(cc) <- c("complexCond", "cond")
  names(cc) <- sapply(cc, attr, "condName")
  attr(cc, "condName") <- modifyStrings(
    paste0("atomicCond..", seq_along(atConds)),
    names(cc),
    parsed2visible(restoredStruct))
  attr(cc, "cases") <- attr(tt, "cases")
  attr(cc, "n") <- attr(tt, "n")
  cc
}

# ==== Auxiliary functions used in booleanCond and atomicCond ====

# Function to translate lower case negation into R-syntax
# (see Wickham, "Advanced R", section 14.7.)
# takes and returns a language object
# Names are rendered either as in vnms (positive value; the variable names in the truthTab) or 
# as lowercase (negative value)
applyCaseNegation <- function(x, vnms, char = .opsParsed[match(":not1:", .opsNames)]) {
  if (is.atomic(x)){
    x
  } else if (is.name(x)){
    xchar <- as.character(x)
    if (tolower(xchar) != toupper(xchar)){
      # Name of variable as it appears in vnms:
      posName <- grep(paste0("^", xchar, "$"), vnms, value = TRUE, ignore.case = TRUE)
      if (posName == tolower(posName)) posName <- toupper(posName)
      if (xchar == tolower(xchar)){
        parse(text = paste0(char, posName), keep.source = FALSE)[[1]]
      } else {
        as.name(posName)
      }
    } else {
      x
    }
  } else if (is.call(x)){
    x[] <- lapply(x, applyCaseNegation, vnms, char = char)
    x
  } else {
    # User supplied incorrect input
    stop("applyCaseNegation is not applicable to type ", typeof(x))
  }
}
# applyCaseNegation(quote(A + b*c), vnms = LETTERS[1:3])
# applyCaseNegation(quote(A + b*c), vnms = LETTERS[1:3], char = "!")
# applyCaseNegation(quote(Ad + be*Cf), vnms = c("Ad", "Be", "CF"))
# applyCaseNegation(quote(Ad + be*Cf), vnms = c("Ad", "Be", "cf"))

# setVarNms: sets the variable names to their name in vnm (wrt case)
# Used in booleanCond and atomicCond
setVarNms <- function(x, vnms) {
  if (is.atomic(x)){
    x
  } else if (is.name(x)){
    xchar <- as.character(x)
    if (tolower(xchar) != toupper(xchar)){
      # Name of variable as it appears in vnms:
      Name <- grep(paste0("^", xchar, "$"), vnms, value = TRUE, ignore.case = TRUE)
      as.name(Name)
    } else {
      x
    }
  } else if (is.call(x)){
    x[] <- lapply(x, setVarNms, vnms)
    x
  } else {
    # User supplied incorrect input
    stop("setVarNms is not applicable to type ", typeof(x))
  }
}
# setVarNms(quote(haNs + klaus + FRITZ), c("Hans", "Klaus", "Fritz"))
# setVarNms(quote((haNs=1) + (klaus=3) + (FRITZ=5)), c("Hans", "Klaus", "Fritz"))

# removeCaseNegation
# - takes and returns a character string
# - modifies only negations applied to a simple name
removeCaseNegation <- function(x){
  grxp <- gregexpr("-[A-Z]+", x)[[1L]]
  if (grxp[[1]] > 0){
    end <- c(grxp) + attr(grxp, "match.length") - 1L
    for (i in seq_along(grxp)){
      substr(x, c(grxp)[i], end[i]) <-
        paste0("\r", tolower(substr(x, c(grxp)[i] + 1L, end[i])))
    }
  }  
  gsub("\r", "", x)
}  
#removeCaseNegation("-B")
#removeCaseNegation("A + -B*-C")
#removeCaseNegation("C <-> D*-B+-ABC")


# ==== Auxiliary functions used in complexCond ====

# .listIndices
# finds the indices required to extract all non-list elements of a (typically nested) list
# value: a list of integer vectors
# Used in complexCond
# sapply(.listIndices(mylist), function(x) mylist[[x]]) is essentially the same as 
# unlist(mylist, use.names = FALSE)
#.listIndices <- function(x){
#  if (!is.list(x) && !is.vector(x)) return(list(NULL))
#  if (!is.list(x)) return(as.list(seq_along(x)))
#  recurs <- lapply(x, .listIndices)
#  mapply(c,
#         rep(seq_along(x), sapply(recurs, length)),
#         unlist(recurs, recursive = FALSE, use.names = FALSE),
#         SIMPLIFY = FALSE, USE.NAMES = FALSE)
#}
# mylist <- list(1:5, letters[1:3], quote(sin(pi/2)), sqrt)
# sapply(.listIndices(mylist), function(x) mylist[[x]])

# convert a call into a (nested) list (abstract syntax tree)
# calls von stopOps werden nicht weiter zerlegt
# Used in complexCond
.call2list <- function(x, stopOps = c("<-", "<<-", "="), validOps = c("&", "|", "(", "-")){
  if (is.call(x)) {
    fn.name <- as.character(x[[1]])
    if (fn.name %in% stopOps){
      x
    } else {
      if (!is.null(validOps) && !(fn.name %in% validOps)) 
        stop("Invalid condition: ", deparse(x))
      x[-1] <- lapply(x[-1], .call2list, stopOps = stopOps, validOps = validOps)
      as.list(x)
    }
  } else {
    x
  }
}
# .call2list(quote(A&B | -C), NULL)

# convert a (nested) list into a call
# Used in complexCond
.list2call <- function(x, ops = c("&", "|", "(", "-")){
  if (is.list(x)) {
    fn.name <- as.character(x[[1]])
    if (fn.name %in% ops){
      x[-1] <- lapply(x[-1], .list2call, ops = ops)
      as.call(x)
    } else {
      as.call(x)
    }
  } else {
    x
  }
}
# .list2call(.call2list(quote(A&B | -C), NULL))

.counter <- function(){
  list(init = function() assign("i", 0L, parent.env(environment())),
       increment = function(){
         env <- parent.env(environment())
         assign("i", get("i", env) + 1L, env)
       }
  )
}

# ------------------------------------------------------------------------------


# ==== print method for "cond" (used for booleanCond and complexCond) ====

print.cond <- function(x, digits = 3, print.table = TRUE, 
          show.cases = NULL, ...){
  cat("type of condition:",  
      switch(class(x)[[1]], 
        atomicCond = "atomic", 
        booleanCond = "boolean"), 
      "\n")                     
  if (print.table){
    dfr <- data.frame(x, n.obs = attr(x, "n"), check.names = FALSE)
    rownames(dfr) <- sapply(lapply(attr(x, "cases"), sort), paste0, collapse = ",")
    if (is.null(show.cases)) show.cases <- max(nchar(rownames(dfr))) <= 20
    print.data.frame(dfr, row.names = show.cases, ...)
  }
  info <- attr(x, "info")
  if (inherits(x, "atomicCond")) {
    cat("Consistency: ", formatC(attr(x, "consistency"), 
      format = "f", digits = digits), " (", info["szy"], 
      "/", info["sy"], ")\n", "Coverage:    ", formatC(attr(x, 
        "coverage"), format = "f", digits = digits), 
      " (", info["szy"], "/", info["sz"], ")\n", "Total no. of cases: ", 
      info["sumf"], "\n", sep = "")
    if (!is.null(nu <- attr(x, "n.unique")) && !is.null(uc <- attr(x, 
      "unique.coverage"))) 
      cat(paste(format(rep(c("Unique Coverages: ", ""), 
        c(1, length(nu) - 1))), format(names(uc)), " : ", 
        formatC(uc, format = "f", digits = digits), " (", 
        nu, "/", info["sz"], ")", sep = ""), sep = "\n")
  }
  else cat("Frequency:   ", formatC(attr(x, "freq"), format = "f", 
    digits = digits), " (", info["sy"], "/", info["sumf"], 
    ")\n", sep = "")
  invisible(x)
}

# ==== print method for "complexCond" ====
print.complexCond <- function(x, digits = 3, print.table = TRUE,
                              show.cases = NULL, ...){
  cat("type of condition:", "complex\n")
  if (print.table){
    dfr <- do.call(data.frame, c(x, list(attr(x, "n"))))
    names(dfr) <- c(unlist(lapply(x, names), use.names = FALSE), "n.obs")
    rownames(dfr) <- sapply(lapply(attr(x, "cases"), sort), paste0, collapse = ",")
    if (is.null(show.cases)) show.cases <- max(nchar(rownames(dfr))) <= 20
    print.data.frame(dfr, row.names = show.cases, ...)
  }  
  cat("Consistency: ", 
      formatC(attr(x, "consistency"), format = "f", digits = digits), "\n",
      "Coverage:    ", 
      formatC(attr(x, "coverage"), format = "f", digits = digits), "\n", 
      "Total no. of cases: ", sum(attr(x, "n")), "\n", sep = "")
  cinf <- attr(x, "componentsInfo")
  cat("Consistency and coverage of components:\n")
  conCov <- t(sapply(cinf, function(x) unlist(x[c("consistency", "coverage")], use.names = TRUE)))
  print.default(conCov, digits = digits)
  invisible(x)
}

# method for class condTbl
condition.condTbl <- function(x, tt, ...)
  condition.default(x[["condition"]], tt, ...)

# ------------------------------------------------------------------------------

# ==== auxiliary function getInfo ====
# extracts consistency, coverage and other features from a "cond"-objects and a truth table
# and stores them in several attributes that are assigned to this object
getInfo <- function(x, tt){
  if (inherits(x, "complexCond")){
    out <- setNames(vector("list", 3L), c("componentsInfo", "consistency", "coverage"))
    cinf <- out$componentsInfo <- lapply(x, getInfo)
    names(cinf) <- paste0("atomicCond..", seq_along(cinf))
    conEnv <- lapply(cinf, "[[", "consistency")
    out$consistency <- eval(attr(x, "complStruct"), conEnv, 
                            enclos = logicalOperators)
    covEnv <- lapply(cinf, "[[", "coverage")
    out$coverage <- eval(attr(x, "complStruct"), covEnv, 
                         enclos = logicalOperators)
    return(out)
  }
  y <- x[[1]]
  f <- attr(x, "n")
  sy <- sum((f*y))
  sumf <- sum(f)
  if (inherits(x, "booleanCond")){
    list(info = c(sy = sy, sumf = sumf),
         freq = sy/sumf)
    }
  else if (inherits(x, "atomicCond")){
    .pmin <- function(x, y){x[x>y] <- y[x>y]; x}
    z <- x[[2]] 
    sz <- sum((f*z))
    szy <- sum(f*.pmin(z, y))
    list(info = c(sy = sy, sz = sz, szy = szy, sumf = sumf),
         consistency = szy/sy,
         coverage = szy/sz)
  }
}

################################################################################

# ==== versions of condition with fixed type ====
cscond <- function(...){
  cl <- match.call(condition, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(condition)
  cl$type <- "cs"
  eval.parent(cl)
}
mvcond <- function(...){
  cl <- match.call(condition, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(condition)
  cl$type <- "mv"
  eval.parent(cl)
}
fscond <- function(...){
  cl <- match.call(condition, sys.call())
  stopifnot(is.null(cl$type))
  cl[[1]] <- quote(condition)
  cl$type <- "fs"
  eval.parent(cl)
}
