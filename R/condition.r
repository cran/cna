
# ==== Generic function condition ====
condition <- function(x, ...)
  UseMethod("condition")

# ------------------------------------------------------------------------------

# ==== condition.default ====
# entirely new
# now able to handle the types "cs", "mv", "fs"
condition.default <- function(x, tt = full.tt(x), type, add.data = FALSE,
                              force.bool = FALSE, rm.parentheses = FALSE, ...){
  stopifnot(is.character(x.in <- x))
  x[] <- noblanks(x)   # remove blanks
  x[] <- gsub(",", "*", x, fixed = TRUE)
  if (inherits(tt, "truthTab")){
    type <- attr(tt, "type")
  } else {
    if (missing(type)) type <- "cs"    # "hidden" Default value!
    tt <- truthTab(tt, type = type, rm.dup.factors = FALSE, rm.const.factors = FALSE)
  }
  sc <- tt.info(tt)$scores
  vnms <- colnames(sc)
  n.cases <- attr(tt, "n", exact = TRUE)
  
  negateCase <- type %in% c("cs", "fs") # use lower case negation in conditions' syntax?
  
  # Determine the conditions' types
  inhStd <- as.logical(
    inherits(x, 
             c("stdBoolean", "stdAtomic", "stdComplex"),
             which = TRUE))
  if (any(inhStd)){
    condTypes <- qct <- rep(c("stdBoolean", "stdAtomic", "stdComplex")[inhStd],
                            length(x))
  } else {
    condTypes <- qct <- .qcondType(x, vnms, type)
    x <- names(qct)
    names(qct) <- names(condTypes) <- NULL
    if (any(noqct <- qct == "unknown")){
      px <- lapply(x[noqct], tryparse)
      px_ind <- which(noqct)
      syntax_ok <- !m_is.null(px)
      inPar_px <- logical(length(px))
      for (i in seq_along(px)){
        if (!syntax_ok[[i]]) next
        if (inPar_px[i] <- (as.character(px[[i]][[1]]) == "("))
          px[[i]] <- rm.parentheses(px[[i]])
        px[[i]] <- reshapeCall(px[[i]])
        x[[px_ind[i]]] <- parsed2visible(px[[i]])
      }
      inPar <- reshaped_px <- logical(length(x))
      inPar[noqct] <- inPar_px
      if (!all(syntax_ok))
        condTypes[noqct][!syntax_ok] <- "invalidSyntax"
      if (any(syntax_ok)){
        vars_ok <- checkVariableNames(px[syntax_ok], if (type == "mv") names(tt) else vnms)
        ct <- character(sum(vars_ok))
        ct_inPar <- !rm.parentheses & inPar_px[syntax_ok][vars_ok]
        if (any(ct_inPar)) ct[ct_inPar] <- "boolean"
        if (!all(ct_inPar))
          ct[!ct_inPar] <- .condType(px[syntax_ok][vars_ok][!ct_inPar], force.bool)
        condTypes[noqct][syntax_ok][vars_ok] <- ct
        if (!all(vars_ok))
          condTypes[noqct][syntax_ok][!vars_ok] <- "invalidValues"
      }
    }
  }
  reshaped <- !mapply(function(s1, s2) grepl(s1, s2, fixed = TRUE),
                      x, noblanks(x.in), 
                      SIMPLIFY = TRUE, USE.NAMES = FALSE)
  forced <- force.bool | (condTypes == "boolean" & grepl("->", x, fixed = TRUE))
  
  # treat conditions according to their condition-type
  out <- vector("list", length(x))
  info <- initializeInfo(condition = x, input = x.in, condTypes, 
                         reshaped = reshaped, force.bool = forced)
  if (any(stdBool <- (condTypes == "stdBoolean"))){
    qc <- qcond_bool(x[stdBool], sc)
    out[stdBool] <- qcond2cond_bool(qc)
    info <- updateInfo(info, stdBool, getInfo_bool(qc, n.cases))
  }
  if (any(stdAtomic <- (condTypes == "stdAtomic"))){
    qc <- qcond_asf(x[stdAtomic], sc, force.bool = force.bool)
    out[stdAtomic] <- qcond2cond_asf(qc)
    infoUpdate <- if (!force.bool){
      getInfo_asf(qc, n.cases)
    } else {
      getInfo_bool(qc, n.cases)
    }
    info <- updateInfo(info, stdAtomic, infoUpdate)
  }
  if (any(stdComplex <- (condTypes == "stdComplex"))){
    qc <- qcond_csf(x[stdComplex], sc, force.bool = force.bool,
                    freqs = n.cases)
    out[stdComplex] <- qcond2cond_csf(qc)
    infoUpdate <- if (!force.bool){
      getInfo_csf(qc)
    } else {
      getInfo_bool(qc, n.cases)
    }
    info <- updateInfo(info, stdComplex, infoUpdate)
  }
  if (any(bool <- (condTypes == "boolean"))){
    out[bool] <- booleanCond(px[match(which(bool), px_ind)], 
                             tt = tt, negateCase = negateCase)
    info <- updateInfo(info, bool, getInfo_bool(out[bool], n.cases))
  }
  if (any(atomic <- (condTypes == "atomic"))){
    out[atomic] <- atomicCond(px[match(which(atomic), px_ind)], 
                              tt = tt, negateCase = negateCase)
    info <- updateInfo(info, atomic,
      getInfo_asf(structure(out[atomic], response = rhs(x[atomic])), 
                  n.cases))
  }
  if (any(complx <- (condTypes == "complex"))){
    i_x <- which(complx) 
    i_px <- match(i_x, px_ind)
    for (i in seq_along(i_x)){
      out[[i_x[i]]] <- complexCond(px[[i_px[i]]], tt = tt, negateCase = negateCase)
    }
    info <- updateInfo(info, complx,
      getInfo_customCsf(out[i_x], lengths(out[i_x]), n.cases))
  }

  ok <- condTypes %in% c("stdBoolean", "stdAtomic", "stdComplex", "boolean", "atomic", "complex")
  out[!ok] <- rep(list(invalidCond()), sum(!ok))
  names(out) <- x
  if (any(forced)){
    names(out)[forced] <- info$condition[forced] <- paste0("(", x[forced], ")")
  }
  attr(out, "type") <- type
  attr(out, "n") <- n.cases
  attr(out, "cases") <- attr(tt, "cases")
  if (add.data) attr(out, "tt") <- tt
  rownames(info) <- NULL
  attr(out, "info") <- info
  class(out) <- "condList"
  out
}

# ------------------------------------------------------------------------------

# ==== ...Cond functions: handling the 3 types of conds (boolean, atomic, complex) ====
# take a quoted string x and a truthTable tt
# returns a cond object

# cond, "boolean" case
booleanCond <- function(px, tt, negateCase, addClass = TRUE){
  nms <- vapply(px, parsed2visible, character(1))
  if (negateCase) {
    px[] <- lapply(px, applyCaseNegation)
  }
  datatype <- switch(attr(tt, "type"),
                     cs = "integer",
                     mv = "integer",
                     fs = "numeric")
  out <- vector("list", length(px))
  for (i in seq_along(px)){
    # evaluate
    out_i <- eval(px[[i]], envir = tt, enclos = logicalOperators)
    mode(out_i) <- datatype
    out_i <- as.data.frame(out_i)
    names(out_i) <- nms[[i]]
    if (addClass) class(out_i) <- c("booleanCond", "cond", "data.frame")
    out[[i]] <- out_i
  }
  out
}


# cond, "atomic" case
atomicCond <- function(px, tt, negateCase){
  l <- length(px)
  bconds <- unlist(lapply(px, function(x){
      ind <- if (as.character(x[[1]]) != "=") 3:2 else 2:3
      as.list(x[ind])
      }), 
    recursive = FALSE)
  bconds <- booleanCond(bconds, tt, negateCase, addClass = FALSE)
  out <- C_relist_List(bconds, rep(2, l))
  for (i in seq_along(out)){
    out_i <- as.data.frame(out[[i]], optional = TRUE)
    class(out_i) <- c("atomicCond", "cond", "data.frame")
    out[[i]] <- out_i
  }
  out
}

# cond, "complex" case
complexCond <- function(pxi, tt, negateCase){
  strct <- .call2list(pxi)
  atConds <- rapply(strct, function(x) x, classes = c("call", "<-", "="), how = "unlist")
  cntr <- .counter()
  cntr$init()
  strct <- rapply(strct, function(x) as.name(paste0("atomicCond..", cntr$increment())), 
                  classes = c("call", "<-", "="), how = "replace")
  restoredStruct <- .list2call(strct)
  cc <- atomicCond(atConds, tt, negateCase = negateCase)
  names(cc) <- vapply(atConds, parsed2visible, character(1))
  attr(cc, "complStruct") <- restoredStruct
  class(cc) <- c("complexCond", "cond")
  cc
}

# invalidCond()
invalidCond <- function() 
  structure("Invalid condition", class = c("invalidCond", "cond", "character"))


# ==== Auxiliary functions used in booleanCond and atomicCond ====

# Function to translate lower case negation into R-syntax
# (see Wickham, "Advanced R", section 14.7.)
# takes and returns a language object
# Names are rendered either as in vnms (positive value; the variable names in the truthTab) or 
# as lowercase (negative value)
applyCaseNegation <- function(x, negChar = "-"){
  if (is.atomic(x)){
    x
  } else if (is.name(x)){
    xchar <- as.character(x)
    if (xchar == tolower(xchar)){
      call(negChar, as.name(switchCase(xchar)))
    } else {
      x
    }
  } else if (is.call(x)){
    x[-1] <- lapply(x[-1], applyCaseNegation, negChar = negChar)
    x
  } else {
    # User supplied incorrect input
    stop("applyCaseNegation is not applicable to type ", typeof(x))
  }
}
# applyCaseNegation(quote(A + b*c))
# applyCaseNegation(quote(A + b*c), negChar = "!")
# applyCaseNegation(quote(ad + BE*cf))
# applyCaseNegation(quote(-a))


# ==== Auxiliary functions used in complexCond ====

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
