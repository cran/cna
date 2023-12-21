
# ==== Generic function condition ====
condition <- function(x, ...)
  UseMethod("condition")

# ------------------------------------------------------------------------------

# ==== condition.default ====
# entirely new
# now able to handle the types "cs", "mv", "fs"
condition.default <- function(x, ct = full.ct(x), type, add.data = FALSE,
                              force.bool = FALSE, rm.parentheses = FALSE, ..., 
                              tt){
  
    # Ensure backward compatibility of argument tt
    if (!missing(tt)){
      warning("Argument tt is deprecated in condition(); use ct instead.", 
              call. = FALSE)
      if (missing(ct)) ct <- tt
    }
  if (inherits(ct, "configTable")){
    type <- attr(ct, "type")
  } else {
    if (missing(type)) type <- "auto"    # "hidden" Default value!
    ct <- configTable(ct, type = type, rm.dup.factors = FALSE, rm.const.factors = FALSE)
  }
  cti <- ctInfo(ct)
  condFromCti(x = x, cti = cti, ct = ct, type = cti$type, 
              add.data = add.data, force.bool = force.bool, rm.parentheses = rm.parentheses)
}

condFromCti <- function(x, cti, ct = cti2ct(cti), type = cti$type, 
                        add.data = FALSE, force.bool = FALSE, rm.parentheses = FALSE, 
                        x.in = x){
  stopifnot(is.character(x.in <- x))
  x[] <- noblanks(x)   # remove blanks
  x[] <- gsub(",", "*", x, fixed = TRUE)
  
  # Determine the conditions' types
  condTypes <- getCondType(x, cti, force.bool = force.bool, rm.parentheses = rm.parentheses)
  x <- attr(condTypes, "x")
  px <- attr(condTypes, "px")
  px_ind <- attr(condTypes, "px_ind")
  condTypes <- as.vector(condTypes)
  
  n.cases <- cti$freq
  reshaped <- !mapply(function(s1, s2) grepl(s1, s2, fixed = TRUE),
                      x, noblanks(x.in), 
                      SIMPLIFY = TRUE, USE.NAMES = FALSE)
  forced <- force.bool | (condTypes == "boolean" & grepl("->", x, fixed = TRUE))
  
  # treat conditions according to their condition-type
  out <- vector("list", length(x))
  info <- initializeInfo(condition = x, input = x.in, condTypes, 
                         reshaped = reshaped, force.bool = forced)
  if (any(stdBool <- (condTypes == "stdBoolean"))){
    qc <- qcond_bool(x[stdBool], cti$scores)
    out[stdBool] <- qcond2cond_bool(qc)
    info <- updateInfo(info, stdBool, getInfo_bool(qc, n.cases))
  }
  if (any(stdAtomic <- (condTypes == "stdAtomic"))){
    qc <- qcond_asf(x[stdAtomic], cti$scores, force.bool = force.bool)
    out[stdAtomic] <- qcond2cond_asf(qc)
    infoUpdate <- if (!force.bool){
      getInfo_asf(qc, n.cases)
    } else {
      getInfo_bool(qc, n.cases)
    }
    info <- updateInfo(info, stdAtomic, infoUpdate)
  }
  if (any(stdComplex <- (condTypes == "stdComplex"))){
    qc <- qcond_csf(x[stdComplex], cti$scores, force.bool = force.bool,
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
                             ct = ct)
    # check and case handling below (and in booleanCond(), too) is necessary mainly 
    # because the getCondType does not properly distinguish 
    # unary '-' (valid) from binary '-' (invalid), as in condition("U-L", d.educate)
    .newInvalid <- which(bool)[sapply(out[bool], inherits, "invalidCond")]
    if (any(.newInvalid)){
      bool[.newInvalid] <- FALSE
      info$condTypes[.newInvalid] <- "invalidSyntax"
    }
    info <- updateInfo(info, bool, getInfo_bool(out[bool], n.cases))
  }
  if (any(atomic <- (condTypes == "atomic"))){
    out[atomic] <- atomicCond(px[match(which(atomic), px_ind)], 
                              ct = ct)
    info <- updateInfo(info, atomic,
                       getInfo_asf(structure(out[atomic], response = rhs(x[atomic])), 
                                   n.cases))
  }
  if (any(complx <- (condTypes == "complex"))){
    i_x <- which(complx) 
    i_px <- match(i_x, px_ind)
    for (i in seq_along(i_x)){
      out[[i_x[i]]] <- complexCond(px[[i_px[i]]], ct = ct)
    }
    info <- updateInfo(info, complx,
                       getInfo_customCsf(out[i_x], lengths(out[i_x]), n.cases))
  }
  
  # temp: insert complexity for "std" cases
  {
    isStd <- condTypes %in% c("stdBoolean", "stdAtomic", "stdComplex")
    info$complexity[isStd] <- getComplexity(info$input[isStd])
  }  
  
  ok <- condTypes %in% c("stdBoolean", "stdAtomic", "stdComplex", "boolean", "atomic", "complex")
  out[!ok] <- rep(list(invalidCond()), sum(!ok))
  names(out) <- x
  if (any(forced)){
    names(out)[forced] <- info$condition[forced] <- paste0("(", x[forced], ")")
  }
  attr(out, "type") <- type
  attr(out, "n") <- n.cases
  attr(out, "cases") <- attr(ct, "cases")
  if (add.data) attr(out, "ct") <- ct
  rownames(info) <- NULL
  attr(out, "info") <- info
  class(out) <- "condList"
  out
}

# ------------------------------------------------------------------------------

# Aux fn: Generate a data.frame from a cti
cti2ct <- function(x, cases = TRUE){
  out <- x$valueId
  if (x$type == "fs"){
    out[] <- x$scores[, colnames(out)]
  } else {
    out[] <- unlist(x$uniqueValues, use.names = FALSE)[as.vector(out)]
  }
  out <- as.data.frame(out, warn = FALSE)
  class(out) <- c("configTable", "data.frame")
  attr(out, "type") <- x$type
  attr(out, "n") <- x$freq
  if (cases)
    attr(out, "cases") <- C_relist_Char(as.character(seq_len(sum(x$freq))), x$freq)
  out
}

# require(dplyr)
# d.educate %>% configTable %>% ctInfo %>% cti2ct %>% setequal(d.educate)
# d.pban %>% mvct %>% ctInfo %>% cti2ct %>% setequal(d.pban %>% mvct)
# d.jobsecurity %>% fsct %>% ctInfo %>% cti2ct %>% setequal(d.jobsecurity)

# ------------------------------------------------------------------------------

# ==== ...Cond functions: handling the 3 types of conds (boolean, atomic, complex) ====
# take a quoted string x and a configTable ct
# returns a cond object

# cond, "boolean" case
booleanCond <- function(px, ct, addClass = TRUE){
  nms <- vapply(px, parsed2visible, character(1))
  type <- attr(ct, "type")
  if (type %in% c("cs", "fs")) {
    px[] <- lapply(px, applyCaseNegation)
  }
  datatype <- switch(type, cs = "integer", mv = "integer", fs = "numeric")
  out <- vector("list", length(px))
  for (i in seq_along(px)){
    # evaluate
    out_i <- try(eval(px[[i]], envir = ct, enclos = logicalOperators), silent = TRUE)
    if (inherits(out_i, "try-error")){
      out_i <- invalidCond()
      attr(out_i, "condType") <- "invalidSyntax"
    } else {
      mode(out_i) <- datatype
      out_i <- as.data.frame(out_i)
      names(out_i) <- nms[[i]]
      if (addClass) class(out_i) <- c("booleanCond", "cond", "data.frame")
    }
    out[[i]] <- out_i
  }
  out
}


# cond, "atomic" case
atomicCond <- function(px, ct){
  l <- length(px)
  bconds <- unlist(lapply(px, function(x){
      ind <- if (as.character(x[[1]]) != "=") 3:2 else 2:3
      as.list(x[ind])
      }), 
    recursive = FALSE)
  bconds <- booleanCond(bconds, ct, addClass = FALSE)
  out <- C_relist_List(bconds, rep(2, l))
  for (i in seq_along(out)){
    out_i <- as.data.frame(out[[i]], optional = TRUE)
    class(out_i) <- c("atomicCond", "cond", "data.frame")
    out[[i]] <- out_i
  }
  out
}

# cond, "complex" case
complexCond <- function(pxi, ct){
  strct <- .call2list(pxi)
  atConds <- rapply(strct, function(x) x, classes = c("call", "<-", "="), how = "unlist")
  cntr <- .counter()
  cntr$init()
  strct <- rapply(strct, function(x) as.name(paste0("atomicCond..", cntr$increment())), 
                  classes = c("call", "<-", "="), how = "replace")
  restoredStruct <- .list2call(strct)
  cc <- atomicCond(atConds, ct)
  names(cc) <- vapply(atConds, parsed2visible, character(1))
  attr(cc, "complStruct") <- restoredStruct
  class(cc) <- c("complexCond", "cond")
  cc
}

# invalidCond()
invalidCond <- function() 
  structure("Invalid condition", 
            class = c("invalidCond", "cond", "character"))


# ==== Auxiliary functions used in booleanCond and atomicCond ====

# Function to translate lower case negation into R-syntax
# (see Wickham, "Advanced R", section 14.7.)
# takes and returns a language object
# Names are rendered either as in vnms (positive value; the variable names in the configTable) or 
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
# type is irrelevant with the default stopOps !
# Used in complexCond
.call2list <- function(x, type = "cs", stopOps = c("<-", "<<-", "="), 
                       validOps = c("&", "|", "(", "-", if (type == "mv") "==")){
  if (is.call(x)) {
    fn.name <- as.character(x[[1]])
    if (fn.name %in% stopOps){
      x
    } else {
      if (!is.null(validOps) && !(fn.name %in% validOps)){
        stop("Invalid operation ", sQuote(parsed2visible(x[[1]])))
      }
      x[-1] <- lapply(x[-1], .call2list, type = type, stopOps = stopOps, validOps = validOps)
      as.list(x)
    }
  } else {
    x
  }
}
# .call2list(quote(A&B | -C))

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
