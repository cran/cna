
# ==== checkValues() ===
# check if all factor values in a condition are %in% values
checkValues <- function(char, split, values, fixed = TRUE){
  stopifnot(length(split) >= 2)
  spl <- hstrsplit(char, split, fixed = fixed, relist = FALSE)
  coll <- collapse(attr(spl, "lengths"), seq(2L, length(split)))
  found <- hrelist(spl, coll,
                   f = function(x) match(x, table = values, nomatch = 0L) > 0L)
  out <- m_all(found)
  if (any(!out)){
    # empty strings in spl (e.g. if char = "A+*B")?
    anyEmpty <- m_any(C_relist_Log(spl=="", coll[[1]])[!out])
    if (any(anyEmpty)){
      syntax_ok <- rep(TRUE, length(char))
      syntax_ok[!out][anyEmpty] <- FALSE
      attr(out, "syntax_ok") <- syntax_ok
    }
    # try with 'unified' case
    checkUnifCase <- !out
    if (any(anyEmpty)) checkUnifCase[!syntax_ok] <- FALSE
    if (any(checkUnifCase)){
      spl0 <- spl <- hsubset(spl, checkUnifCase)
      spl[] <- unifyCase(spl)
      found <- hrelist(spl, collapse(attr(spl, "lengths"), seq(2L, length(split))),
                       f = function(x) match(x, table = values, nomatch = 0L) > 0L)
      allfound <- m_all(found)
      if (any(allfound)){
        # attribute "fixedCase" with corrected condition (unified case)
        dif <- spl != spl0
        fixedCase <- rep(NA_character_, length(char))
        dups <- duplicated(spl0[dif])
        fixedCase[checkUnifCase][allfound] <- 
          modifyStrings(spl0[dif][!dups], spl[dif][!dups], char[checkUnifCase][allfound])
        attr(out, "fixedCase") <- fixedCase
        out[checkUnifCase][allfound] <- TRUE
      }
    }
  }
  out
}

# ==== checkVariables() ===
# slower version of checkValues for parsed conditions
checkVariableNames <- function(px, values){
  vars <- lapply(px, all.vars)
  found <- happly(vars, function(x) match(x, table = values, nomatch = 0L) > 0L)
  m_all(found)
}

# ==== "maybe"-functions ====
# check the syntactic form of the condition
#   does not check the validity of  factor values
#   NA = invalidSyntax
maybeBoolean <- function(x, tt_type){
  invalidChars <- if (tt_type == "mv"){
    "[\\<\\-\\>!\\(\\)]"
  } else {
    "[\\<\\-\\>!\\(\\)=]"
  }
  out <- !grepl(invalidChars, x)
  if (tt_type == "mv"){
    mvValueString <- "^[[:alpha:]\\.][[:alnum:]\\._]*=[[:digit:]]+$"
    spl <- hstrsplit(x, c("+", "*"), relist = FALSE)
    string_ok <- grepl(mvValueString, spl) 
    string_ok <- hrelist(string_ok, collapse(attr(spl, "lengths"), 2))
    string_ok <- m_all(string_ok)
    if (any(!string_ok)){
       out[!string_ok] <- FALSE
    }
  }
  if (any(out))
    out[out][grepl("^[\\+\\*]", x[out])] <- NA
  if (any(out))
    out[out][grepl("[\\+\\*]$", x[out])] <- NA
  out
}
maybeAtomic <- function(x, tt_type){
  ok <- grepl("<*->", x)
  if (any(ok)){
    mbBool <- happly(strsplit(x[ok], "<*->"), maybeBoolean, tt_type = tt_type)
    chk <- m_all(mbBool)
    ok1 <- ok
    ok[ok][!chk] <- FALSE
    ok[ok1][chk & lengths(mbBool) != 2] <- NA  # NA = invalidSyntax
  }
  ok
}
maybeComplex <- function(x, tt_type, multiple.only = TRUE){
  ok <- grepl("^\\(", x) & grepl("\\)$", x) 
  if (multiple.only) ok <- ok & grepl(")*(", x, fixed = TRUE)
  if (any(ok)){
    csfStruct <- m_all(happly(strsplit(sub("^\\((.+)\\)$", "\\1", x[ok]), ")*(", fixed = TRUE), 
                               maybeAtomic, tt_type = tt_type))
    ok[ok][!csfStruct] <- FALSE
  }
  ok
}


# ==== .qcondType() ====
# Determine the 'qtypes' of conditions
# possible values:
# "constant" , "stdBoolean", "stdAtomic", "stdComplex", "unknown", "invalidValues"
.qcondType <- function(x, values, tt_type, stdComplex.multiple.only = TRUE){
  out <- rep("unknown", length(x))
  names(out) <- x
  
  # empty string
  out[!nzchar(x)] <- "invalidSyntax"
  
  # "constant"
  out[x %in% c("0", "1")] <- "constant"
  
  # "boolean"
  sel1 <- out == "unknown"
  if (!any(sel1)) return(out)
  mbBool <- maybeBoolean(x[sel1], tt_type)
  sel2 <- mbBool & !is.na(mbBool)
  out[is.na(mbBool)] <- "invalidSyntax"
  if (any(sel2)){
    sel3 <- checkValues(x[sel1][sel2], c("+", "*"), values)
    out[sel1][sel2][sel3] <- "stdBoolean"
    out[sel1][sel2][!sel3] <- "invalidValues"
    if (!is.null(invSyn <- attr(sel3, "syntax_ok"))){
      out[sel1][sel2][!invSyn] <- "invalidSyntax"
    }
    if (!is.null(fc <- attr(sel3, "fixedCase"))){
      x[sel1][sel2][!is.na(fc)] <- fc[!is.na(fc)]
      names(out) <- x
    }
  }
  
  # "atomic"
  sel1 <- out == "unknown"
  if (!any(sel1)) return(out)
  mbAt <- maybeAtomic(x[sel1], tt_type)
  sel2 <- mbAt & !is.na(mbAt)
  out[is.na(mbAt)] <- "invalidSyntax"
  if (any(sel2)){
    sel3 <- checkValues(x[sel1][sel2], c("<*->", "+", "*"), values, 
                        fixed = c(FALSE, TRUE))
    out[sel1][sel2][sel3] <- "stdAtomic"
    out[sel1][sel2][!sel3] <- "invalidValues"
    if (!is.null(invSyn <- attr(sel3, "syntax_ok"))){
      out[sel1][sel2][!invSyn] <- "invalidSyntax"
    }
    if (!is.null(fc <- attr(sel3, "fixedCase"))){
      x[sel1][sel2][!is.na(fc)] <- fc[!is.na(fc)]
      names(out) <- x
    }
  }
  
  # "complex"
  sel1 <- out == "unknown"
  if (!any(sel1)) return(out)
  sel2 <- maybeComplex(x[sel1], tt_type, multiple.only = stdComplex.multiple.only)
  if (any(sel2)){
    sel3 <-  checkValues(gsub("^\\(|\\)$", "", x[sel1][sel2]), c(")*(", "<*->", "+", "*"), 
                         values, fixed = c(TRUE, FALSE, TRUE))
    out[sel1][sel2][sel3] <- "stdComplex"
    out[sel1][sel2][!sel3] <- "invalidValues"
    if (!is.null(invSyn <- attr(sel3, "syntax_ok"))){
      out[sel1][sel2][!invSyn] <- "invalidSyntax"
    }
    if (!is.null(fc <- attr(sel3, "fixedCase"))){
      x[sel1][sel2][!is.na(fc)] <- paste0("(", fc[!is.na(fc)], ")")
      names(out) <- x
    }
  }

  out
}


qcond2cond_bool <- function(x){
  qc <- setNames(split.default(as.data.frame(x), seq_len(ncol(x))),
                 colnames(x))
  lapply(qc, "class<-", c("stdBooleanCond", "booleanCond", "cond", "data.frame"))
}


qcond2cond_asf <- function(x){
  if (length(dim(x)) == 2) return(qcond2cond_bool(x))
  cond_nms <- attr(x, "condition")
  n <- nrow(x)
  l <- length(cond_nms)
  xx <- `dim<-`(x, c(n, 2*l))
  colnames(xx) <- hstrsplit(cond_nms, "<*->", fixed = FALSE, 
                            relist = FALSE, lengths.attr = FALSE)
  qc <- setNames(split.default(as.data.frame(xx), rep(seq_len(l), each = 2)),
                 cond_nms)
  qc <- lapply(qc, "class<-", c("stdAtomicCond", "atomicCond", "cond", "data.frame"))
  attr(qc, "response") <- attr(x, "response")
  qc
}

qcond2cond_csf <- function(x){
  if (!is.list(x)) return(qcond2cond_bool(x)) # case force.bool = TRUE
  n <- nrow(x[[1]])
  ll <- lengths(x)/n
  xx <- `dim<-`(unlist(x, recursive = FALSE, use.names = FALSE),
                c(n, sum(ll)))
  asfNames <- unlist(extract_asf(names(x)), FALSE, FALSE)
  colnames(xx) <- hstrsplit(asfNames, "<*->", 
                  fixed = FALSE, relist = FALSE, lengths.attr = FALSE)
  xx <- setNames(split.default(as.data.frame(xx), rep(rep(seq_len(sum(ll)/2)), each=2)),
                 asfNames)
  xx <- lapply(xx, "class<-", c("stdAtomicCond", "atomicCond", "cond", "data.frame"))
  qc <- setNames(split.default(xx, rep(seq_along(ll), ll/2)),
                 names(x))
  lapply(qc, "class<-", c("stdComplexCond", "complexCond", "cond"))
}


