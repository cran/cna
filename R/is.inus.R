
is.inus <- function(cond, x = NULL){
  .inus(x, noblanks(cond))
}

# ==== .inus() ====
# Switch order of first 2 args to provide dispatching on x
# identifies the asf that are INUS within some 1-sided formulas
# Generic function
.inus <- function(x, cond, ...) UseMethod(".inus")


  # aux fun
  constantCols <- function(x){
    matrixStats::colAlls(x == rep(x[1, ], each = nrow(x)))
  }

# ==== Method for class 'tti' ====
#   x         tti
#   cond      character vector with the csf
# value: A lgical vector with same length as cond
# See examples below
.inus.tti <- function(x, cond, full = FALSE){
  if (!full) x <- full.tt(x)
  sc <- x$scores
  out <- rep(TRUE, length(cond))
  names(out) <- cond
  
  # constant 0 or 1?
  zeroOne <- cond %in% c("0", "1")
  out[zeroOne] <- FALSE
  if (!any(out)) return(out)
  
  # constant?
  evalCond0 <- qcond_bool(cond[out], sc)[, , drop = FALSE]
  out[constantCols(evalCond0)] <- FALSE
  if (!any(out)) return(out)
 
  notConst <- out
  w <- which(notConst)
  disjs <- strsplit(cond[notConst], "+", fixed = TRUE)
  evalDisj <- qcond_bool(unlist(disjs), sc)
  indList <- C_relist_Int(seq_len(ncol(evalDisj)), lengths(disjs))
  conjs <- C_relist_List(strsplit(unlist(disjs), "*", fixed = TRUE), lengths(disjs))
  
  for (i in seq_along(cond[notConst])){
    evalDisj_i <- evalDisj[, indList[[i]], drop = FALSE]
    conjs_i <- conjs[[i]]

    # Is any conjunction redundant?  -> If yes: condition is not INUS
    if (length(conjs_i) > 1L && any(C_redund(!evalDisj_i))){
      out[w][i] <- FALSE
      next
    }

    # Is a factor in a conjunction redundant? -> If yes: condition is not INUS
    evalCond0_i <- evalCond0[, i, drop = TRUE]
    if (any(ll <- (lengths(conjs_i) > 1L))){
      brk <- FALSE
      for (j in seq_along(conjs_i)){
        if (!ll[[j]]) next
        if (any(C_redund(sc[evalCond0_i == 0, conjs_i[[j]], drop = FALSE]))){
          out[w][i] <- FALSE
          brk <- TRUE
        }
        if (brk) break
      }
    }
  }
  
  out
}

# ==== Method for class 'truthTab' ====
#   x         truthTab
#   cond      character vector with the csf
# value: A lgical vector with same length as cond
# See examples below
.inus.truthTab <- function(x, cond){
  tti <- tt.info(x)
  .inus.tti(tti, cond, full = FALSE) 
}

# ==== Default Method (for matrix or data.frame) ====
# builds 
#   x       truthTab
# value:    truthTab, mv if original is mv, cs else
.inus.default  <- function(x, cond){
  if (is.null(x)){
    x <- full.tt(cond)
  } else {
    x <- full.tt(x)
  }
  .inus.tti(tt.info(x), cond, full = TRUE)
}



