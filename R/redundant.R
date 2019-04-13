
redundant <- function(cond, x = NULL, simplify = TRUE){
  cond <- noblanks(cond)
  .redund(x, cond, simplify)
}

# ==== .redund() ====
# Switch order of first 2 args to provide dispatching on x
# Generic function
.redund <- function(x, cond, ...) UseMethod(".redund")

# ==== Method for class 'tti' ====
# identifies the asf that are redundant within some csf
#   x         tti
#   cond      character vector with the csf
#   simplify  output matrix instead of list if all csf have the same number of asf
# value: A list of logical vectors (lengths corresponing to the number of asf), 
#        or a matrix if simplify=TRUE and all csf have the same number of asf
.redund.tti <- function(x, cond, simplify = TRUE, full = FALSE, names = TRUE,
                        qc_full = qcond_csf(cond, sc, flat = TRUE)){
  if (!full) x <- full.tt(x)
  sc <- x$scores
  n <- nrow(sc)
  ee <- matrix(qc_full[, 1L, ] == qc_full[, 2L, ], n)
  colnames(ee) <- attr(qc_full, "condition")
  lengths <- attr(qc_full, "csflengths")
  out <- C_mredund(ee, lengths)
  names(out) <- cond
  if (simplify && length(ul <- unique(lengths(out, use.names = FALSE))) == 1L){
    nms <- names(out)
    out <- matrix(unlist(out, use.names = FALSE), ncol = ul, byrow = TRUE)
    if (names) rownames(out) <- nms
  }
  out
}

# ==== Method for class 'truthTab' ====
# Function suited for interactive use
.redund.truthTab <- function(x, cond, simplify = TRUE, full = FALSE){
  tti <- tt.info(x)
  ok <- .qcondType(cond, colnames(tti$scores), tti$type) %in% c("stdAtomic", "stdComplex")
  if (any(!ok)) 
    stop("Invalid input to redundant:\n", paste0("  ", cond[!ok], collapse = "\n"),
         call. = FALSE)
  .redund.tti(tti, cond, simplify = simplify, full = full) 
}

# ==== Default Method (for matrix or data.frame) ====
# builds full.tt if x is NULL
#   x       truthTab or NULL
# value:    truthTab, mv if original is mv, cs else
.redund.default  <- function(x, cond, simplify = TRUE, ...){
  if (is.null(x)){
    x <- full.tt.default(cond)
  } else {
    x <- full.tt(x)
  }
  tti <- tt.info(x)
  ok <- .qcondType(cond, colnames(tti$scores), tti$type) %in% c("stdAtomic", "stdComplex")
  if (any(!ok)) 
    stop("Invalid input to redundant:\n", paste0("  ", cond[!ok], collapse = "\n"),
         call. = FALSE)
  .redund.tti(tti, cond, simplify = simplify, full = TRUE)
}
