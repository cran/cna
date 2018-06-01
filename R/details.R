
# ==== details ====
details <- function(cond, x, 
  what = c("inus", "exhaustiveness", "faithfulness", "coherence", "redundant")){
  .det(x, noblanks(cond), what = what)
}

# ==== .det() ====
# Switch order of first 2 args to provide dispatching on x
# Generic function
.det <- function(x, cond, ...) UseMethod(".det")

# ==== Default method for class 'tti' ====
#  Converts x to a truthTab and then calls the .det.tti
.det.default <- function(x, cond, 
    what = c("inus", "exhaustiveness", "faithfulness", "coherence", "redundant"),
    type){
  if (!inherits(x, "truthTab")){
    if (missing(type)) type <- "cs"
    x <- truthTab(x, type = type)
  }
  .det.tti(tt.info(x), cond = cond, what = what, available = x$details)
}

# ==== Method for class 'tti' ====
#   x        tti
#   cond     character vector with the csf
#   what     char-vector: list of required measures
# value: A data.frame with columns according to what
.det.tti <- function(x, cond, 
    what = c("inus", "exhaustiveness", "faithfulness", "coherence", "redundant"),
    available = what
){
  what <- clarify_details(what, available = available)
  qc_tt <- qcond_csf(cond, x$scores, flat = TRUE)
  x_full <- full.tt(x)
  qc_full <- qcond_csf(cond, x_full$scores, flat = TRUE)
  out <- data.frame(matrix(numeric(0), length(cond)), row.names = cond)
  if ("inus" %in% what){
    out$inus <- m_all(
      C_relist_Log(.inus.tti(x_full, lhs(attr(qc_tt, "condition")), 
                             full = TRUE),
                   attr(qc_full, "csflengths")))
  }
  ex_ff <- c("exhaustiveness", "faithfulness") %in% what
  if (any(ex_ff)){
    exf <- exff.tti(x, cond, names = FALSE, tti.full = x_full,
                    qc_full = qc_full)[, ex_ff, drop = FALSE]
    out <- cbind(out, exf)
  }
  if ("coherence" %in% what)
    out$coherence <- .coher(cond, std = TRUE, tti = x, qc_tt = qc_tt, names = FALSE)
  if (any(c("inus", "redundant") %in% what)){
    red <- m_any(
      .redund.tti(x_full, cond, simplify = FALSE, full = TRUE, 
                  qc_full = qc_full, names = FALSE))
    if ("inus" %in% what) out$inus <- out$inus & !red
    if ("redundant" %in% what) out$redundant <- red
  }
  out
}


# Aux function clarify_details()
#   what       required measures
#   measures   measures that are meaningful in the the present context
#              (e.g. "coherence" and "redundant" are not for msc's and asf's
#   available  available measures (when read from a cna-object)
#   warn       Logical: warn if some required measure is not available
clarify_details <- function(what,
    measures = c("inus", "exhaustiveness", "faithfulness", "coherence", "redundant"),
    available = NULL, warn = TRUE){
  if (is.logical(what) && length(what) == 1){
    if (!is.null(available)) measures <- intersect(measures, available)
    return(if (what) measures else character(0))
  }
  what <- measures[pmatch(what, measures, nomatch = 0)]
  if (warn && !is.null(available) && 
      length(not_av <- setdiff(what, available)) > 0)
    warning("Measure(s) not available: ", paste0(not_av, collapse = ", "),
            ".\nYou may have to specify details in cna().",
            call. = FALSE)
  if (!is.null(available)) what <- intersect(what, available)
  what
}
